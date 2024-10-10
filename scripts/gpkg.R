# Load required libraries
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)

# Define variables to fetch
vars <- c(
  "B01003_001",
  # Total population
  "B01002_001",
  # Median age
  "B02001_002",
  # White alone
  "B02001_003",
  # Black or African American alone
  "B02001_004",
  # American Indian and Alaska Native alone
  "B02001_005",
  # Asian alone
  "B02001_006",
  # Native Hawaiian and Other Pacific Islander alone
  "B02001_007",
  # Some other race alone
  "B02001_008",
  # Two or more races
  "B03003_003",
  # Hispanic or Latino
  "B19013_001",
  # Median household income in the past 12 months
  "B25001_001",
  # Housing units
  "B25034_002",
  # Housing units built (2020 to 2022)
  "B25034_003",
  # Housing units built (2010 to 2019)
  "B25038_002",
  # Housing units owner occupied (total)
  "B25038_003",
  # Housing units owner occupied (2021 to 2022)
  "B25038_004",
  # Housing units owner occupied (2018 to 2020)
  "B25038_005",
  # Housing units owner occupied (2010 to 2017)
  "B25038_009",
  # Housing units renter occupied (total)
  "B25038_010",
  # Housing units renter occupied (2021 to 2022)
  "B25038_011",
  # Housing units renter occupied (2018 to 2020)
  "B25038_012"
  # Housing units renter occupied (2010 to 2017)
)

# Get Florida census tract demographic data for 2022
fl_demographics2022 <- get_acs(
  geography = "tract",
  variables = vars,
  state = "FL",
  year = 2022,
  geometry = TRUE,
  output = "wide",
  cache = TRUE
)

# Rename and calculate columns
fl_demographics2022 <- fl_demographics2022 %>%
  rename(
    total_population = B01003_001E,
    median_age = B01002_001E,
    median_income = B19013_001E,
    housing_units = B25001_001E,
    housing_units_2020_2022 = B25034_002E,
    housing_units_2010_2020 = B25034_003E,
    housing_units_owner_total = B25038_002E,
    housing_units_owner_2021_2022 = B25038_003E,
    housing_units_owner_2018_2020 = B25038_004E,
    housing_units_owner_2010_2017 = B25038_005E,
    housing_units_renter_total = B25038_009E,
    housing_units_renter_2021_2022 = B25038_010E,
    housing_units_renter_2018_2020 = B25038_011E,
    housing_units_renter_2010_2017 = B25038_012E,
    demo_white = B02001_002E,
    demo_black = B02001_003E,
    demo_native = B02001_004E,
    demo_asian = B02001_005E,
    demo_pacific = B02001_006E,
    demo_other_race = B02001_007E,
    demo_two_or_more = B02001_008E,
    demo_hispanic = B03003_003E,
  )

# Get Florida census tract demographic data for 2010
fl_demographics2010 <- get_acs(
  geography = "tract",
  variables = vars,
  state = "FL",
  year = 2010,
  geometry = TRUE,
  output = "wide",
  cache = TRUE
)

# Rename and calculate columns
fl_demographics2010 <- fl_demographics2010 %>%
  rename(
    total_population = B01003_001E,
    median_age = B01002_001E,
    median_income = B19013_001E,
    housing_units = B25001_001E,
    demo_white = B02001_002E,
    demo_black = B02001_003E,
    demo_native = B02001_004E,
    demo_asian = B02001_005E,
    demo_pacific = B02001_006E,
    demo_other_race = B02001_007E,
    demo_two_or_more = B02001_008E,
    demo_hispanic = B03003_003E,
  )

# Population-weighted areal interpolation for calculating tract level population change: https://walker-data.com/census-r/spatial-analysis-with-us-census-data.html#population-weighted-areal-interpolation
fl_interpolate_pw <- interpolate_pw(
  fl_demographics2010,
  fl_demographics2022,
  to_id = "GEOID",
  extensive = TRUE,
  weights = fl_demographics2022,
  weight_column = "total_population",
  crs = 26949
)

# Join data based on source year
fl_op_growth_join <- fl_demographics2022 %>%
  left_join(
    st_drop_geometry(fl_interpolate_pw),
    by = "GEOID",
    suffix = c("_latest", "_2010")
  )

# Rename and calculate required columns
fl_op_growth <- fl_op_growth_join %>%
  mutate(
    population_change = total_population_latest - total_population_2010,
    housing_units_total = housing_units_latest,
    housing_units_built_after_2010 = housing_units_2020_2022 + housing_units_2010_2020,
    housing_units_built_before_2010 = housing_units_latest - (housing_units_2020_2022 + housing_units_2010_2020),
    housing_units_built_2020_2022 = housing_units_2020_2022,
    housing_units_built_2010_2020 = housing_units_2010_2020,
    housing_units_owner_occupied = housing_units_owner_total,
    housing_units_owner_occupied_after_2010 = housing_units_owner_2021_2022 + housing_units_owner_2018_2020 + housing_units_owner_2010_2017,
    housing_units_owner_occupied_before_2010 = housing_units_owner_total - (
      housing_units_owner_2021_2022 + housing_units_owner_2018_2020 + housing_units_owner_2010_2017
    ),
    housing_units_renter_occupied = housing_units_renter_total,
    housing_units_renter_occuped_after_2010 = housing_units_renter_2021_2022 + housing_units_renter_2018_2020 + housing_units_renter_2010_2017,
    housing_units_renter_occupied_before_2010 = housing_units_renter_total - (
      housing_units_renter_2021_2022 + housing_units_renter_2018_2020 + housing_units_renter_2010_2017
    ),
    median_age_2022 = median_age_latest,
    median_income_2022 = median_income_latest,
    demo_white_2022 = demo_white_latest,
    demo_black_2022 = demo_black_latest,
    demo_native_2022 = demo_native_latest,
    demo_asian_2022 = demo_asian_latest,
    demo_pacific_2022 = demo_pacific_latest,
    demo_other_race_2022 = demo_other_race_latest,
    demo_two_or_more_2022 = demo_two_or_more_latest,
    demo_hispanic_2022 = demo_hispanic_latest,
  ) %>%
  select(
    GEOID,
    NAME,
    population_change,
    housing_units_total,
    housing_units_built_after_2010,
    housing_units_built_before_2010,
    housing_units_built_2020_2022,
    housing_units_built_2010_2020,
    housing_units_owner_occupied,
    housing_units_owner_occupied_after_2010,
    housing_units_owner_occupied_before_2010,
    housing_units_renter_occupied,
    housing_units_renter_occuped_after_2010,
    housing_units_renter_occupied_before_2010,
    median_age_2022,
    median_income_2022,
    demo_white_2022,
    demo_black_2022,
    demo_native_2022,
    demo_asian_2022,
    demo_pacific_2022,
    demo_other_race_2022,
    demo_two_or_more_2022,
    demo_hispanic_2022,
    geometry
  )

# Get Florida Census places
fl_places <- get_acs(
  geography = "place",
  variables = "B01003_001",
  # We only need this to get the geometries
  state = "FL",
  year = 2022,
  geometry = TRUE,
  cache = TRUE
) %>%
  select(GEOID, NAME, geometry) %>%
  rename(place_geoid = GEOID, place_name = NAME)

# Perform spatial join
fl_op_growth_with_places <- st_join(fl_op_growth, fl_places, join = st_intersects, left = TRUE)

# Write the data to a GeoPackage file
st_write(
  fl_op_growth_with_places,
  "../data/florida.gpkg",
  driver = "GPKG",
  overwrite = TRUE,
  append = FALSE
)

data <- fl_op_growth_with_places

# 1. Find places with both high median_age_2022 and housing_units_built_after_2010
high_age_new_housing <- data %>%
  mutate(
    age_zscore = scale(median_age_2022),
    new_housing_zscore = scale(housing_units_built_after_2010),
    composite_score = age_zscore + new_housing_zscore
  ) %>%
  arrange(desc(composite_score)) %>%
  select(NAME, place_name, median_age_2022, housing_units_built_after_2010, median_income_2022, demo_white_2022, composite_score) %>%
  head(100)

print("Places with high combination of median age and new housing units:")
print(high_age_new_housing)

# 2. Find places with both high median_age_2022 and housing_units_owner_occupied_after_2010
high_age_owner_occupied <- data %>%
  mutate(
    age_zscore = scale(median_age_2022),
    new_housing_zscore = scale(housing_units_owner_occupied_after_2010),
    composite_score = age_zscore + new_housing_zscore
  ) %>%
  arrange(desc(composite_score)) %>%
  select(NAME, place_name, median_age_2022, housing_units_owner_occupied_after_2010, median_income_2022, demo_white_2022, composite_score) %>%
  head(100)

print("Places with high median age and many owner-occupied units built after 2010:")
print(high_age_owner_occupied)

# 3. Find places with the highest housing_units_built_after_2010
highest_new_housing <- data %>%
  arrange(desc(housing_units_built_after_2010)) %>%
  select(NAME, place_name, housing_units_built_after_2010, median_income_2022, demo_white_2022) %>%
  head(50)

print("Places with the highest number of housing units built after 2010:")
print(highest_new_housing)

# 5. Find the number of unique places where housing_units_built_after_2010 > 1000 and median_age_2022 > 50
high_new_housing_and_age <- data %>%
  filter(housing_units_built_after_2010 > 1000, median_age_2022 > 50) %>%
  distinct(NAME) %>%
  nrow()

print(paste0("Number of unique places with over 1000 new housing units and median age over 50: ", high_new_housing_and_age))

# Optional: List of these places
list_high_new_housing_and_age <- data %>%
  filter(housing_units_built_after_2010 > 1000, median_age_2022 > 50) %>%
  distinct(place_name, .keep_all = TRUE) %>%
  select(NAME, place_name, housing_units_built_after_2010, median_age_2022)

print("List of places meeting the criteria:")
print(list_high_new_housing_and_age)

# 6. Create a scatter plot of median_age vs housing_units_built_after_2010
scatter_plot_data <- data %>%
  filter(housing_units_built_after_2010 > 1000, median_age_2022 > 30)

scatter_plot <- ggplot(scatter_plot_data, aes(x = median_age_2022, y = housing_units_built_after_2010)) +
  geom_point(alpha = 0.5) +
  labs(title = "Title",
       x = "Median Age",
       y = "Housing Units built since 2010",
       caption = "Data source: ACS 5-Year Summaries, 2022") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Save the plot
ggsave("median_age_vs_new_housing.png", scatter_plot, width = 10, height = 6, dpi = 300)