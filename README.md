# 2024/Milton

All our code and maps for our exploration of the recent housing and population changes within Hurricane Milton's path. 

## Data Analysis

The data analysis is done in R. The code is in the `scripts` folder.

Population, housing and demographic data are sourced from the American Community Survey 5-Year Data, 2010 and 2022, by the US Census Bureau. Census tract data is the fundamental geographic unit used, from the ACS 5-Year tables B01003, B01002, B25001, B25034, B25038. Proccessed in R using tidycensus. Population growth between 2010 and 2022 calculated using population-weighted areal interpolation.

The maps are created with QGIS and the files are in the `data` folder.

For creating the hex maps, generated centroids for each census tract and binned into a hexagonal grid for aggregation. Percentage of housing units built since 2010 calculated against total number of housing units in the census tract.

## Issues

If you notice anything wrong with the data or the analysis, please open an issue and we'll attempt to correct it as soon as possible.

## License

This project is licensed under the CC-BY-NC-SA License.
