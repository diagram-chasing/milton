

<div class="references">
	<slot name="title">
		<h2 class="title">References & Further reading</h2>
	</slot>
	<slot name="content">
		<p>Default content</p>
	</slot>
	
</div>

<style>
	.references {
		display: flex;
		flex-direction: column;
		justify-content: center;
		gap: 1rem;
	}

	:global(.references span) {
		padding: 0;
	}
	
	.title {
		font-size: 0.8rem;
		font-weight: 700;
		margin: 0;
		text-transform: uppercase;
		opacity: 0.5;
		padding: 0;
		font-family: 'Roboto', sans-serif;
	}

	:global(.references p, li) {
		font-size: 0.65rem !important;
		padding: 0;
        margin: 0;
	}
</style>