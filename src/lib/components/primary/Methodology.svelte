

<div class="methodology">
	<slot name="title">
		<h2 class="title">Methodology</h2>
	</slot>
	<slot name="content">
		<p>Default content</p>
	</slot>
	
</div>

<style>
	.methodology {
		display: flex;
		flex-direction: column;
		justify-content: center;
		gap: 1rem;
	}

	:global(.methodology span) {
		padding: 0;
	}
	
	.title {
		font-size:  0.8rem;
		font-weight: 700;
		margin: 0;
		text-transform: uppercase;
		opacity: 0.5;
		padding: 0;
		font-family: 'Roboto', sans-serif;
	}

	:global(.methodology p) {
		font-size: 0.75rem;
		padding: 0;
	}
</style>