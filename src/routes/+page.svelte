<script>
	import SEO from '$components/primary/SEO.svelte';
	import Logo from '$assets/dc_logo_white.png';

	import Crown from '$ai2html/crown.svelte';
	export let data = {};
</script>

<SEO {...data.metadata} />

<div class="my-6">
	<figure class="h-full max-w-[15rem] -mt-16 w-full">
		<svelte:component this={Crown}/>
		
	  </figure>
	  
	<h1 class="mx-auto text-center md:w-2/3">The New Residents in Hurricane Milton's Path</h1>

	<p class="text-xs text-center text-white/80">By <a href="https://twitter.com/Vonterinon">Vivek Matthew</a>, <a href="https://twitter.com/thedivtagguy">Aman Bhargava</a></p>
	<p class="text-xs text-center text-white/60">Published on October 10, 2024</p>
</div>
<section>
	<svelte:component this={data.content} />
</section>
<footer>
	<p class="text-xs text-mono">An exploration for</p>
	<a href="https://diagramchasing.fun">
		<picture>
			<img src={Logo} alt="Diagram Chasing" />
		</picture>
	</a>

	<p class="text-[0.6rem] text-white/60">
		Data analysis and text by <a href="https://twitter.com/Vonterinon">Vivek Matthew</a> <br />

		Graphics by <a href="https://twitter.com/thedivtagguy">Aman Bhargava</a>
	</p>
</footer>

<style>
	section {
		display: flex;
		flex-direction: column;
		justify-content: center;
		gap: 1rem;
	}

	footer {
		display: flex;
		flex-direction: column;
		justify-content: start;
		align-items: start;
		margin: 2rem auto;
	}

	footer img {
		width: 100%;
		max-width: 12rem;
	}
</style>
