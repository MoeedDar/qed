import styles from "@/styles.css?url";

export const head = () => ({
	meta: [
		{
			charSet: "utf-8",
		},
		{
			name: "viewport",
			content: "width=device-width, initial-scale=1",
		},
		{
			title: "qed",
		},
	],
	links: [
		{
			rel: "preconnect",
			href: "https://fonts.googleapis.com",
		},
		{
			rel: "preconnect",
			href: "https://fonts.gstatic.com",
		},
		{
			rel: "stylesheet",
			href: "https://fonts.googleapis.com/css2?family=Manrope:ital,wght@0,300..800;1,300..800&family=Victor+Mono:ital,wght@0,100..700;1,100..700&display=swap",
		},
		{
			rel: "stylesheet",
			href: styles,
		},
	],
	scripts: [
		{
			src: "/lsp_js.js",
			defer: true,
		},
	],
});
