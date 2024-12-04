import { Html } from "@elysiajs/html";
import { ThemeToggle } from "./ThemeToggle";
import { hljsThemes } from "./shared";

type LayoutProps = {
  children?: any;
};

export const Layout = (props: LayoutProps) => (
  <html lang="en">
    <head>
      <meta charset="UTF-8" />
      <meta name="viewport" content="width=device-width, initial-scale=1.0" />
      <title>Advent of Code</title>
      <link rel="icon" type="image/x-icon" href="/public/favicon.ico" />
      <link
        rel="stylesheet"
        href="https://cdn.jsdelivr.net/npm/bulma@1.0.2/css/bulma.min.css"
      />
      <link
        href="https://cdn.jsdelivr.net/npm/theme-toggles@4.10.1/css/expand.min.css"
        rel="stylesheet"
      />
      <link id="hljs-theme-link" rel="stylesheet" href="#" />
      <link rel="stylesheet" href="/public/styles.css" />
      <script type="text/javascript">
        {`
        const theme = localStorage.getItem("theme") || "dark";
        if(theme) document.documentElement.dataset.theme = theme;
        window.hljsThemes = ${JSON.stringify(hljsThemes)};
        document.getElementById("hljs-theme-link").href = window.hljsThemes[theme];
        `}
      </script>
      <script>let FF_FOUC_FIX;</script>
    </head>
    <body>
      <ThemeToggle />
      {props.children}
      <footer>Made by Mati</footer>
      <script src="/public/setupListeners.js" />
    </body>
  </html>
);
