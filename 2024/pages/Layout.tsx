import { Html } from "@elysiajs/html";
import { ThemeToggle } from "./ThemeToggle";

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
      ></link>
      <link rel="stylesheet" href="/public/styles.css" />
      <script>let FF_FOUC_FIX;</script>
    </head>
    <body>
      <ThemeToggle />
      {props.children}
      <script src="/public/setupListeners.js" />
    </body>
  </html>
);
