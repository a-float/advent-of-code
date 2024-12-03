import { Html } from "@elysiajs/html";

export const ThemeToggle = () => (
  <label class="theme-toggle" title="Toggle theme">
    <input id="theme-toggle" type="checkbox" />
    <span class="theme-toggle-sr">Toggle theme</span>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      aria-hidden="true"
      width="1em"
      height="1em"
      fill="currentColor"
      class="theme-toggle__expand"
      viewBox="0 0 32 32"
    >
      <clipPath id="theme-toggle__expand__cutout">
        <path d="M0-11h25a1 1 0 0017 13v30H0Z" />
      </clipPath>
      <g clip-path="url(#theme-toggle__expand__cutout)">
        <circle cx="16" cy="16" r="8.4" />
        <path d="M18.3 3.2c0 1.3-1 2.3-2.3 2.3s-2.3-1-2.3-2.3S14.7.9 16 .9s2.3 1 2.3 2.3zm-4.6 25.6c0-1.3 1-2.3 2.3-2.3s2.3 1 2.3 2.3-1 2.3-2.3 2.3-2.3-1-2.3-2.3zm15.1-10.5c-1.3 0-2.3-1-2.3-2.3s1-2.3 2.3-2.3 2.3 1 2.3 2.3-1 2.3-2.3 2.3zM3.2 13.7c1.3 0 2.3 1 2.3 2.3s-1 2.3-2.3 2.3S.9 17.3.9 16s1-2.3 2.3-2.3zm5.8-7C9 7.9 7.9 9 6.7 9S4.4 8 4.4 6.7s1-2.3 2.3-2.3S9 5.4 9 6.7zm16.3 21c-1.3 0-2.3-1-2.3-2.3s1-2.3 2.3-2.3 2.3 1 2.3 2.3-1 2.3-2.3 2.3zm2.4-21c0 1.3-1 2.3-2.3 2.3S23 7.9 23 6.7s1-2.3 2.3-2.3 2.4 1 2.4 2.3zM6.7 23C8 23 9 24 9 25.3s-1 2.3-2.3 2.3-2.3-1-2.3-2.3 1-2.3 2.3-2.3z" />
      </g>
    </svg>
  </label>
);
