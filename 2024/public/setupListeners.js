document.addEventListener("DOMContentLoaded", function () {
  const inputArea = document.getElementById("input");
  const resultPart1 = document.getElementById("result-part-1");
  const timePart1 = document.getElementById("time-part-1");
  const btnPart1 = document.getElementById("run-part-1");
  const resultPart2 = document.getElementById("result-part-2");
  const timePart2 = document.getElementById("time-part-2");
  const btnPart2 = document.getElementById("run-part-2");
  const hljsThemeLink = document.getElementById("hljs-theme-link");

  const day = inputArea?.dataset?.day;
  const dataStorageKey = day ? `data-day-${day}` : null;
  const initialData = day ? localStorage.getItem(dataStorageKey) : "";
  if (inputArea) inputArea.value = initialData;

  inputArea?.addEventListener("input", (e) => {
    const value = e.target.value;
    if (!day) return;
    localStorage.setItem(dataStorageKey, value);
  });

  btnPart1?.addEventListener("click", async () => {
    if (!("part1" in window && typeof window.part1 === "function")) return;
    const input = inputArea.value;
    const start = performance.now();
    btnPart1.classList.add("is-loading");
    const result = await window.part1(input);
    btnPart1.classList.remove("is-loading");
    const end = performance.now();
    resultPart1.textContent = result;
    timePart1.textContent = (end - start).toFixed(2) + "ms";
  });

  btnPart2?.addEventListener("click", async () => {
    if (!("part2" in window && typeof window.part2 === "function")) return;
    const input = inputArea.value;
    const start = performance.now();
    btnPart2.classList.add("is-loading");
    const result = await window.part2(input);
    btnPart2.classList.remove("is-loading");
    const end = performance.now();
    resultPart2.textContent = result;
    timePart2.textContent = (end - start).toFixed(2) + "ms";
  });

  const themeToggle = document.getElementById("theme-toggle");
  const theme = localStorage.getItem("theme");
  if (theme) themeToggle.checked = theme === "light";
  themeToggle.addEventListener("click", (e) => {
    const newTheme = e.target.checked ? "light" : "dark";
    localStorage.setItem("theme", newTheme);

    hljsThemeLink.href = window.hljsThemes[newTheme];
    document.documentElement.setAttribute("data-theme", newTheme);
  });
});
