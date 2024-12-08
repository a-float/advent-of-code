import { Elysia, error, t } from "elysia";
import { html, Html } from "@elysiajs/html";
import { staticPlugin } from "@elysiajs/static";
import { IndexPage } from "./pages/IndexPage";
import { DayPage } from "./pages/DayPage";
import path, { join } from "path";

const CODE_TRIM_START = `if (typeof require !== "undefined" && require.main === module) {`;
const stars: Record<number, [boolean, boolean]> = {};
for (let i = 0; i < 25; i++) {
  const filename = `day-${String(i + 1).padStart(2, "0")}.js`;
  const file = Bun.file(join(process.cwd(), "public", "out", filename));
  await file
    .text()
    .then((text) => {
      stars[i] = [text.includes("part1"), text.includes("part2")];
    })
    .catch(() => {});
}

const app = new Elysia()
  .use(html())
  .use(staticPlugin())
  .get(
    "/day/:day",
    async ({ params: { day } }) => {
      const dayName = String(day).padStart(2, "0");
      const file = Bun.file(path.join("src", `day-${dayName}.ts`));
      try {
        let code = await file.text();
        code = code.slice(0, code.indexOf(CODE_TRIM_START)).trim();
        return <DayPage day={day} code={code} />;
      } catch {
        return `Will solve on ${new Date(`2024-12-${day}`).toDateString()}`;
      }
    },
    { params: t.Object({ day: t.Number({ minimum: 1, maximum: 25 }) }) }
  )
  .get("/", async () => {
    return <IndexPage stars={stars} />;
  })
  .listen(parseInt(process.env.PORT || "3000"));

console.log(`🦊 Elysia running on port ${app.server?.port}...`);
