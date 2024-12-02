import { Elysia, t } from "elysia";
import { html, Html } from "@elysiajs/html";
import { staticPlugin } from "@elysiajs/static";
import { IndexPage } from "./pages/IndexPage";
import { DayPage } from "./pages/DayPage";
import { join } from "path";

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
  .onError(({ error }) => console.log(error))
  .get("/day/:day", ({ params: { day } }) => <DayPage day={day} />, {
    params: t.Object({ day: t.Number({ minimum: 1, maximum: 25 }) }),
  })
  .get("/", async () => {
    return <IndexPage stars={stars} />;
  })
  .listen(parseInt(process.env.PORT || "3000"));

console.log(`🦊 Elysia running on port ${app.server?.port}...`);
