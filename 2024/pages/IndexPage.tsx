import { Html } from "@elysiajs/html";
import { Layout } from "./Layout";
import { christmasEmojis, subtitles } from "./shared";

type IndexPageProps = {
  stars: Record<number, [boolean, boolean]>;
};

const isPast = (day: number) => {
  const date = new Date();
  date.setDate(day + 1);
  date.setMonth(11);
  date.setFullYear(2024);
  return new Date().valueOf() - date.valueOf() >= 0;
};

export const IndexPage = (props: IndexPageProps) => (
  <Layout>
    <section class="hero is-info">
      <div class="hero-body">
        <h1 class="title">Advent of Code 2024</h1>
        <h2 class="subtitle">coded with Typescript</h2>
      </div>
    </section>
    <main class="container is-fullhd">
      <div class="card-grid">
        {Array.from({ length: 25 })
          .map((_, i) => i)
          .filter(isPast)
          .map((day) => (
            <a class="day-card-link" href={`/day/${day + 1}`}>
              <div class="day-card">
                <span class="emoji">{christmasEmojis[day]}</span>
                <span class="title is-4 m-0">Day {day + 1}</span>
                <div class="stars">
                  {props.stars[day]?.[0] ? (
                    <span class="star filled">★</span>
                  ) : (
                    <span class="star">☆</span>
                  )}
                  {props.stars[day]?.[1] ? (
                    <span class="star filled">★</span>
                  ) : (
                    <span class="star">☆</span>
                  )}
                </div>
              </div>
              {!!subtitles[day + 1] && (
                <div class="subtitle is-size-6">{subtitles[day + 1]}</div>
              )}
            </a>
          ))}
      </div>
    </main>
  </Layout>
);
