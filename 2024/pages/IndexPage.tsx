import { Html } from "@elysiajs/html";
import { Layout } from "./Layout";

type IndexPageProps = {
  stars: Record<number, [boolean, boolean]>;
};

// prettier-ignore
const christmasEmojis = [
  '🎄', '🎅', '🤶', '🎁', '❄️', '⛄', '🎉', '🌟', '🎶', '🎼', 
  '🎊', '🎋', '🍪', '🥛', '🕯️', '🦌', '🧦', '🧸', '🎇', '🌨️',
  '🎍', '🏠', '🔔', '🎀', '🦋', '🌍', '🎆'
];

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
        <p class="title">Advent of Code 2024</p>
        <p class="subtitle">Made by me</p>
      </div>
    </section>
    <main class="container is-fullhd p-6">
      <div class="card-grid">
        {Array.from({ length: 25 })
          .map((_, i) => i)
          .filter(isPast)
          .map((i) => (
            <a href={`/day/${i + 1}`}>
              <div class="day-card">
                <span class="emoji">{christmasEmojis[i]}</span>
                <span class="title is-4 m-0">Day {i + 1}</span>
                <div class="stars">
                  {props.stars[i]?.[0] ? (
                    <span class="star filled">★</span>
                  ) : (
                    <span class="star">☆</span>
                  )}
                  {props.stars[i]?.[1] ? (
                    <span class="star filled">★</span>
                  ) : (
                    <span class="star">☆</span>
                  )}
                </div>
              </div>
            </a>
          ))}
      </div>
    </main>
  </Layout>
);
