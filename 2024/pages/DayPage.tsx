import { Html } from "@elysiajs/html";
import { Layout } from "./Layout";

type DayPageProps = {
  day: number;
};

export const DayPage = (props: DayPageProps) => {
  const scriptSrc =
    '"' + `/public/out/day-${String(props.day).padStart(2, "0")}.js` + '"';

  return (
    <Layout>
      <section class="hero is-warning">
        <div class="hero-body">
          <p class="title">Day {props.day}</p>
          <p class="subtitle"></p>
          <a href="/" class="back-link">
            Go back
          </a>
        </div>
      </section>
      <main class="container is-fullhd p-6 day-container">
        <section class="input-section">
          <textarea
            id="input"
            class="textarea"
            placeholder="Enter your input here..."
          />
          <div class="buttons">
            <button id="run-part-1" class="button">
              Run Part 1
            </button>
            <button id="run-part-2" class="button">
              Run Part 2
            </button>
          </div>
        </section>

        <section class="results-section">
          <div class="result">
            <h3 class="is-size-3">Part 1</h3>
            <hr class="my-4" />
            <p>
              <strong>Answer: </strong> <span id="result-part-1">?</span>
            </p>
            <p>
              <strong>Execution Time: </strong>
              <span id="time-part-1">?</span>
            </p>
          </div>
          <div class="result">
            <h3 class="is-size-3">Part 2</h3>
            <hr class="my-4" />
            <p>
              <strong>Answer: </strong> <span id="result-part-2">?</span>
            </p>
            <p class="color-primary">
              <strong>Execution Time: </strong>
              <span id="time-part-2">?</span>
            </p>
          </div>
        </section>
      </main>
      <script type="module">
        {`import {part1, part2} from ${scriptSrc}
          window.part1 = part1;
          window.part2 = part2;
          console.log('done')`}
      </script>
      <script src="/public/setupListeners.js" />
    </Layout>
  );
};
