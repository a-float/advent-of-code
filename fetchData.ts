import fs from "node:fs"

const AOC_SESSION = ""
const year = parseInt(Bun.argv.at(2) ?? "")
if (isNaN(year)) {
    console.log('Usage bun fetchData.ts [year]')
    process.exit()
}

for (let day = 1; day <= 25; day++) {
    const path = `./data/${year}/data${day}.txt`;
    if (fs.existsSync(path)) continue
    const result = await fetch(`https://adventofcode.com/${year}/day/${day}/input`, {
        headers: {
            Cookie: `session=${AOC_SESSION};`
        }
    }).then(res => res.text());
    console.log(`Fetched ${year} day ${day}`)
    await Bun.write(path, result);
}