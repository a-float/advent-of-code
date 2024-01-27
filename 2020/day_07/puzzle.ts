import * as fs from 'fs';

type Rule = { name: string, count: number };
const holded: Map<string, Rule[]> = new Map();
const holders: Map<string, Rule[]> = new Map();

/**
 * @returns name of the holding bag with a list of bags have to be inside of it in a given count 
 */
function parseRuleLine(line: string): [string, Rule[]] {
    const match = line.replace(/[\r.]/g, '').replace(/bags/g, 'bag').split(/contain|,/).map(x => x.trim())
    const holder = match[0];
    if (match.length == 2 && match[1].startsWith('no')) {
        return [holder, []];
    }
    const subRules = match.slice(1).map(m => ({ name: m.split(' ').slice(1).join(' '), count: parseInt(m.split(' ')[0]) }))
    return [holder, subRules];
}

/**
 * @returns set of bag names that can contain the specified bag 
 */
function findPossibleHolders(name: string, holdedMap: Map<string, Rule[]>, set = new Set<string>()): Set<string> {
    for (const rule of holdedMap.get(name) || []) {
        set.add(rule.name);
        findPossibleHolders(rule.name, holdedMap, set);
    }
    return set;
}

/**
 * @returns number of bags inside of the given one including the given one (subtract one to get part 2 result) 
 */
function countInsideBags(name: string, holder: Map<string, Rule[]>, mult = 1): number {
    if (holder.get(name)!.length > 0) {
        // containing bags + numbers of bags inside all of them
        return mult + holder.get(name)!.reduce((acc, rule) => acc + countInsideBags(rule.name, holder, rule.count * mult), 0);
    } else {
        return mult;
    }
}

const filename = process.argv.length < 3 ? 'data.txt' : process.argv[2];
fs.readFile(filename, 'ascii', (err, content) => {
    if (err) {
        console.error("Unable to open file " + filename);
        process.exit(1);
    }
    for (const line of content.split("\n")) {
        const [holder, subRules] = parseRuleLine(line);
        // for each bag - what bag can contain it
        for (const rule of subRules) {
            if (!holded.has(rule.name)) {
                holded.set(rule.name, []);
            }
            holded.get(rule.name)?.push({ name: holder, count: rule.count });
        }
        // for each bag - what bags can be inside of it
        holders.set(holder, subRules);
    }
    const shinyBagHolders = findPossibleHolders('shiny gold bag', holded);
    console.log(`My shiny gold bag can be in of ${shinyBagHolders.size} colored bags`);
    console.log(`There are ${countInsideBags('shiny gold bag', holders) - 1} bags inside of mine`);
})
