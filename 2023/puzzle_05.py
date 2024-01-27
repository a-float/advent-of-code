from time import time


def seeds_to_soil(seed_spans, maps):
    spans = seed_spans[:]
    for m in maps:
        for i, val in enumerate(spans):
            for span in m:
                if val >= span[1] and val < span[1] + span[2]:
                    spans[i] = val + span[0] - span[1]
                    break
    return spans


with open("data5.txt", "r") as f:
    data = f.read().strip()
    sections = [d.split("\n") for d in data.split("\n\n")]
    seeds = [int(seed) for seed in sections[0][0].split(":")[1].split()]
    raw_maps = sections[1:]
    print(seeds)
    # maps = {s: s for s in seeds}
    maps = []
    for m in raw_maps:
        y = []
        # print(f"Parsing: {m[0]}")
        for x in m[1:]:
            dest, src, size = [int(n) for n in x.split()]
            y.append((dest, src, size))
        maps.append(y)

    # for m in maps:
    #     print(m)

    soils = seeds_to_soil(seeds, maps)
    print(f"Part I  = {min(soils)}\n")

    part2 = float("inf")
    pairs = len(seeds) // 2
    for i in range(0, len(seeds), 2):
        t0 = time()
        print(f"Starting pair {i//2+1}/{pairs}")
        ss = list(range(seeds[i], seeds[i] + seeds[i + 1]))
        part2 = min([part2] + seeds_to_soil(ss, maps))
        print(f"- done in {time() - t0}s")

    print(f"Part II = {part2}")
