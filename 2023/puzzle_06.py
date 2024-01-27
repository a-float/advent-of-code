with open("data06.txt", "r") as f:
    lines = f.read().split("\n")
    times = [int(x) for x in lines[0].split()[1:]]
    dists = [int(x) for x in lines[1].split()[1:]]
    print(times, dists)

    races = list(zip(times, dists))
    part1 = 1
    for r, race in enumerate(races):
        ways = 0
        for i in range(1, race[0]):
            time_left = race[0] - i
            speed = i
            boat_range = speed * time_left
            if boat_range > race[1]:
                ways += 1
        print(ways)
        part1 *= ways

    print(f"Part  I: {part1}")

    big_time = int("".join(lines[0].split()[1:]))
    big_dist = int("".join(lines[1].split()[1:]))
    races = [(big_time, big_dist)]
    part2 = 1
    for r, race in enumerate(races):
        ways = 0
        for i in range(1, race[0]):
            if(i % 500000 == 0):
                print(i)
            time_left = race[0] - i
            speed = i
            boat_range = speed * time_left
            if boat_range > race[1]:
                ways += 1
        print(ways)
        part2 *= ways

    print(f"Part II: {part2}")