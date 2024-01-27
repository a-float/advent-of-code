# with open("data04.txt", "r") as f:
#     lines = f.read().split("\n")
#     mults = defaultdict(lambda: 0)
#     total = 0
#     for line in lines:
#         nums = line.split(":")[1]
#         winning, mine = nums.split("|")
#         winning = set([int(x) for x in winning.split()])
#         mine = set([int(x) for x in mine.split()])
#         matches = len(winning.intersection(mine))
#         if matches > 0:
#             total += 2**(matches - 1)
#     print(total)

    
with open("data04.txt", "r") as f:
    lines = f.read().split("\n")
    copies = {i: 1 for i in range(len(lines))}
    total = 0
    for lineId, line in enumerate(lines):
        nums = line.split(":")[1]
        winning, mine = nums.split("|")
        winning = set([int(x) for x in winning.split()])
        mine = set([int(x) for x in mine.split()])
        matches = len(winning.intersection(mine))
        for i in range(0, matches):
            copies[lineId + i + 1] += copies[lineId]
    
    part2 = sum([v for k,v in copies.items()])
    print(part2)