from string import digits


def scan_part(x, y, data):
    row_len = len(data[0])
    taken = set()
    nums = []
    for dx in range(x - 1, x + 2):
        for dy in range(y - 1, y + 2):
            if data[dy][dx] in digits:
                if dy * 10 + dx in taken:
                    continue
                left = 0
                right = 0
                while data[dy][dx - left - 1] in digits:
                    left += 1
                while dx + right < row_len - 1 and data[dy][dx + right + 1] in digits:
                    right += 1
                for d in range(-left, right + 1):
                    taken.add(dy * 10 + dx + d)
                part = data[dy][dx - left : dx + right + 1]
                nums.append(int(part))
    gears = nums[0] * nums[1] if data[y][x] == "*" and len(nums) == 2 else 0
    return sum(nums), gears


with open("data3.txt", "r") as f:
    data = f.read().split("\n")
    part1, part2 = 0, 0
    for y, row in enumerate(data):
        for x, c in enumerate(row):
            if c not in "." + digits:
                a, b = scan_part(x, y, data)
                part1 += a
                part2 += b
    print("Part I:  ", part1)
    print("Part II: ", part2)
