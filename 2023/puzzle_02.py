with open("data2.txt", "r") as f:
    data = f.read().split("\n")
    sum = 0
    power = 0
    for gameId, d in enumerate(data):
        # d = d.replace(",", "")
        x = d.split(":")[1].split(";")
        x = list(map(lambda x: x.strip().split(","), x))
        
        gameOk = True
        mins = {"red": 0, "blue": 0, "green": 0}
        for g in x:
            for r in g:
                pair = r.strip().split(" ")
                count = int(pair[0])
                color = pair[1]
                # print("pair", count, color)
                mins[color] = max(mins[color], count)
                if color == "red" and count > 12:
                    gameOk = False
                if color == "blue" and count > 14:
                    gameOk = False
                if color == "green" and count > 13:
                    gameOk = False
        if gameOk:
            sum += gameId + 1
        power += mins["blue"] * mins["green"] * mins["red"]
    print(sum)
    print(power)
