with open("data1.txt", "r") as f:
    data = f.read().split("\n")
    sum = 0
    for d in data:
        d = d.replace("zero", "z0o")
        d = d.replace("one", "o1e")
        d = d.replace("two", "t2o")
        d = d.replace("three", "t3r")
        d = d.replace("four", "f4r")
        d = d.replace("five", "f5e")
        d = d.replace("six", "s6x")
        d = d.replace("seven", "s7n")
        d = d.replace("eight", "e8t")
        d = d.replace("nine", "n9n")
        a = list(filter(lambda c: c in "0123456789", d))
        sum += int(a[0] + a[-1])
        print(sum)