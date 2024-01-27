def predict(seq):
    if all([x == 0 for x in seq]):
        return 0
    diffs = [seq[i + 1] - seq[i] for i in range(len(seq) - 1)]
    next_diff = predict(diffs)
    return seq[-1] + next_diff


def predict_past(seq):
    if all([x == 0 for x in seq]):
        return 0
    diffs = [seq[i] - seq[i + 1] for i in range(len(seq) - 1)]
    prev_diff = predict_past(diffs)
    return seq[0] + prev_diff


with open("data9.txt", "r") as f:
    seqs = f.read().strip().split("\n")
    seqs = [[int(x) for x in seq.split()] for seq in seqs]
    predictions = [predict(seq) for seq in seqs]
    print(f"Part I = {sum(predictions)}")

    predictions = [predict_past(seq) for seq in seqs]
    # print(predictions)
    print(f"Part I = {sum(predictions)}")
