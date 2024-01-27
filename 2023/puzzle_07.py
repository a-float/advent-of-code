from enum import Enum
from collections import Counter


class Kind(Enum):
    FIVE = 7
    FOUR = 6
    FULL_HOUSE = 5
    THREE = 4
    TWO_PAIRS = 3
    ONE_PAIR = 2
    HIGH_CARD = 1


def get_high_card(hand, part2=False):
    order = "23456789TJQKA" if not part2 else "J23456789TQKA"
    score = 0
    for i, card in enumerate(hand):
        score += len(order) ** (len(hand) - i) * order.find(card)
    return score


def get_kind_1(hand):
    cnt = dict(Counter(hand))
    if len(cnt) == 1:
        return Kind.FIVE
    if len(cnt) == 2:
        if max(cnt.values()) == 4:
            return Kind.FOUR
        else:
            return Kind.FULL_HOUSE
    if len(cnt) == 3:
        if max(cnt.values()) == 3:
            return Kind.THREE
        else:
            return Kind.TWO_PAIRS
    if len(cnt) == 4:
        return Kind.ONE_PAIR
    return Kind.HIGH_CARD


def get_kind_2(hand):
    cnt = dict(Counter(hand))
    js = cnt.get("J", 0)
    if len(cnt) == 1:
        return Kind.FIVE
    if len(cnt) == 2:
        if js > 0:
            return Kind.FIVE
        if max(cnt.values()) == 4:
            return Kind.FOUR
        else:
            return Kind.FULL_HOUSE
    if len(cnt) == 3:
        if max(cnt.values()) == 3:
            if js > 0:
                return Kind.FOUR
            return Kind.THREE
        else:
            if js == 2:
                return Kind.FOUR
            elif js == 1:
                return Kind.FULL_HOUSE
            return Kind.TWO_PAIRS
    if len(cnt) == 4:
        if js > 0:
            return Kind.THREE
        return Kind.ONE_PAIR

    if js > 0:
        return Kind.ONE_PAIR
    return Kind.HIGH_CARD


with open("data07.txt", "r") as f:
    lines = f.read().split("\n")
    bids = [(line.split()[0], int(line.split()[1])) for line in lines]
    print(bids)
    # for bid in bids:
    # print(f"{bid} {get_kind(bid[0])} {get_high_card(bid[0])}")

    ordered = sorted(
        bids,
        key=lambda b: get_kind_2(b[0]).value * 1e7 + get_high_card(b[0], True),
    )
    print(ordered)
    part1 = 0
    for i, o in enumerate(ordered):
        part1 += o[1] * (i + 1)
    print(f"Part  I = ", part1)
 