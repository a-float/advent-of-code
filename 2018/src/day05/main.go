package main

import (
	"fmt"
	"math"
	"os"
	"strings"
)

func main() {
	input, err := os.ReadFile("./day05/input.txt")
	if err != nil {
		panic(err)
	}

	fmt.Println("Part 1:", part1(string(input)))
	fmt.Println("Part 2:", part2(string(input)))
}

func part1(input string) int {
	s := strings.TrimSpace(input)
	ns := s
	for {
		for c := int('a'); c <= int('z'); c++ {
			unit := string(rune(c)) + strings.ToUpper(string(rune(c)))
			unit2 := strings.ToUpper(string(rune(c))) + string(rune(c))
			// fmt.Printf("unit %s\n", unit)
			ns = strings.ReplaceAll(ns, unit, "")
			ns = strings.ReplaceAll(ns, unit2, "")
		}
		// fmt.Printf("%s\n\n", ns)
		if len(s) == len(ns) {
			break
		}
		s = ns
	}
	return len(ns)
}

func part2(input string) int {
	trimmed := strings.TrimSpace(input)
	best := math.MaxInt
	for c := int('a'); c <= int('z'); c++ {
		s := strings.ReplaceAll(trimmed, string(rune(c)), "")
		s = strings.ReplaceAll(s, strings.ToUpper(string(rune(c))), "")
		best = min(best, part1(s))
	}

	return best
}
