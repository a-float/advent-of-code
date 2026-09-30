package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func part1(lines []string) int {
	twos := 0
	threes := 0

	for _, line := range lines {
		count := map[byte]int{}

		for _, char := range []byte(line) {
			count[char]++
		}
		hasTwo := false
		hasThree := false
		for _, c := range count {
			if c == 2 {
				hasTwo = true
			}
			if c == 3 {
				hasThree = true
			}
		}

		if hasTwo {
			twos++
		}
		if hasThree {
			threes++
		}
	}
	return twos * threes
}

func part2(lines []string) string {
	compare := func(a string, b string) int {
		diff := 0
		for i, x := range []byte(a) {
			if x != b[i] {
				diff++
			}
		}
		return diff
	}

	for i := range len(lines) {
		for j := i; j < len(lines); j++ {
			diff := compare(lines[i], lines[j])
			if diff == 1 {
				commonLetters := []byte{}
				for k, c := range []byte(lines[i]) {
					if c == lines[j][k] {
						commonLetters = append(commonLetters, c)
					}
				}
				return string(commonLetters)
			}
		}
	}

	panic("No similar strings found")
}

func main() {
	path := filepath.Join("./day02/input.txt")
	dat, err := os.ReadFile(path)
	check(err)

	lines := strings.Split(strings.TrimSpace(string(dat)), "\n")

	fmt.Printf("Part 1 = %d\n", part1(lines))
	fmt.Printf("Part 2 = %s\n", part2(lines))
}
