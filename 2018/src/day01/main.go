package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	dat, err := os.ReadFile("./day01/input.txt")
	check(err)

	lines := strings.Split(strings.TrimSpace(string(dat)), "\n")

	freq := 0
	for _, line := range lines {
		diff, err := strconv.Atoi(line)
		check(err)
		freq += diff
	}

	part1 := freq

	freq = 0
	seen := map[int]int{0: 1}
	i := 0
	for {
		line := lines[i%len(lines)]
		diff, _ := strconv.Atoi(line)
		freq += diff
		seen[freq]++
		if seen[freq] == 2 {
			break
		}
		i++
	}

	fmt.Printf("Part 1 = %d\n", part1)
	fmt.Printf("Part 2 = %d\n", freq)
}
