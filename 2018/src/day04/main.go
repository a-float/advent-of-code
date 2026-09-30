package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type SleepCounter map[string][60]int

func main() {
	input, err := os.ReadFile("./day04/input.txt")
	if err != nil {
		panic(err)
	}

	fmt.Println("Part 1:", part1(string(input)))
	fmt.Println("Part 2:", part2(string(input)))
}

func incrementSleep(slept SleepCounter, guardId string, start, end int) {
	for i := start; i < end; i++ {
		currentMinutes := slept[guardId]
		currentMinutes[i]++
		slept[guardId] = currentMinutes
	}
}

func populateSleeps(input string) SleepCounter {
	lines := strings.Split(strings.TrimSpace(input), "\n")
	sort.Strings(lines)

	var (
		minute     int
		guardId    string
		startSleep int
		endSleep   int
	)

	slept := make(SleepCounter)

	for _, log := range lines {
		parts := strings.SplitN(log, " ", 5)
		minute, _ = strconv.Atoi(parts[1][3:5])
		switch {
		case strings.Contains(log, "Guard"):
			guardId = parts[3]
		case strings.Contains(log, "falls asleep"):
			startSleep = minute
		case strings.Contains(log, "wakes up"):
			endSleep = minute
			incrementSleep(slept, guardId, startSleep, endSleep)
		default:
			fmt.Printf("Unexpected log: %s\n", log)
		}
	}

	return slept
}

func part1(input string) int {
	slept := populateSleeps(input)

	var (
		sleepiestGuardId string
		sleepiestMinute  int
		mostSleeps       int
	)
	for guardId, sleeps := range slept {
		var (
			sum      = 0
			big      = 0
			bigIndex = 0
		)
		for i := range 60 {
			sum += sleeps[i]
			if sleeps[i] > big {
				bigIndex = i
				big = sleeps[i]
			}
		}
		if sum > mostSleeps {
			sleepiestGuardId = guardId
			mostSleeps = sum
			sleepiestMinute = bigIndex
		}
	}

	guardIdNumber, _ := strconv.Atoi(sleepiestGuardId[1:])
	return guardIdNumber * sleepiestMinute
}

func part2(input string) int {
	slept := populateSleeps(input)

	var (
		sleepiestGuardId string
		sleepiestMinute  int
		mostSleeps       int
	)
	for guardId, sleeps := range slept {
		var (
			big      = 0
			bigIndex = 0
		)
		for i := range 60 {
			if sleeps[i] > big {
				bigIndex = i
				big = sleeps[i]
			}
		}
		if big > mostSleeps {
			sleepiestGuardId = guardId
			mostSleeps = big
			sleepiestMinute = bigIndex
		}
	}

	guardIdNumber, _ := strconv.Atoi(sleepiestGuardId[1:])
	return guardIdNumber * sleepiestMinute
}
