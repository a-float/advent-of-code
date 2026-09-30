package main

import (
	"fmt"
	"math"
	"os"
	"slices"
	"strings"
)

type Job struct {
	letter   byte
	timeLeft int
}

func main() {
	input, err := os.ReadFile("./day07/input.txt")
	if err != nil {
		panic(err)
	}

	fmt.Println("Part 1:", part1(string(input)))
	fmt.Println("Part 2:", part2(string(input), 5, 60))
}

func part1(input string) string {
	lines := strings.Split(strings.TrimSpace(input), "\n")
	reqs := make(map[byte][]byte)

	for _, line := range lines {
		before, after := line[5], line[36]
		reqs[after] = append(reqs[after], before)
		if _, exists := reqs[before]; !exists {
			reqs[before] = nil
		}
	}

	ready := []byte{}
	answer := make([]byte, 0, len(lines))
	for len(reqs) > 0 {
		for key, slice := range reqs {
			if len(slice) == 0 {
				delete(reqs, key)
				ready = append(ready, key)
			}
		}
		slices.Sort(ready)
		answer = append(answer, ready[0])
		for key, slice := range reqs {
			reqs[key] = slices.DeleteFunc(slice, func(b byte) bool {
				return b == ready[0]
			})
		}
		ready = ready[1:]
	}

	return string(answer)
}

func part2(input string, workerCount, timeDiff int) int {
	lines := strings.Split(strings.TrimSpace(input), "\n")
	reqs := make(map[byte][]byte)

	for _, line := range lines {
		before, after := line[5], line[36]
		reqs[after] = append(reqs[after], before)
		if _, exists := reqs[before]; !exists {
			reqs[before] = nil
		}
	}

	answer := []byte{}
	totalTime := 0
	workers := make([]Job, workerCount)

	for len(reqs) > 0 {
		// schedule jobs
		for key, slice := range reqs {
			if len(slice) == 0 {
				for i := range workers {
					if workers[i].letter == 0 {
						workers[i].letter = key
						workers[i].timeLeft = int(key-'A') + 1 + timeDiff
						delete(reqs, key) // cannot be rescheduled
						break
					}
				}
			}
		}

		// advance time
		timeToAdvance := math.MaxInt
		for _, worker := range workers {
			if worker.letter > 0 { // has a job
				timeToAdvance = min(timeToAdvance, worker.timeLeft)
			}
		}
		totalTime += timeToAdvance
		ready := []byte{}
		for i := range workers {
			workers[i].timeLeft -= timeToAdvance
			if workers[i].timeLeft == 0 && workers[i].letter > 0 {
				ready = append(ready, workers[i].letter)
				workers[i].letter = 0
			}
		}
		slices.Sort(ready)
		answer = slices.Concat(answer, ready)

		// cleanup - marks new reqs for picking up
		for key, slice := range reqs {
			reqs[key] = slices.DeleteFunc(slice, func(b byte) bool {
				return slices.Contains(ready, b)
			})
		}
		ready = nil
	}

	return totalTime
}
