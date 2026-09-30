package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Point struct {
	X int
	Y int
}

func abs(x int) int {
	if x > 0 {
		return x
	} else {
		return -x
	}
}

func main() {
	input, err := os.ReadFile("./day06/input.txt")
	if err != nil {
		panic(err)
	}

	fmt.Println("Part 1:", part1(string(input)))
	fmt.Println("Part 2:", part2(string(input), 10000))
}

func part1(input string) int {
	var minX, maxX, minY, maxY int
	lines := strings.Split(strings.TrimSpace(input), "\n")
	var points []Point
	for _, line := range lines {
		parts := strings.Split(line, ", ")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		minX = min(minX, x)
		maxX = max(maxX, x)
		minY = min(minY, y)
		maxY = max(maxY, y)
		points = append(points, Point{X: x, Y: y})
	}

	totals := make(map[int]int)
	edges := make(map[int]bool)

	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			minDist := math.MaxInt
			minIdx := -1
			stelemate := false
			for idx, point := range points {
				dist := abs(point.X-x) + abs(point.Y-y)
				if dist == minDist {
					stelemate = true
				}
				if dist < minDist {
					minDist = dist
					minIdx = idx
					stelemate = false
				}
			}
			if !stelemate {
				if x == minX || y == minY || x == maxX || y == maxY {
					edges[minIdx] = true
				}
				totals[minIdx]++
			}
		}
	}

	fmt.Printf("%v\n", totals)
	fmt.Printf("%v\n", edges)

	biggestFinite := 0
	for idx := range points {
		if totals[idx] > biggestFinite && !edges[idx] {
			biggestFinite = totals[idx]
		}
	}

	return biggestFinite
}

func part2(input string, maxDist int) int {
	var minX, maxX, minY, maxY int
	lines := strings.Split(strings.TrimSpace(input), "\n")
	var points []Point
	for _, line := range lines {
		parts := strings.Split(line, ", ")
		x, _ := strconv.Atoi(parts[0])
		y, _ := strconv.Atoi(parts[1])
		minX = min(minX, x)
		maxX = max(maxX, x)
		minY = min(minY, y)
		maxY = max(maxY, y)
		points = append(points, Point{X: x, Y: y})
	}

	goods := 0

	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			sumDist := 0
			for _, point := range points {
				dist := abs(point.X-x) + abs(point.Y-y)
				sumDist += dist
			}
			if sumDist < maxDist {
				goods++
			}
		}
	}

	return goods
}
