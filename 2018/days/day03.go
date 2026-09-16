//go:build day03

package main

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
)

type Vec2 struct {
	X int
	Y int
}

type Claim struct {
	id   string
	pos  Vec2
	size Vec2
}

func unsafeAtoi(s string) int {
	val, _ := strconv.Atoi(s)
	return val
}

func readClaim(s string) Claim {
	s = strings.ReplaceAll(s, "x", ",")
	s = strings.ReplaceAll(s, ":", "")
	s = strings.ReplaceAll(s, ",", " ")
	parts := strings.Split(s, " ")

	return Claim{
		id:   parts[0],
		pos:  Vec2{X: unsafeAtoi(parts[2]), Y: unsafeAtoi(parts[3])},
		size: Vec2{X: unsafeAtoi(parts[4]), Y: unsafeAtoi(parts[5])},
	}
}

func main() {
	path := filepath.Join("../data/day03.txt")
	dat, err := os.ReadFile(path)
	if err != nil {
		panic("File not found")
	}

	lines := strings.Split(strings.TrimSpace(string(dat)), "\n")

	claims := make(map[Vec2][]string)
	for _, line := range lines {
		claim := readClaim(line)
		for x := claim.pos.X; x < claim.pos.X+claim.size.X; x++ {
			for y := claim.pos.Y; y < claim.pos.Y+claim.size.Y; y++ {
				pos := Vec2{X: x, Y: y}
				claims[pos] = append(claims[pos], claim.id)
			}
		}
	}

	hasOverlapMap := make(map[string]bool)
	overlap := 0
	for _, claimers := range claims {
		if len(claimers) > 1 {
			overlap++
			for _, claimer := range claimers {
				hasOverlapMap[claimer] = true
			}
		} else if !hasOverlapMap[claimers[0]] {
			hasOverlapMap[claimers[0]] = false
		}
	}

	fmt.Printf("Part 1 = %d\n", overlap)

	for claimer, hasOverlap := range hasOverlapMap {
		if !hasOverlap {
			fmt.Printf("Part 2 = %s\n", claimer[1:])
			break
		}
	}
}
