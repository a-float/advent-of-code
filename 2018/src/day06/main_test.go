package main

import "testing"

var input = `
1, 1
1, 6
8, 3
3, 4
5, 5
8, 9`

func TestPart1(t *testing.T) {
	got := part1(input)
	want := 17

	if got != want {
		t.Errorf("part1() = %d, want %d", got, want)
	}
}

func TestPart2(t *testing.T) {
	got := part2(input, 32)
	want := 16

	if got != want {
		t.Errorf("part2() = %d, want %d", got, want)
	}
}
