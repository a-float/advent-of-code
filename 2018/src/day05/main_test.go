package main

import "testing"

var input = `dabAcCaCBAcCcaDA`

func TestPart1(t *testing.T) {
	got := part1(input)
	want := 10

	if got != want {
		t.Errorf("part1() = %d, want %d", got, want)
	}
}

func TestPart2(t *testing.T) {
	got := part2(input)
	want := 6

	if got != want {
		t.Errorf("part2() = %d, want %d", got, want)
	}
}
