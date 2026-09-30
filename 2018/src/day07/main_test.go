package main

import "testing"

var input = `
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
`

func TestPart1(t *testing.T) {
	got := part1(input)
	want := "CABDFE"

	if got != want {
		t.Errorf("part1() = %s, want %s", got, want)
	}
}

func TestPart2(t *testing.T) {
	got := part2(input, 2, 0)
	want := 15

	if got != want {
		t.Errorf("part2() = %d, want %d", got, want)
	}
}
