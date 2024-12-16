package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

// This is surely in some standard library somewhere, yes?
type pair[T, U any] struct {
	First  T
	Second U
}

type Location struct {
	x, y int
}

type Direction struct {
	x, y int
}

type State struct {
	loc Location
	dir Direction
}

func (loc Location) Add(dir Direction) Location {
	return Location{loc.x + dir.x, loc.y + dir.y}
}

func (loc Location) Sub(dir Direction) Location {
	return Location{loc.x - dir.x, loc.y - dir.y}
}

func (d Direction) TurnRight() Direction {
	return Direction{d.y, -d.x}
}

func (d Direction) TurnLeft() Direction {
	return Direction{-d.y, d.x}
}

func getNeighborsWithCosts(grid map[Location]byte, current State) []pair[State, int] {
	retval := make([]pair[State, int], 0, 3)
	nxtch, ok := grid[current.loc.Add(current.dir)]
	if ok && nxtch != '#' {
		retval = append(retval, pair[State, int]{State{current.loc.Add(current.dir), current.dir}, 1})
	}
	retval = append(retval, pair[State, int]{State{current.loc, current.dir.TurnLeft()}, 1000})
	retval = append(retval, pair[State, int]{State{current.loc, current.dir.TurnRight()}, 1000})
	return retval
}

func getNeighborsWithCostsBackwards(grid map[Location]byte, current State) []pair[State, int] {
	retval := make([]pair[State, int], 0, 3)
	nxtch, ok := grid[current.loc.Sub(current.dir)]
	if ok && nxtch != '#' {
		retval = append(retval, pair[State, int]{State{current.loc.Sub(current.dir), current.dir}, 1})
	}
	retval = append(retval, pair[State, int]{State{current.loc, current.dir.TurnLeft()}, 1000})
	retval = append(retval, pair[State, int]{State{current.loc, current.dir.TurnRight()}, 1000})
	return retval
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc16.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	height := 0
	// width := 0
	var startLoc, endLoc Location
	mymap := make(map[Location]byte)
	for scanner.Scan() {
		for colidx, ch := range scanner.Text() {
			mymap[Location{x: height, y: colidx}] = byte(ch)
			if ch == 'S' {
				startLoc = Location{x: height, y: colidx}
			}
			if ch == 'E' {
				endLoc = Location{x: height, y: colidx}
			}
		}
		// width = len(txt)
		height += 1
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning:", err)
	}
	costMap := make(map[State]int)
	working := []pair[State, int]{{State{startLoc, Direction{0, 1}}, 0}}
	for len(working) > 0 {
		nexts := make([]pair[State, int], 0, 3*len(working))
		for _, w := range working {
			val, ok := costMap[w.First]
			if val > w.Second || !ok {
				costMap[w.First] = w.Second
				for _, nxt := range getNeighborsWithCosts(mymap, w.First) {
					nexts = append(nexts, pair[State, int]{nxt.First, w.Second + nxt.Second})
				}
			}
		}
		working = nexts
	}
	endCost := -1
	var endState State
	for state, cost := range costMap {
		if state.loc == endLoc {
			if endCost < 0 || cost < endCost {
				endCost = cost
				endState = state
			}
		}
	}
	fmt.Println("Part 1:", endCost)

	goodspots := make(map[Location]bool)
	goodspots[endLoc] = true
	working = []pair[State, int]{{endState, endCost}}
	for len(working) > 0 {
		nexts := make([]pair[State, int], 0, 3*len(working))
		for _, w := range working {
			val, ok := costMap[w.First]
			if ok && val == w.Second {
				// Okay, so we're following the minpath
				goodspots[w.First.loc] = true
				for _, nxt := range getNeighborsWithCostsBackwards(mymap, w.First) {
					nexts = append(nexts, pair[State, int]{nxt.First, w.Second - nxt.Second})
				}
			}
		}
		working = nexts
	}
	fmt.Println("Part 2:", len(goodspots))
}
