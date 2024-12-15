package main

import (
	"bufio"
	"fmt"
	"log"
	"maps"
	"os"
	"slices"
)

type Location struct {
	x, y int
}

type Direction struct {
	x, y int
}

type State struct {
	roboLoc Location
	grid    map[Location]byte
}

func (loc Location) Add(dir Direction) Location {
	return Location{loc.x + dir.x, loc.y + dir.y}
}

func (state State) doStep(dirch byte) State {
	var mydir Direction
	switch dirch {
	case '^':
		mydir = Direction{-1, 0}
	case '<':
		mydir = Direction{0, -1}
	case '>':
		mydir = Direction{0, 1}
	case 'v':
		mydir = Direction{1, 0}
	}
	newGrid := maps.Clone(state.grid)
	newRoboLoc := state.roboLoc.Add(mydir)

	moving := make(map[Location]bool)
	moving[state.roboLoc] = true
	currentPush := []Location{state.roboLoc}
	for len(currentPush) > 0 {
		newPush := make([]Location, 0, len(currentPush))
		for _, where := range currentPush {
			what := state.grid[where]
			// fmt.Println(where, what, what == 'O')
			if what == '#' {
				return state
			}
			if what == '.' {
				continue
			}
			if what == 'O' || what == '@' || ((what == '[' || what == ']') && mydir.x == 0) {
				moving[where] = true
				newPush = append(newPush, where.Add(mydir))
				continue
			}
			if what == '[' {
				moving[where] = true
				moving[where.Add(Direction{0, 1})] = true
				newPush = append(newPush, where.Add(mydir))
				if !slices.Contains(currentPush, where.Add(Direction{0, 1})) {
					newPush = append(newPush, where.Add(Direction{0, 1}).Add(mydir))
				}
			}
			if what == ']' {
				moving[where] = true
				moving[where.Add(Direction{0, -1})] = true
				newPush = append(newPush, where.Add(mydir))
				if !slices.Contains(currentPush, where.Add(Direction{0, -1})) {
					newPush = append(newPush, where.Add(Direction{0, -1}).Add(mydir))
				}
			}
		}
		// fmt.Println("-> ", newPush)
		currentPush = newPush
	}
	for where, _ := range moving {
		newGrid[where] = '.'
	}
	for where, _ := range moving {
		newGrid[where.Add(mydir)] = state.grid[where]
	}
	return State{newRoboLoc, newGrid}
}

func (state State) checkSum() int {
	retval := 0
	for loc, ch := range state.grid {
		if ch == 'O' || ch == '[' {
			retval += 100*loc.x + loc.y
		}
	}
	return retval
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc15.in"
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
	roboLoc := Location{}
	mymap := make(map[Location]byte)
	for scanner.Scan() {
		txt := scanner.Text()
		if txt == "" {
			break
		}
		for colidx, ch := range txt {
			mymap[Location{x: height, y: colidx}] = byte(ch)
			if ch == '@' {
				roboLoc = Location{x: height, y: colidx}
			}
		}
		// width = len(txt)
		height += 1
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning:", err)
	}
	moves := ""
	for scanner.Scan() {
		moves += scanner.Text()
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning:", err)
	}
	state := State{roboLoc: roboLoc, grid: mymap}
	for _, ch := range moves {
		state = state.doStep(byte(ch))
	}
	fmt.Println("Part 1:", state.checkSum())

	mymap2 := make(map[Location]byte)
	for loc, ch := range mymap {
		if ch == '@' {
			mymap2[Location{loc.x, 2 * loc.y}] = '@'
			mymap2[Location{loc.x, 2*loc.y + 1}] = '.'
			roboLoc = Location{loc.x, 2 * loc.y}
		} else if ch == 'O' {
			mymap2[Location{loc.x, 2 * loc.y}] = '['
			mymap2[Location{loc.x, 2*loc.y + 1}] = ']'
		} else {
			mymap2[Location{loc.x, 2 * loc.y}] = ch
			mymap2[Location{loc.x, 2*loc.y + 1}] = ch
		}
	}
	state = State{roboLoc: roboLoc, grid: mymap2}
	// for i := range height {
	// 	for j := range 2 * width {
	// 		fmt.Printf("%c", rune(state.grid[Location{i, j}]))
	// 	}
	// 	fmt.Println()
	// }
	// fmt.Println()
	for _, ch := range moves {
		state = state.doStep(byte(ch))
		// for i := range height {
		// 	for j := range 2 * width {
		// 		fmt.Printf("%c", rune(state.grid[Location{i, j}]))
		// 	}
		// 	fmt.Println()
		// }
		// fmt.Println()
	}
	fmt.Println("Part 2:", state.checkSum())
}
