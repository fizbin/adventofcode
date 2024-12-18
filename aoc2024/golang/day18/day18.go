package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

// This is surely in some standard library somewhere, yes?
type pair[T, U any] struct {
	fst T
	snd U
}

type Location struct {
	x, y int
}

type Direction struct {
	x, y int
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

func getNeighbors(grid map[Location]byte, current Location) []Location {
	retval := make([]Location, 0)
	poss := []Location{{current.x, current.y - 1}, {current.x - 1, current.y}, {current.x + 1, current.y}, {current.x, current.y + 1}}
	for _, p := range poss {
		if grid[p] == '.' {
			retval = append(retval, p)
		}
	}
	return retval
}

func mkmap(fallen []Location) map[Location]byte {
	retval := make(map[Location]byte)
	for xidx := range 71 {
		for yidx := range 71 {
			retval[Location{xidx, yidx}] = '.'
		}
	}
	for _, p := range fallen {
		retval[p] = '#'
	}
	return retval
}

func stepsToGoal(grid map[Location]byte) int {
	minsteps := make(map[Location]int)
	for loc := range grid {
		minsteps[loc] = len(grid) + 10
	}
	q := []pair[Location, int]{{Location{0, 0}, 0}}
	for len(q) > 0 {
		work := q[0]
		q = q[1:]
		if minsteps[work.fst] <= work.snd {
			continue
		}
		minsteps[work.fst] = work.snd
		if work.fst == (Location{70, 70}) {
			return work.snd
		}
		for _, nb := range getNeighbors(grid, work.fst) {
			q = append(q, pair[Location, int]{nb, 1 + work.snd})
		}
	}
	return -1
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc18.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	mydata := []Location{}
	for scanner.Scan() {
		splitted := strings.Split(scanner.Text(), ",")
		lft, _ := strconv.Atoi(splitted[0])
		rgt, _ := strconv.Atoi(splitted[1])
		mydata = append(mydata, Location{lft, rgt})
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning:", err)
	}
	mymap := mkmap(mydata[:1024])
	fmt.Printf("Part 1: %d\n", stepsToGoal(mymap))
	highval := len(mydata)
	lowval := 1024
	for lowval+1 < highval {
		midval := (lowval + highval) / 2
		mymap = mkmap(mydata[:midval])
		if stepsToGoal(mymap) < 0 {
			highval = midval
		} else {
			lowval = midval
		}
	}
	fmt.Printf("Part 2: %d,%d\n", mydata[lowval].x, mydata[lowval].y)
}
