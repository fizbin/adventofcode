package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

type Location struct {
	x int
	y int
}

type Direction struct {
	x int
	y int
}

func (d Direction) TurnRight() Direction {
	return Direction{x: d.y, y: -d.x}
}

func (where Location) Add(d Direction) Location {
	return Location{x: where.x + d.x, y: where.y + d.y}
}

type guardState struct {
	loc Location
	dir Direction
}

func checkObstacle(cmap map[Location]rune, guardstart Location, obstacle Location) bool {
	guarddir := Direction{-1, 0}
	guardhist := make(map[guardState]bool)
	guardspot := guardstart
	for cmap[guardspot] != '\x00' {
		if guardhist[guardState{guardspot, guarddir}] {
			return true
		}
		guardhist[guardState{guardspot, guarddir}] = true
		nextspot := guardspot.Add(guarddir)
		for nextspot == obstacle || cmap[nextspot] == '#' {
			guarddir = guarddir.TurnRight()
			nextspot = guardspot.Add(guarddir)
		}
		guardspot = nextspot
	}
	return false
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc6.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatalf("Couldn't open file: %v", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	cmap := make(map[Location]rune)
	row := 0
	var guardstart Location
	for scanner.Scan() {
		line := scanner.Text()
		for col, spot := range line {
			cmap[Location{row, col}] = spot
			if spot == '^' {
				guardstart = Location{row, col}
			}
		}
		row += 1
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("Error scanning: %v", err)
	}
	guarddir := Direction{-1, 0}
	guardhist := make(map[Location]bool)
	total := 0
	guardspot := guardstart
	for cmap[guardspot] != '\x00' {
		if !guardhist[guardspot] {
			total += 1
			guardhist[guardspot] = true
		}
		for cmap[guardspot.Add(guarddir)] == '#' {
			guarddir = guarddir.TurnRight()
		}
		guardspot = guardspot.Add(guarddir)
	}
	fmt.Println("Part 1:", total)

	total = 0
	for obspot, _ := range guardhist {
		if obspot != guardstart {
			if checkObstacle(cmap, guardstart, obspot) {
				total += 1
			}
		}
	}
	fmt.Println("Part 2:", total)
}
