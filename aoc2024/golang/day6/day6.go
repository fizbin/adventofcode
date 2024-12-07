package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"runtime"
	"sync"
)

type Location struct {
	x int
	y int
}

func TurnRight(d int) int {
	return (d + 1) % 4
}

func (where Location) Add(d int) Location {
	switch d {
	case 0:
		return Location{x: where.x - 1, y: where.y}
	case 1:
		return Location{x: where.x, y: where.y + 1}
	case 2:
		return Location{x: where.x + 1, y: where.y}
	case 3:
		return Location{x: where.x, y: where.y - 1}
	}
	log.Fatal("Bad direction", d)
	return Location{}
}

type guardState struct {
	loc Location
	dir int
}

func checkObstacle(cmap map[Location]rune, guardstart Location, obstacle Location) bool {
	guarddir := 0
	guardhist := make(map[guardState]bool)
	guardspot := guardstart
	for cmap[guardspot] != '\x00' {
		if guardhist[guardState{guardspot, guarddir}] {
			return true
		}
		guardhist[guardState{guardspot, guarddir}] = true
		nextspot := guardspot.Add(guarddir)
		for nextspot == obstacle || cmap[nextspot] == '#' {
			guarddir = TurnRight(guarddir)
			nextspot = guardspot.Add(guarddir)
		}
		guardspot = nextspot
		for nextspot != obstacle && cmap[nextspot] == '.' {
			guardspot = nextspot
			nextspot = nextspot.Add(guarddir)
		}
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
	guarddir := 0
	guardhist := make(map[Location]bool)
	total := 0
	guardspot := guardstart
	for cmap[guardspot] != '\x00' {
		if !guardhist[guardspot] {
			total += 1
			guardhist[guardspot] = true
		}
		for cmap[guardspot.Add(guarddir)] == '#' {
			guarddir = TurnRight(guarddir)
		}
		guardspot = guardspot.Add(guarddir)
	}
	fmt.Println("Part 1:", total)

	runtime.GOMAXPROCS(runtime.NumCPU())
	results := make(chan bool, len(guardhist))
	var wg sync.WaitGroup
	for obspot := range guardhist {
		if obspot != guardstart {
			wg.Add(1)
			go func(obspot Location) {
				defer wg.Done()
				results <- checkObstacle(cmap, guardstart, obspot)
			}(obspot)
		}
	}
	wg.Wait()
	close(results)

	total = 0
	for result := range results {
		if result {
			total += 1
		}
	}
	fmt.Println("Part 2:", total)
}
