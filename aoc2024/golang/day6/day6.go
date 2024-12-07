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

func (loc Location) IsIn(height int, width int) bool {
	return loc.x >= 0 && loc.y >= 0 && loc.x < height && loc.y < width
}

func checkObstacle(grid [][]byte, height int, width int, guardstart Location, obstacle Location) bool {
	guarddir := 0
	guardhist := make([][]byte, height)
	for idx := range guardhist {
		guardhist[idx] = make([]byte, width)
	}
	guardspot := guardstart
	for guardspot.IsIn(height, width) {
		if guardhist[guardspot.x][guardspot.y]&(1<<guarddir) != 0 {
			return true
		}
		guardhist[guardspot.x][guardspot.y] |= 1 << guarddir
		nextspot := guardspot.Add(guarddir)
		for nextspot == obstacle || (nextspot.IsIn(height, width) && grid[nextspot.x][nextspot.y] == '#') {
			guarddir = TurnRight(guarddir)
			nextspot = guardspot.Add(guarddir)
		}
		guardspot = nextspot
		for nextspot != obstacle && nextspot.IsIn(height, width) && grid[nextspot.x][nextspot.y] == '#' {
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

	grid := make([][]byte, 0)
	cmap := make(map[Location]rune)
	height := 0
	width := 0
	var guardstart Location
	for scanner.Scan() {
		line := scanner.Text()
		for col, spot := range line {
			cmap[Location{height, col}] = spot
			if spot == '^' {
				guardstart = Location{height, col}
			}
			if col+1 > width {
				width = col + 1
			}
		}
		grid = append(grid, []byte(line))
		height += 1
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
				results <- checkObstacle(grid, height, width, guardstart, obspot)
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
