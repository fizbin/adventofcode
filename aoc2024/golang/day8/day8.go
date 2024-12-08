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

func (where Location) Diff(other Location) Direction {
	return Direction{where.x - other.x, where.y - other.y}
}

func (where Location) Add(d Direction) Location {
	return Location{where.x + d.x, where.y + d.y}
}

func (loc Location) IsIn(height int, width int) bool {
	return loc.x >= 0 && loc.y >= 0 && loc.x < height && loc.y < width
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc8.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatalf("Couldn't open file: %v", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	antennas := make(map[rune][]Location)
	height := 0
	width := 0
	for scanner.Scan() {
		line := scanner.Text()
		for col, spot := range line {
			if spot != '.' {
				if antennas[spot] == nil {
					antennas[spot] = make([]Location, 0, 10)
				}
				antennas[spot] = append(antennas[spot], Location{height, col})
			}
			if col+1 > width {
				width = col + 1
			}
		}
		height += 1
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("Error scanning: %v", err)
	}
	antinodes := make(map[Location]bool)
	for _, antennaList := range antennas {
		for highIdx := range antennaList {
			for lowIdx := range highIdx {
				anti := antennaList[highIdx].Add(antennaList[highIdx].Diff(antennaList[lowIdx]))
				if anti.IsIn(height, width) {
					antinodes[anti] = true
				}
				anti = antennaList[lowIdx].Add(antennaList[lowIdx].Diff(antennaList[highIdx]))
				if anti.IsIn(height, width) {
					antinodes[anti] = true
				}
			}
		}
	}
	fmt.Printf("Part 1: %d\n", len(antinodes))

	antinodes = make(map[Location]bool)
	for _, antennaList := range antennas {
		for highIdx := range antennaList {
			for lowIdx := range highIdx {
				andiff := antennaList[highIdx].Diff(antennaList[lowIdx])
				anti := antennaList[highIdx]
				for anti.IsIn(height, width) {
					antinodes[anti] = true
					anti = anti.Add(andiff)
				}
				andiff = antennaList[lowIdx].Diff(antennaList[highIdx])
				anti = antennaList[lowIdx]
				for anti.IsIn(height, width) {
					antinodes[anti] = true
					anti = anti.Add(andiff)
				}
			}
		}
	}
	fmt.Printf("Part 2: %d\n", len(antinodes))
}
