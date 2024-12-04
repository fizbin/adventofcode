package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func spot(lines []string, x int, y int) byte {
	if (x < 0) || (y < 0) {
		return ' '
	}
	if x >= len(lines) {
		return ' '
	}
	if y >= len(lines[x]) {
		return ' '
	}
	return lines[x][y]
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc4.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var rawlines []string
	for scanner.Scan() {
		rawlines = append(rawlines, scanner.Text())
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning: {}", err)
	}

	directions := [][]int{{1, 0}, {0, 1}, {-1, 0}, {0, -1}, {1, 1}, {-1, -1}, {1, -1}, {-1, 1}}

	var total int
	for x, row := range rawlines {
		for y, val := range row {
			if val == 'X' {
				for _, mydir := range directions {
					l1 := spot(rawlines, x+mydir[0], y+mydir[1])
					l2 := spot(rawlines, x+2*mydir[0], y+2*mydir[1])
					l3 := spot(rawlines, x+3*mydir[0], y+3*mydir[1])
					if (l1 == 'M') && (l2 == 'A') && (l3 == 'S') {
						total += 1
					}
				}
			}
		}
	}

	fmt.Println("Part 1:", total)

	total = 0
	for x, row := range rawlines {
		for y, val := range row {
			if val == 'A' {
				l1 := spot(rawlines, x+1, y+1)
				l2 := spot(rawlines, x-1, y-1)
				l3 := spot(rawlines, x-1, y+1)
				l4 := spot(rawlines, x+1, y-1)
				if ((l1 == 'M') && (l2 == 'S')) || ((l1 == 'S') && (l2 == 'M')) {
					if ((l3 == 'M') && (l4 == 'S')) || ((l3 == 'S') && (l4 == 'M')) {
						total += 1
					}
				}
			}
		}
	}
	fmt.Println("Part 2:", total)
}
