package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func isAt(data [][]byte, x int, y int) bool {
	if (0 <= x) && (0 <= y) && (x < len(data)) && (y < len(data[0])) && (data[x][y] == '@') {
		return true
	}
	return false
}

func part1(data [][]byte) int {
	ans := 0
	for x, row := range data {
		for y, ch := range row {
			if ch == '@' {
				s := 0
				for xoff := -1; xoff < 2; xoff++ {
					for yoff := -1; yoff < 2; yoff++ {
						if isAt(data, x+xoff, y+yoff) {
							s++
						}
					}
				}
				if s < 5 { // remember to include the center in the limit
					ans++
				}
			}
		}
	}
	return ans
}

func part2(data [][]byte) int {
	ans := 0
	keepGoing := true
	for keepGoing {
		keepGoing = false
		for x, row := range data {
			for y, ch := range row {
				if ch == '@' {
					s := 0
					for xoff := -1; xoff < 2; xoff++ {
						for yoff := -1; yoff < 2; yoff++ {
							if isAt(data, x+xoff, y+yoff) {
								s++
							}
						}
					}
					if s < 5 { // remember to include the center in the limit
						ans++
						data[x][y] = 'x'
						keepGoing = true
					}
				}
			}
		}
	}
	return ans
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc4.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var rawlines [][]byte
	for scanner.Scan() {
		rawlines = append(rawlines, []byte(scanner.Text()))
	}
	fmt.Println("Part 1:", part1(rawlines))
	fmt.Println("Part 2:", part2(rawlines))
}
