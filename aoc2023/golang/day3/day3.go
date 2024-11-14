package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("day3/day3.in")
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

	var lines []string
	lines = append(lines, strings.Repeat(".", len(rawlines[0])+2))
	for _, v := range rawlines {
		lines = append(lines, "."+v+".")
	}
	lines = append(lines, strings.Repeat(".", len(rawlines[0])+2))

	partsum := 0
	numfinder := regexp.MustCompile(`\d+`)
	symfinder := regexp.MustCompile(`[^0-9.\s]`)
	starfinder := regexp.MustCompile(`[*]`)
	starparts := make(map[[2]int][]int)

	for rowidx, row := range lines {
		match := numfinder.FindAllStringIndex(row, -1)
		if len(match) == 0 {
			continue
		}
		for _, m := range match {
			surrounding := lines[rowidx-1][m[0]-1:m[1]+1] + row[m[0]-1:m[0]] + row[m[1]:m[1]+1] + lines[rowidx+1][m[0]-1:m[1]+1]
			if symfinder.MatchString(surrounding) {
				n, e := strconv.Atoi(row[m[0]:m[1]])
				if e != nil {
					log.Fatal("Couldn't convert number")
				}
				partsum += n
				for _, staridxPair := range starfinder.FindAllStringIndex(lines[rowidx-1][m[0]-1:m[1]+1], -1) {
					col := m[0] - 1 + staridxPair[0]
					prevlist, ok := starparts[[2]int{rowidx - 1, col}]
					if ok {
						prevlist = append(prevlist, n)
					} else {
						prevlist = []int{n}
					}
					starparts[[2]int{rowidx - 1, col}] = prevlist
				}
				for _, staridxPair := range starfinder.FindAllStringIndex(lines[rowidx+1][m[0]-1:m[1]+1], -1) {
					col := m[0] - 1 + staridxPair[0]
					prevlist, ok := starparts[[2]int{rowidx + 1, col}]
					if ok {
						prevlist = append(prevlist, n)
					} else {
						prevlist = []int{n}
					}
					starparts[[2]int{rowidx + 1, col}] = prevlist
				}
				if row[m[0]-1] == '*' {
					prevlist, ok := starparts[[2]int{rowidx, m[0] - 1}]
					if ok {
						prevlist = append(prevlist, n)
					} else {
						prevlist = []int{n}
					}
					starparts[[2]int{rowidx, m[0] - 1}] = prevlist
				}
				if row[m[1]] == '*' {
					prevlist, ok := starparts[[2]int{rowidx, m[1]}]
					if ok {
						prevlist = append(prevlist, n)
					} else {
						prevlist = []int{n}
					}
					starparts[[2]int{rowidx, m[1]}] = prevlist
				}
			}
		}
	}

	gearsum := 0
	for _, partlist := range starparts {
		if len(partlist) == 2 {
			gearsum += partlist[0] * partlist[1]
		}
	}

	fmt.Printf("Part 1: %d\n", partsum)
	fmt.Printf("Part 2: %d\n", gearsum)
}
