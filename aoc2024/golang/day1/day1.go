package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"strconv"
)

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc1.in"
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

	numFinder := regexp.MustCompile(`(\d+)\s+(\d+)`)
	var row1 []int64
	var row2 []int64
	for _, line := range rawlines {
		for _, m := range numFinder.FindAllStringSubmatch(line, -1) {
			p1, e1 := strconv.ParseInt(m[1], 10, 64)
			p2, e2 := strconv.ParseInt(m[2], 10, 64)
			if e1 != nil {
				log.Fatal("Couldn't read number ", m[1])
			}
			if e2 != nil {
				log.Fatal("Couldn't read number ", m[2])
			}
			row1 = append(row1, p1)
			row2 = append(row2, p2)
		}
	}
	slices.Sort(row1)
	slices.Sort(row2)
	total := int64(0)
	for idx := range row1 {
		if row1[idx] < row2[idx] {
			total += row2[idx] - row1[idx]
		} else {
			total += row1[idx] - row2[idx]
		}
	}
	fmt.Println("Part 1:", total)

	total = 0
	for _, val1 := range row1 {
		for _, val2 := range row2 {
			if val1 == val2 {
				total += val1
			}
		}
	}
	fmt.Println("Part 2:", total)
}
