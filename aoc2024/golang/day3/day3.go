package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	// "slices"
	// "strconv"
	// "strings"
)

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc3.in"
	} else {
		infile = argsWithoutProg[0]
	}

	data, err := os.ReadFile(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}

	mulFinder := regexp.MustCompile(`mul\((\d+), *(\d+)\)`)
	total := 0
	for _, m := range mulFinder.FindAllSubmatch(data, -1) {
		n1, _ := strconv.Atoi(string(m[1]))
		n2, _ := strconv.Atoi(string(m[2]))
		total += n1 * n2
	}
	fmt.Println("Part 1:", total)

	mulFinder = regexp.MustCompile(`(do\(\))|(don't\(\))|mul\((\d+), *(\d+)\)`)
	total = 0
	enabled := true
	for _, m := range mulFinder.FindAllSubmatch(data, -1) {
		if m[1] != nil {
			enabled = true
		} else if m[2] != nil {
			enabled = false
		} else if enabled {
			n1, _ := strconv.Atoi(string(m[3]))
			n2, _ := strconv.Atoi(string(m[4]))
			total += n1 * n2
		}
	}
	fmt.Println("Part 2:", total)
}
