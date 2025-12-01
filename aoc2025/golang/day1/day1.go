package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
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

	parser := regexp.MustCompile(`(\w)(\d+)`)
	ans1 := 0
	ans2 := 0
	dial := int64(50)
	for _, line := range rawlines {
		m := parser.FindStringSubmatch(line)
		dist, edist := strconv.ParseInt(m[2], 10, 64)
		if edist != nil {
			log.Fatal("Couldn't read number ", m[1])
		}
		ans2 += int(dist / 100)
		dist = dist % 100
		switch m[1] {
		case "L":
			if dial > 0 && dist > dial {
				ans2 += 1
			}
			dial = (dial - dist + 200) % 100
		case "R":
			if dial+dist > 100 {
				ans2 += 1
			}
			dial = (dial + dist) % 100
		}
		if dial == 0 {
			ans1 += 1
			ans2 += 1
		}
	}
	fmt.Println("Part 1:", ans1)
	fmt.Println("Part 2:", ans2)
}
