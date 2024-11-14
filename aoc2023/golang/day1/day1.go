package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
)

func numval(num string) int {
	switch num {
	case "one":
		return 1
	case "two":
		return 2
	case "three":
		return 3
	case "four":
		return 4
	case "five":
		return 5
	case "six":
		return 6
	case "seven":
		return 7
	case "eight":
		return 8
	case "nine":
		return 9
	}
	return int(num[0]) - 48
}

func main() {
	file, err := os.Open("day1/day1.in")
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

	numFinder := regexp.MustCompile(`\d`)
	total1 := 0
	for _, line := range rawlines {
		firstnum := -1
		lastnum := -1
		for _, m := range numFinder.FindAllString(line, -1) {
			a := int(m[0]) - 48
			if firstnum < 0 {
				firstnum = a
			}
			lastnum = a
		}
		total1 += 10*firstnum + lastnum
	}
	fmt.Printf("Part 1: %d\n", total1)

	numfinder2 := regexp.MustCompile(`\d|one|two|three|four|five|six|seven|eight|nine`)
	numfinder3 := regexp.MustCompile(`.*(\d|one|two|three|four|five|six|seven|eight|nine)`)
	total2 := 0
	for _, line := range rawlines {
		m1 := numfinder2.FindString(line)
		m2 := numfinder3.FindStringSubmatch(line)
		firstnum := numval(m1)
		lastnum := numval(m2[1])
		total2 += 10*firstnum + lastnum
	}
	fmt.Printf("Part 2: %d\n", total2)
}
