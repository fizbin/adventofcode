package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

func part1Sum(low string, high string) uint64 {
	var total uint64 = 0
	var xpart uint64

	lown, _ := strconv.ParseUint(low, 10, 64)
	highn, _ := strconv.ParseUint(high, 10, 64)
	if len(low) <= 1 {
		xpart = 1
	} else {
		xpart, _ = strconv.ParseUint(low[0:len(low)/2], 10, 64)
	}
	for {
		s := strconv.FormatUint(xpart, 10)
		x, _ := strconv.ParseUint(s+s, 10, 64)
		if x > highn {
			return total
		}
		if x >= lown {
			total += x
		}
		xpart += 1
	}
}

func part2Sum(low string, high string) uint64 {
	var total uint64 = 0
	var xpart uint64
	badIds := map[uint64]bool{}

	lown, _ := strconv.ParseUint(low, 10, 64)
	highn, _ := strconv.ParseUint(high, 10, 64)
	for repc := 2; repc <= len(high); repc++ {
		if len(low) < repc {
			xpart = 1
		} else {
			xpart, _ = strconv.ParseUint(low[0:len(low)/repc], 10, 64)
		}
		for {
			s := strconv.FormatUint(xpart, 10)
			x, _ := strconv.ParseUint(strings.Repeat(s, repc), 10, 64)
			if x > highn {
				break
			}
			if x >= lown {
				badIds[x] = true
			}
			xpart += 1
		}
	}
	for idVal, _ := range badIds {
		total += idVal
	}
	return total
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc2.in"
	}

	file, err := os.ReadFile(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	prsr := regexp.MustCompile(`(\d+)-(\d+)`)
	ans1 := uint64(0)
	ans2 := uint64(0)
	for _, line := range prsr.FindAllStringSubmatch(string(file), -1) {
		low := line[1]
		high := line[2]
		ans1 += part1Sum(low, high)
		ans2 += part2Sum(low, high)
	}
	fmt.Println("Part 1:", ans1)
	fmt.Println("Part 2:", ans2)
}
