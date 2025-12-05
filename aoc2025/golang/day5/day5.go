package main

import (
	"bufio"
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
		infile = "../aoc5.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	ranges := make([][]uint64, 0, 100)
	rangeParser := regexp.MustCompile(`(\d+)-(\d+)`)
	for scanner.Scan() {
		m := rangeParser.FindStringSubmatch(scanner.Text())
		if m == nil {
			break
		}
		rg := make([]uint64, 2)
		rg[0], _ = strconv.ParseUint(m[1], 10, 64)
		rg[1], _ = strconv.ParseUint(m[2], 10, 64)
		ranges = append(ranges, rg)
	}
	idVals := make([]uint64, 0, 100)
	for scanner.Scan() {
		idVal, _ := strconv.ParseUint(scanner.Text(), 10, 64)
		idVals = append(idVals, idVal)
	}
	ans1 := 0
	for _, idVal := range idVals {
		for _, rg := range ranges {
			if (idVal >= rg[0]) && (idVal <= rg[1]) {
				ans1++
				break
			}
		}
	}
	println(ans1)
	slices.SortFunc(ranges, func(a, b []uint64) int {
		if a[0] < b[0] {
			return -1
		}
		if a[0] > b[0] {
			return 1
		}
		if a[1] < b[1] {
			return -1
		}
		if a[1] > b[1] {
			return 1
		}
		return 0
	})
	ans2 := uint64(0)
	curRange := slices.Clone(ranges[0])
	for _, rg := range ranges {
		if curRange[1]+1 < rg[0] {
			ans2 += curRange[1] - curRange[0] + 1
			curRange[0], curRange[1] = rg[0], rg[1]
		} else {
			curRange[1] = max(rg[1], curRange[1])
		}
	}
	ans2 += curRange[1] - curRange[0] + 1
	println(ans2)
}
