package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

func isSafe(x []int) bool {
	negOk := true
	posOk := true
	pval := x[0]
	for _, val := range x[1:] {
		c := val - pval
		if (c < -3) || (c > -1) {
			negOk = false
		}
		if (c > 3) || (c < 1) {
			posOk = false
		}
		pval = val
	}
	return (negOk || posOk)
}

func isSafe2(x []int) bool {
	if isSafe(x) {
		return true
	}
	for idx := range x {
		y := slices.Clone[[]int](x)
		if isSafe(slices.Delete(y, idx, idx+1)) {
			return true
		}
	}
	return false
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc2.in"
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

	nsafe1 := 0
	nsafe2 := 0
	for _, line := range rawlines {
		nums := make([]int, 0, 10)
		for _, fld := range strings.Fields(line) {
			p1, e1 := strconv.Atoi(fld)
			if e1 != nil {
				log.Fatal("Couldn't read number ", fld)
			}
			nums = append(nums, p1)
		}
		if isSafe(nums) {
			nsafe1 += 1
		}
		if isSafe2(nums) {
			nsafe2 += 1
		}
	}
	fmt.Println("Part 1:", nsafe1)
	fmt.Println("Part 2:", nsafe2)
}
