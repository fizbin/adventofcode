package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func advanceNum(n int) []int {
	if n == 0 {
		return []int{1}
	}
	s := strconv.Itoa(n)
	if len(s)%2 == 0 {
		a1, _ := strconv.Atoi(s[0 : len(s)/2])
		a2, _ := strconv.Atoi(s[len(s)/2:])
		return []int{a1, a2}
	}
	return []int{n * 2024}
}

func advanceMap(in map[int]uint64) map[int]uint64 {
	retval := make(map[int]uint64)
	for k, v := range in {
		for _, newk := range advanceNum(k) {
			retval[newk] = retval[newk] + v
		}
	}
	return retval
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc11.in"
	} else {
		infile = argsWithoutProg[0]
	}

	data, err := os.ReadFile(infile)
	if err != nil {
		log.Fatal("Couldn't open file: ", err)
	}
	nums := make([]int, 0)
	for _, numStr := range strings.Fields(string(data)) {
		n, e := strconv.Atoi(numStr)
		if e != nil {
			log.Fatalf("Couldn't parse '%s'", numStr)
		}
		nums = append(nums, n)
	}
	working := make(map[int]uint64)
	for _, n := range nums {
		working[n] = 1
	}
	for range 25 {
		working = advanceMap(working)
	}
	total := uint64(0)
	for _, v := range working {
		total += v
	}
	fmt.Println("Part 1:", total)

	for range 50 {
		working = advanceMap(working)
	}
	total = 0
	for _, v := range working {
		total += v
	}
	fmt.Println("Part 2:", total)
}
