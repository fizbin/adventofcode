package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func findTowelCombinations(towels map[string]bool, tgt string) uint64 {
	cache := make([]uint64, len(tgt)+1)
	cache[0] = 1
	var ans uint64
	for hi := 1; hi < len(tgt)+1; hi++ {
		ans = 0
		for lo := 0; lo < hi; lo++ {
			if towels[tgt[lo:hi]] {
				ans += cache[lo]
			}
		}
		cache[hi] = ans
	}
	return ans
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc19.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	// first line
	scanner.Scan()
	towelstr := scanner.Text()
	towels := strings.Split(towelstr, ", ")

	towelSet := make(map[string]bool)
	for _, t := range towels {
		towelSet[t] = true
	}
	scanner.Scan()
	// blank

	total1 := 0
	total2 := uint64(0)
	for scanner.Scan() {
		step := findTowelCombinations(towelSet, scanner.Text())
		if step > 0 {
			total1++
		}
		total2 += step
	}
	fmt.Println("Part 1:", total1)
	fmt.Println("Part 2:", total2)
}
