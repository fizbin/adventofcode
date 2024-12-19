package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func findTowelCombinations(cache map[string]uint64, towels []string, tgt string) uint64 {
	if tgt == "" {
		return 1
	}
	ans, ok := cache[tgt]
	if !ok {
		for _, towel := range towels {
			if strings.HasPrefix(tgt, towel) {
				ans += findTowelCombinations(cache, towels, tgt[len(towel):])
			}
		}
		cache[tgt] = ans
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
	scanner.Scan()
	// blank

	total1 := 0
	total2 := uint64(0)
	for scanner.Scan() {
		step := findTowelCombinations(make(map[string]uint64), towels, scanner.Text())
		if step > 0 {
			total1++
		}
		total2 += step
	}
	fmt.Println("Part 1:", total1)
	fmt.Println("Part 2:", total2)
}
