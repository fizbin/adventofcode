package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"strings"
)

func main() {
	file, err := os.Open("day4/day4.in")
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
	numfinder := regexp.MustCompile(`\d+`)
	var nWinning []int
	for _, line := range rawlines {
		stuff := strings.Split(line, ":")
		parts := strings.Split(stuff[1], "|")
		winning := numfinder.FindAllString(parts[0], -1)
		haveValues := numfinder.FindAllString(parts[1], -1)
		wins := 0
		for _, v := range haveValues {
			if slices.Contains(winning, v) {
				wins += 1
			}
		}
		nWinning = append(nWinning, wins)
	}

	part1Sum := 0
	for _, winCount := range nWinning {
		if winCount > 0 {
			part1Sum += 1 << (winCount - 1)
		}
	}
	fmt.Printf("Part 1: %d\n", part1Sum)

	var nCards []uint64
	for i := 0; i < len(nWinning); i++ {
		nCards = append(nCards, 1)
	}

	var cardTotal uint64 = 0
	for gameIdx, wins := range nWinning {
		myCards := nCards[gameIdx]
		cardTotal += myCards
		for targetIdx := gameIdx + 1; targetIdx <= gameIdx+wins; targetIdx++ {
			nCards[targetIdx] += myCards
		}
	}

	fmt.Printf("Part 2: %d\n", cardTotal)
}
