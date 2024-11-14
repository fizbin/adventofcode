package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"strconv"
)

type Range struct {
	start  int
	length int
}

func toNumber(thing string) int {
	r, e := strconv.Atoi(thing)
	if e != nil {
		log.Fatal("Couldn't parse number: ", thing)
	}
	return r
}

func mapNumber(thing int, mapTo int, mapFrom int, mapLen int) (int, bool) {
	if thing >= mapFrom && thing < mapFrom+mapLen {
		return thing + mapTo - mapFrom, true
	}
	return 0, false
}

func rangeIntersection(a Range, b Range) []Range {
	if a.start+a.length <= b.start {
		return []Range{}
	}
	if b.start+b.length <= a.start {
		return []Range{}
	}
	start := slices.Max([]int{a.start, b.start})
	end := slices.Min([]int{a.start + a.length, b.start + b.length})
	return []Range{{start, end - start}}
}

// return changed, unchanged
func mapRange(thing Range, mapTo int, mapFrom int, mapLen int) ([]Range, []Range) {
	unmapped := append(rangeIntersection(thing, Range{0, mapFrom}),
		rangeIntersection(thing, Range{mapFrom + mapLen, mapFrom + mapLen + thing.length + thing.start})...)
	var mapped []Range
	for _, v := range rangeIntersection(thing, Range{mapFrom, mapLen}) {
		mapped = append(mapped, Range{v.start + mapTo - mapFrom, v.length})
	}
	return mapped, unmapped
}

func main() {
	file, err := os.Open("day5/day5.in")
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
	{
		var seeds []int
		for _, seedStr := range numfinder.FindAllString(rawlines[0], -1) {
			seeds = append(seeds, toNumber(seedStr))
		}
		var newSeeds []int
		newSeeds = seeds
		seeds = make([]int, 0, len(newSeeds))
		for _, mapLine := range rawlines[1:] {
			if !numfinder.MatchString(mapLine) {
				if len(newSeeds) > 0 {
					seeds = append(seeds, newSeeds...)
					newSeeds = make([]int, 0, len(seeds))
				}
			} else {
				mapNums := numfinder.FindAllString(mapLine, -1)
				tmpSeeds := make([]int, 0, len(seeds))
				for _, seed := range seeds {
					newVal, mapped := mapNumber(seed, toNumber(mapNums[0]), toNumber(mapNums[1]), toNumber(mapNums[2]))
					if mapped {
						newSeeds = append(newSeeds, newVal)
					} else {
						tmpSeeds = append(tmpSeeds, seed)
					}
				}
				seeds = tmpSeeds
			}
		}
		seeds = append(seeds, newSeeds...)
		fmt.Printf("Part 1: %d\n", slices.Min(seeds))
	}
	numPairFinder := regexp.MustCompile(`(\d+)\s+(\d+)`)
	{
		seedRangeStrs := numPairFinder.FindAllStringSubmatch(rawlines[0], -1)
		var seeds []Range
		for _, rangeSpec := range seedRangeStrs {
			seeds = append(seeds, Range{toNumber(rangeSpec[1]), toNumber(rangeSpec[2])})
		}
		var newSeeds []Range
		newSeeds = seeds
		seeds = make([]Range, 0, len(newSeeds))
		for _, mapLine := range rawlines[1:] {
			if !numfinder.MatchString(mapLine) {
				if len(newSeeds) > 0 {
					seeds = append(seeds, newSeeds...)
					newSeeds = make([]Range, 0, len(seeds))
				}
			} else {
				mapNums := numfinder.FindAllString(mapLine, -1)
				tmpSeeds := make([]Range, 0, len(seeds))
				for _, seed := range seeds {
					mapped, unmapped := mapRange(seed, toNumber(mapNums[0]), toNumber(mapNums[1]), toNumber(mapNums[2]))
					newSeeds = append(newSeeds, mapped...)
					tmpSeeds = append(tmpSeeds, unmapped...)
				}
				seeds = tmpSeeds
			}
		}
		seeds = append(seeds, newSeeds...)
		minval := seeds[0].start
		for _, v := range seeds[1:] {
			if v.start < minval {
				minval = v.start
			}
		}
		fmt.Printf("Part 2: %d\n", minval)
	}
}
