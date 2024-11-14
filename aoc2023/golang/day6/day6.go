package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"math/big"
	"os"
	"regexp"
	"strconv"
)

func toNumber(thing string) int {
	r, e := strconv.Atoi(thing)
	if e != nil {
		log.Fatal("Couldn't parse number: ", thing)
	}
	return r
}

func main() {
	file, err := os.Open("day6/day6.in")
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
	var times []int
	var distances []int
	for _, v := range numfinder.FindAllString(rawlines[0], -1) {
		times = append(times, toNumber(v))
	}
	for _, v := range numfinder.FindAllString(rawlines[1], -1) {
		distances = append(distances, toNumber(v))
	}

	wayCombos := 1

	for idx := 0; idx < len(times); idx++ {
		// dist = (t - h) * h
		// dist = th - h^2
		// h^2 - th + dist = 0
		// (t +/- sqrt(t^2 - 4*dist))/2
		guess := int(math.Floor((float64(times[idx])-(math.Sqrt(float64(times[idx]*times[idx])-4.0*float64(distances[idx]))))/2.0)) - 1
		for guess*(times[idx]-guess) < distances[idx] {
			guess += 1
		}
		nworkings := times[idx] - 2*guess + 1
		wayCombos *= nworkings
	}
	fmt.Printf("Part 1: %d\n", wayCombos)

	bigtimeStr := ""
	for _, v := range numfinder.FindAllString(rawlines[0], -1) {
		bigtimeStr += v
	}
	bigdistStr := ""
	for _, v := range numfinder.FindAllString(rawlines[1], -1) {
		bigdistStr += v
	}

	bigtime := big.NewInt(0)
	_, ok := bigtime.SetString(bigtimeStr, 10)
	if !ok {
		log.Fatal("Couldn't convert string ", bigtimeStr)
	}

	bigdist := big.NewInt(0)
	_, ok = bigdist.SetString(bigdistStr, 10)
	if !ok {
		log.Fatal("Couldn't convert string ", bigdistStr)
	}

	scratch := big.NewInt(0)
	scratch.Mul(bigtime, bigtime)
	scratch.Sub(scratch, bigdist)
	scratch.Sub(scratch, bigdist)
	scratch.Sub(scratch, bigdist)
	scratch.Sub(scratch, bigdist)
	scratch.Sqrt(scratch)
	scratch.Sub(bigtime, scratch)
	scratch.Quo(scratch, big.NewInt(2))
	scratch.Sub(scratch, big.NewInt(5))
	guess := big.NewInt(0)
	guess.Set(scratch)
	scratch.SetInt64(0)
	for scratch.Cmp(bigdist) < 0 {
		guess.Add(guess, big.NewInt(1))
		scratch.Sub(bigtime, guess)
		scratch.Mul(guess, scratch)
	}
	scratch.Mul(guess, big.NewInt(2))
	scratch.Sub(bigtime, scratch)
	scratch.Add(scratch, big.NewInt(1))
	fmt.Printf("Part 2: %v\n", scratch)
}
