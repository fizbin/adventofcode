package main

import (
	"bufio"
	"fmt"
	"log"
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

type GameHand struct {
	red   int
	green int
	blue  int
}

func (gh *GameHand) toMax(other GameHand) {
	if other.red > gh.red {
		gh.red = other.red
	}
	if other.blue > gh.blue {
		gh.blue = other.blue
	}
	if other.green > gh.green {
		gh.green = other.green
	}
}

func (gh GameHand) isLeq(other GameHand) bool {
	return ((gh.red <= other.red) && (gh.blue <= other.blue) && (gh.green <= other.green))
}

func (gh *GameHand) adjust(color string, val int) {
	switch color {
	case "red":
		gh.red = val
	case "green":
		gh.green = val
	case "blue":
		gh.blue = val
	default:
		log.Fatal("Invalid color ", color)
	}
}

func main() {
	file, err := os.Open("day2/day2.in")
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

	gameNumFinder := regexp.MustCompile(`Game (\d+)`)
	handFinder := regexp.MustCompile(`[:;][^:;]*`)
	valFinder := regexp.MustCompile(`(\d+) (red|green|blue)`)

	part1sum := 0
	part2sum := 0
	for _, line := range rawlines {
		gameMatch := gameNumFinder.FindStringSubmatch(line)
		if gameMatch == nil {
			log.Fatal("Bad line, no game: ", line)
		}
		gameNum := toNumber(gameMatch[1])
		gameOk := true
		gameMin := GameHand{}
		for _, handString := range handFinder.FindAllString(line, -1) {
			hand := GameHand{}
			for _, valMatch := range valFinder.FindAllStringSubmatch(handString, -1) {
				hand.adjust(valMatch[2], toNumber(valMatch[1]))
			}
			if !hand.isLeq(GameHand{12, 13, 14}) {
				gameOk = false
			}
			gameMin.toMax(hand)
		}
		if gameOk {
			part1sum += gameNum
		}
		part2sum += gameMin.red * gameMin.green * gameMin.blue
	}
	fmt.Printf("Part 1: %d\n", part1sum)
	fmt.Printf("Part 2: %d\n", part2sum)
}
