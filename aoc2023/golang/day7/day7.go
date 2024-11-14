package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"sort"
	"strconv"
	"strings"
)

type HandKind int

const (
	HighCard = iota
	OnePair
	TwoPair
	ThreeKind
	FullHouse
	FourKind
	FiveKind
)

func toNumber(thing string) int {
	r, e := strconv.Atoi(thing)
	if e != nil {
		log.Fatal("Couldn't parse number: ", thing)
	}
	return r
}

func cardValue(card rune, isPartTwo bool) int {
	if isPartTwo && card == 'J' {
		return 1
	}
	switch card {
	case '2':
		return 2
	case '3':
		return 3
	case '4':
		return 4
	case '5':
		return 5
	case '6':
		return 6
	case '7':
		return 7
	case '8':
		return 8
	case '9':
		return 9
	case 'T':
		return 10
	case 'J':
		return 11
	case 'Q':
		return 12
	case 'K':
		return 13
	case 'A':
		return 14
	}
	log.Fatal("Couldn't value card", card)
	return 0
}

func computeHandKind(hand string) HandKind {
	accum := make(map[rune]int)
	for _, v := range hand {
		curval, ok := accum[v]
		if ok {
			accum[v] = curval + 1
		} else {
			accum[v] = 1
		}
	}
	var retval []int
	for _, v := range accum {
		retval = append(retval, v)
	}
	sort.Ints(retval)
	if slices.Equal(retval, []int{5}) {
		return FiveKind
	}
	if slices.Equal(retval, []int{1, 4}) {
		return FourKind
	}
	if slices.Equal(retval, []int{2, 3}) {
		return FullHouse
	}
	if slices.Equal(retval, []int{1, 1, 3}) {
		return ThreeKind
	}
	if slices.Equal(retval, []int{1, 2, 2}) {
		return TwoPair
	}
	if slices.Equal(retval, []int{1, 1, 1, 2}) {
		return OnePair
	}
	return HighCard
}

func computeHandValue(hand string, isPartTwo bool) int {
	val := 0
	if isPartTwo {
		for _, repChar := range "23456789TQKA" {
			hand2 := strings.ReplaceAll(hand, "J", string(repChar))
			val2 := int(computeHandKind(hand2))
			if val < val2 {
				val = val2
			}
		}
	} else {
		val = int(computeHandKind(hand))
	}
	for _, card := range hand {
		val *= 100
		val += cardValue(card, isPartTwo)
	}
	return val
}

type handRec struct {
	hand      string
	handValue int
	handBid   int
}

type handRecSlice []handRec

func (hrs handRecSlice) Len() int {
	return len(hrs)
}
func (hrs handRecSlice) Swap(i, j int) {
	hrs[i], hrs[j] = hrs[j], hrs[i]
}
func (is handRecSlice) Less(i, j int) bool {
	return is[i].handValue < is[j].handValue
}

func main() {
	file, err := os.Open("day7/day7.in")
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

	stuffFinder := regexp.MustCompile(`\S+`)
	var allHands handRecSlice
	for _, line := range rawlines {
		lineparts := stuffFinder.FindAllString(line, -1)
		hand := lineparts[0]
		handBid := toNumber(lineparts[1])
		allHands = append(allHands, handRec{hand, computeHandValue(hand, false), handBid})
	}
	sort.Sort(allHands)
	total := 0
	for i := 0; i < len(allHands); i++ {
		total += (i + 1) * allHands[i].handBid
	}
	fmt.Printf("Part 1: %d\n", total)

	for idx := range allHands {
		allHands[idx].handValue = computeHandValue(allHands[idx].hand, true)
	}
	sort.Sort(allHands)
	total = 0
	for i := 0; i < len(allHands); i++ {
		total += (i + 1) * allHands[i].handBid
	}
	fmt.Printf("Part 2: %d\n", total)
}
