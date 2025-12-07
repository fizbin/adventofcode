package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func part1(data [][]byte) uint64 {
	tachys := make(map[int]bool)
	ans := uint64(0)
	for _, row := range data {
		newtachys := make(map[int]bool)
		for chidx, ch := range row {
			switch ch {
			case '.':
				if tachys[chidx] {
					newtachys[chidx] = true
				}
			case '^':
				if tachys[chidx] {
					newtachys[chidx-1] = true
					newtachys[chidx+1] = true
					ans++
				}
			case 'S':
				newtachys[chidx] = true
			}
		}
		tachys = newtachys
	}
	return ans
}

func part2(data [][]byte) uint64 {
	tachys := make(map[int]uint64)
	for _, row := range data {
		newtachys := make(map[int]uint64)
		for chidx, ch := range row {
			switch ch {
			case '.':
				newtachys[chidx] += tachys[chidx]
			case '^':
				newtachys[chidx-1] += tachys[chidx]
				newtachys[chidx+1] += tachys[chidx]
			case 'S':
				newtachys[chidx] = 1
			}
		}
		tachys = newtachys
	}
	ans := uint64(0)
	for _, v := range tachys {
		ans += v
	}
	return ans
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc7.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var rawlines [][]byte
	for scanner.Scan() {
		rawlines = append(rawlines, []byte(scanner.Text()))
	}
	fmt.Println(part1(rawlines))
	fmt.Println(part2(rawlines))
}
