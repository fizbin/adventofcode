package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func advance(n uint64) uint64 {
	n = n ^ (n * 64)
	n %= 16777216
	n = n ^ (n / 32)
	n %= 16777216
	n = n ^ (n * 2048)
	n %= 16777216
	return n
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc22.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	total1 := uint64(0)
	total2 := 0
	bananaStrategyMap := make(map[[4]int]int)
	for scanner.Scan() {
		myBananas := make(map[[4]int]int)
		n0, _ := strconv.ParseUint(scanner.Text(), 10, 64)
		n := n0
		hist := make([]int, 0, 2002)
		prev := int(n % 10)
		for range 2000 {
			n = advance(n)
			hist = append(hist, int(n%10)-prev)
			prev = int(n % 10)
			if len(hist) > 3 {
				histKey := ([4]int)(hist[0:4])
				if _, ok := myBananas[histKey]; !ok {
					myBananas[histKey] = prev
				}
				hist = hist[1:]
			}
		}
		for k, v := range myBananas {
			bananaStrategyMap[k] += v
			if bananaStrategyMap[k] > total2 {
				total2 = bananaStrategyMap[k]
			}
		}
		total1 += n
	}
	fmt.Println("Part 1:", total1)
	fmt.Println("Part 2:", total2)
}
