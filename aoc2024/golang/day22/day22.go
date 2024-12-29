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
	total2 := uint16(0)
	vndrNum := uint16(0)
	bananaTotal := make([]uint16, 1<<20)
	vndrWritten := make([]uint16, 1<<20)
	for scanner.Scan() {
		vndrNum++
		n0, _ := strconv.ParseUint(scanner.Text(), 10, 64)
		n := n0
		hist := uint32(0)
		prev := uint16(n % 10)
		for range 2000 {
			n = advance(n)
			hist = ((hist & ((1 << 15) - 1)) << 5) | uint32(10+uint16(n%10)-prev)
			prev = uint16(n % 10)
			if hist >= (1 << 15) {
				if vndrWritten[hist] < vndrNum {
					nval := bananaTotal[hist] + prev
					total2 = max(total2, nval)
					bananaTotal[hist] = nval
					vndrWritten[hist] = vndrNum
				}
			}
		}
		total1 += n
	}
	fmt.Println("Part 1:", total1)
	fmt.Println("Part 2:", total2)
}
