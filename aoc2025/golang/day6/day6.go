package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

func part1(data [][]byte) uint64 {
	numdata := make([][]uint64, 0, 10)
	ansdata := make([]uint64, 0)
	digitFinder := regexp.MustCompile(`\d+`)
	for _, line := range data {
		nums := make([]uint64, 0)
		for _, val := range digitFinder.FindAll(line, -1) {
			n, _ := strconv.ParseUint(string(val), 10, 64)
			nums = append(nums, n)
		}
		if len(nums) == 0 {
			// hit last line without numbers
			opFinder := regexp.MustCompile(`[*+]`)
			for idx, val := range opFinder.FindAll(line, -1) {
				ans := uint64(0)
				switch val[0] {
				case '*':
					ans += 1
					for _, nums := range numdata {
						ans *= nums[idx]
					}
				case '+':
					for _, nums := range numdata {
						ans += nums[idx]
					}
				}
				ansdata = append(ansdata, ans)
			}
		} else {
			numdata = append(numdata, nums)
		}
	}
	finalAnswer := uint64(0)
	for _, v := range ansdata {
		finalAnswer += v
	}
	return finalAnswer
}

func part2(data [][]byte) uint64 {
	parser := regexp.MustCompile(`(^\s*$)|(?:^\s*(\d+)\s*$)|(?:^\s*(\d+)\s*([+*])\s*$)`)
	ansdata := make([]uint64, 0)
	curdata := make([]uint64, 0)
	for colidx := len(data[0]); colidx > 0; colidx-- {
		line := make([]byte, 0, len(data))
		for _, row := range data {
			line = append(line, row[colidx-1])
		}
		m := parser.FindSubmatch(line)
		if m[1] != nil {
			continue
		}
		if m[2] != nil {
			n, _ := strconv.ParseUint(string(m[2]), 10, 64)
			curdata = append(curdata, n)
		}
		if m[3] != nil {
			n, _ := strconv.ParseUint(string(m[3]), 10, 64)
			curdata = append(curdata, n)
		}
		if m[4] != nil {
			ans := uint64(0)
			switch m[4][0] {
			case '*':
				ans += 1
				for _, n := range curdata {
					ans *= n
				}
			case '+':
				for _, n := range curdata {
					ans += n
				}
			}
			ansdata = append(ansdata, ans)
			curdata = make([]uint64, 0, len(curdata))
		}
	}

	finalAnswer := uint64(0)
	for _, v := range ansdata {
		finalAnswer += v
	}
	return finalAnswer
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc6.in"
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
