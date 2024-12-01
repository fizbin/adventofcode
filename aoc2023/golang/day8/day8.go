package main

import (
	"bufio"
	"fmt"
	"log"
	"math/big"
	"os"
	"regexp"
	"strings"
)

func lengthToEnd(start string, lefts map[string]string, rights map[string]string,
	dirlist string, atEnd func(string) bool) int {
	retval := 0
	for {
		for _, dir := range dirlist {
			switch dir {
			case 'R':
				start = rights[start]
				retval += 1
			case 'L':
				start = lefts[start]
				retval += 1
			}
			if atEnd(start) {
				return retval
			}
		}
	}
}

func main() {
	file, err := os.Open("day8/day8.in")
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

	netLineParser := regexp.MustCompile(`(\w+) *= *\((\w+), *(\w+)\)`)
	rLList := rawlines[0]
	lefts := make(map[string]string)
	rights := make(map[string]string)
	for _, line := range rawlines[2:] {
		m := netLineParser.FindStringSubmatch(line)
		lefts[m[1]] = m[2]
		rights[m[1]] = m[3]
	}
	part1 := lengthToEnd("AAA", lefts, rights, rLList, func(f string) bool { return f == "ZZZ" })
	fmt.Printf("Part 1: %d\n", part1)

	loopLengths := make([]*big.Int, 0, 10)
	for start := range lefts {
		if strings.HasSuffix(start, "A") {
			loopL := lengthToEnd(start, lefts, rights, rLList, func(f string) bool { return strings.HasSuffix(f, "Z") })
			loopLengths = append(loopLengths, big.NewInt(int64(loopL)))
		}
	}
	ans := big.NewInt(0)
	ans.Set(loopLengths[0])
	for _, loopLen := range loopLengths {
		gcd := big.NewInt(0)
		gcd.GCD(nil, nil, ans, loopLen)
		ans.Mul(ans, loopLen)
		ans.Quo(ans, gcd)
	}
	fmt.Printf("Part 2: %v\n", ans)
}
