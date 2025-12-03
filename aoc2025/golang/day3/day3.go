package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
)

func part1(line string) uint64 {
	var ans uint64 = 0

	for first_idx := 0; first_idx < len(line)-1; first_idx++ {
		for second_idx := first_idx + 1; second_idx < len(line); second_idx++ {
			x := uint64(line[first_idx]-'0')*10 + uint64(line[second_idx]-'0')
			ans = uint64(max(ans, x))
		}
	}
	return ans
}

func part2_piece(pref uint64, n int, start_idx int, line string) uint64 {
	var ans uint64 = 0
	if (n == 0) || (start_idx >= len(line)) {
		return 0
	}
	max_dig := slices.Max([]byte(line[start_idx : len(line)-n+1]))
	for idx := start_idx; idx < len(line)-n+1; idx++ {
		if line[idx] == max_dig {
			new_pref := pref*10 + uint64(max_dig-'0')
			if n == 1 {
				ans = max(ans, new_pref)
			} else {
				ans = max(ans, part2_piece(new_pref, n-1, idx+1, line))
			}
		}
	}
	return ans
}

func part2(line string) uint64 {
	return part2_piece(0, 12, 0, line)
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc3.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var rawlines []string
	for scanner.Scan() {
		rawlines = append(rawlines, scanner.Text())
	}
	ans1 := uint64(0)
	ans2 := uint64(0)
	for _, line := range rawlines {
		ans1 += part1(line)
		ans2 += part2(line)
	}
	fmt.Println("Part 1:", ans1)
	fmt.Println("Part 2:", ans2)
}
