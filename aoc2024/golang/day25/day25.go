package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
)

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc25.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	keys := make([][]int, 0)
	locks := make([][]int, 0)

	allPound := regexp.MustCompile(`^#+$`)
	working := make([]int, 0)
	isLock := false
	for scanner.Scan() {
		if scanner.Text() == "" {
			if isLock {
				locks = append(locks, working)
			} else {
				keys = append(keys, working)
			}
			working = make([]int, 0, len(working))
		} else if len(working) == 0 {
			isLock = allPound.MatchString(scanner.Text())
			for _, ch := range scanner.Text() {
				if ch == '#' {
					working = append(working, 1)
				} else {
					working = append(working, 0)
				}
			}
		} else {
			for idx, ch := range scanner.Text() {
				if ch == '#' {
					working[idx]++
				}
			}
		}
	}
	if len(working) > 0 {
		if isLock {
			locks = append(locks, working)
		} else {
			keys = append(keys, working)
		}
	}

	total1 := 0
	for _, lock := range locks {
		for _, key := range keys {
			if fits(lock, key) {
				total1++
			}
		}
	}
	fmt.Println("Part 1:", total1)
}

func fits(lock []int, key []int) bool {
	for idx := range lock {
		if lock[idx]+key[idx] >= 8 {
			return false
		}
	}
	return true
}
