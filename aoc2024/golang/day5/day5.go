package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func sortBook(rules map[string]bool, book []string) {
	for idx1 := range book {
		for idx2 := idx1; idx2 < len(book); idx2++ {
			if rules[book[idx2]+"|"+book[idx1]] {
				book[idx1], book[idx2] = book[idx2], book[idx1]
			}
		}
	}
}

func checkBook(rules map[string]bool, book []string) bool {
	for idx1 := range book {
		for idx2 := idx1; idx2 < len(book); idx2++ {
			if rules[book[idx2]+"|"+book[idx1]] {
				return false
			}
		}
	}
	return true
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc5.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatalf("Couldn't open file: %v", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	rules := make(map[string]bool)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			break
		}
		rules[line] = true
	}
	if err := scanner.Err(); err != nil {
		log.Fatalf("Error scanning: %v", err)
	}
	total1 := 0
	total2 := 0
	for scanner.Scan() {
		bookline := scanner.Text()
		book := strings.Split(bookline, ",")
		isGood := checkBook(rules, book)
		if !isGood {
			sortBook(rules, book)
		}
		r, e := strconv.Atoi(book[len(book)/2])
		if e != nil {
			log.Fatalf("Couldn't convert number %v: %v", book[len(book)/2], err)
		}
		if isGood {
			total1 += r
		} else {
			total2 += r
		}
	}
	fmt.Println("Part 1:", total1)
	fmt.Println("Part 2:", total2)
}
