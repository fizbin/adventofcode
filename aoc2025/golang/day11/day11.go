package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"regexp"
	"strings"
)

func main() {
	flag.Parse()
	argsWithoutProg := flag.Args()
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc11.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	parser := regexp.MustCompile(`(\S+): *(.*)`)
	connections := make(map[string][]string)
	for scanner.Scan() {
		groups := parser.FindSubmatch(scanner.Bytes())
		dests := strings.Split(strings.TrimSpace(string(groups[2])), " ")
		connections[string(groups[1])] = dests
	}
	fmt.Println(part1(connections))
	fmt.Println(part2(connections))
}

func part1(connections map[string][]string) int {
	memo := make(map[string]int)
	memo["out"] = 1
	var recfunc func(string) int
	recfunc = func(a string) int {
		if ans, ok := memo[a]; ok {
			return ans
		}
		ans := 0
		for _, nxt := range connections[a] {
			ans += recfunc(nxt)
		}
		memo[a] = ans
		return ans
	}
	return recfunc("you")
}

func part2(connections map[string][]string) int {
	memo := make(map[string][4]int)
	memo["out"] = [4]int{1, 0, 0, 0}
	var recfunc func(string) [4]int
	recfunc = func(a string) [4]int {
		if ans, ok := memo[a]; ok {
			return ans
		}
		ans := [4]int{0, 0, 0, 0}
		for _, nxt := range connections[a] {
			tmp := recfunc(nxt)
			switch a {
			case "fft":
				ans[1] += tmp[0] + tmp[1]
				ans[3] += tmp[2] + tmp[3]
			case "dac":
				ans[2] += tmp[0] + tmp[2]
				ans[3] += tmp[1] + tmp[3]
			default:
				ans[0] += tmp[0]
				ans[1] += tmp[1]
				ans[2] += tmp[2]
				ans[3] += tmp[3]
			}
		}
		memo[a] = ans
		return ans
	}
	return recfunc("svr")[3]
}
