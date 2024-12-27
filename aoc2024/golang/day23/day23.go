package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strings"
)

func combinations[S ~[]A, A any](n int, from S) [][]A {
	if n < 0 || len(from) < n {
		// no combos
		return [][]A{}
	}
	if n == 0 {
		// one combo, that is empty
		return [][]A{{}}
	}
	start := combinations(n, from[1:])
	second := combinations(n-1, from[1:])
	for _, sval := range second {
		newCombo := make([]A, 1, n)
		newCombo[0] = from[0]
		newCombo = append(newCombo, sval...)
		start = append(start, newCombo)
	}
	return start
}

func fullyConnected(network map[string][]string, nodes []string) bool {
	for xidx, x := range nodes[:len(nodes)-1] {
		for _, y := range nodes[xidx+1:] {
			if !slices.Contains(network[x], y) {
				return false
			}
		}
	}
	return true
}

func findTTriples(network map[string][]string) int {
	total := 0
	for snode, connections := range network {
		if !strings.HasPrefix(snode, "t") {
			continue
		}
		for _, others := range combinations(2, connections) {
			if strings.HasPrefix(others[0], "t") && others[0] < snode {
				continue
			}
			if strings.HasPrefix(others[1], "t") && others[1] < snode {
				continue
			}
			if slices.Contains(network[others[0]], others[1]) {
				total++
			}
		}
	}
	return total
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc23.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	network := make(map[string][]string)

	for scanner.Scan() {
		splitted := strings.Split(scanner.Text(), "-")
		if rst, ok := network[splitted[0]]; ok {
			network[splitted[0]] = append(rst, splitted[1])
		} else {
			network[splitted[0]] = []string{splitted[1]}
		}
		if rst, ok := network[splitted[1]]; ok {
			network[splitted[1]] = append(rst, splitted[0])
		} else {
			network[splitted[1]] = []string{splitted[0]}
		}
	}

	for _, v := range network {
		slices.Sort(v)
	}

	maxCore := []string{}
	for node, nexts := range network {
		nodeFollowers := make([]string, 0, len(nexts))
		for _, n := range nexts {
			if n > node {
				nodeFollowers = append(nodeFollowers, n)
			}
		}

		for comboSz := len(nexts); comboSz > len(maxCore); comboSz-- {
			for _, rest := range combinations(comboSz-1, nodeFollowers) {
				tst := append(rest, node)
				if fullyConnected(network, tst) {
					maxCore = tst
					break
				}
			}
		}
	}

	slices.Sort(maxCore)
	fmt.Println("Part 1:", findTTriples(network))
	fmt.Println("Part 2:", strings.Join(maxCore, ","))
}
