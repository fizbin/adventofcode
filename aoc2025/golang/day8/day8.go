package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
	"strconv"
	"strings"
)

func find_group_nums(group_nums map[int]int, max_idx int, connected [][2]int) map[int]int {
	changed := make([]int, 0, max_idx)
	for x := range max_idx {
		changed = append(changed, x)
	}
	connectedPrime := make(map[int][]int)
	for _, pair := range connected {
		x, y := pair[0], pair[1]
		if connectedPrime[x] == nil {
			connectedPrime[x] = []int{y}
		} else {
			connectedPrime[x] = append(connectedPrime[x], y)
		}
		if connectedPrime[y] == nil {
			connectedPrime[y] = []int{x}
		} else {
			connectedPrime[y] = append(connectedPrime[y], x)
		}
	}
	for len(changed) > 0 {
		nchanged := make([]int, 0, max_idx)
		for _, x := range changed {
			if nbs := connectedPrime[x]; nbs != nil {
				for _, y := range nbs {
					if group_nums[y] != group_nums[x] {
						ming := min(group_nums[y], group_nums[x])
						if group_nums[y] != ming {
							nchanged = append(nchanged, y)
							group_nums[y] = ming
						}
						if group_nums[x] != ming {
							nchanged = append(nchanged, x)
							group_nums[x] = ming
						}
					}
				}
			}
		}
		changed = nchanged
	}
	return group_nums
}

func distsq(a, b [3]uint64) uint64 {
	return (a[0]-b[0])*(a[0]-b[0]) + (a[1]-b[1])*(a[1]-b[1]) + (a[2]-b[2])*(a[2]-b[2])
}

func part1(datapoints [][3]uint64) uint64 {
	all_idx_pairs := make([][2]int, 0)
	for x := range len(datapoints) {
		for y := x + 1; y < len(datapoints); y++ {
			all_idx_pairs = append(all_idx_pairs, [2]int{x, y})
		}
	}
	slices.SortFunc(all_idx_pairs, func(a, b [2]int) int {
		dista := distsq(datapoints[a[0]], datapoints[a[1]])
		distb := distsq(datapoints[b[0]], datapoints[b[1]])
		if dista > distb {
			return 1
		} else if distb > dista {
			return -1
		}
		return 0
	})
	connected := all_idx_pairs[0:1000]
	group_nums := make(map[int]int)
	for x := range len(datapoints) {
		group_nums[x] = x + 1
	}
	group_nums = find_group_nums(group_nums, len(datapoints), connected)
	size_map := make(map[int]uint64)
	for _, gnum := range group_nums {
		size_map[gnum]++
	}
	var v1, v2, v3 uint64
	for _, gsize := range size_map {
		if gsize >= v1 {
			v3 = v2
			v2 = v1
			v1 = gsize
			continue
		}
		if gsize >= v2 {
			v3 = v2
			v2 = gsize
			continue
		}
		if gsize >= v3 {
			v3 = gsize
		}
	}
	return v1 * v2 * v3
}

func part2(datapoints [][3]uint64) uint64 {
	all_idx_pairs := make([][2]int, 0)
	for x := range len(datapoints) {
		for y := x + 1; y < len(datapoints); y++ {
			all_idx_pairs = append(all_idx_pairs, [2]int{x, y})
		}
	}
	slices.SortFunc(all_idx_pairs, func(a, b [2]int) int {
		dista := distsq(datapoints[a[0]], datapoints[a[1]])
		distb := distsq(datapoints[b[0]], datapoints[b[1]])
		if dista > distb {
			return 1
		} else if distb > dista {
			return -1
		}
		return 0
	})
	pair_idx := 0
	found_nodes := make(map[int]bool)
	for len(found_nodes) < len(datapoints) {
		found_nodes[all_idx_pairs[pair_idx][0]] = true
		found_nodes[all_idx_pairs[pair_idx][1]] = true
		pair_idx++
	}
	pair_idx -= 2
	connected := all_idx_pairs[0:pair_idx]
	group_nums := make(map[int]int)
	for x := range len(datapoints) {
		group_nums[x] = x + 1
	}
	group_nums = find_group_nums(group_nums, len(datapoints), connected)
	some_disconnected := 0
	for node, group_num := range group_nums {
		if group_num > 1 {
			some_disconnected = node
			break
		}
	}
	for some_disconnected > 0 {
		connected = append(connected, all_idx_pairs[pair_idx])
		pair_idx++
		group_nums = find_group_nums(group_nums, len(datapoints), connected)
		some_disconnected = 0
		for node, group_num := range group_nums {
			if group_num > 1 {
				some_disconnected = node
				break
			}
		}
	}
	return datapoints[all_idx_pairs[pair_idx-1][0]][0] * datapoints[all_idx_pairs[pair_idx-1][1]][0]
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc8.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var points [][3]uint64
	for scanner.Scan() {
		var nvals [3]uint64
		for idx, v := range strings.Split(scanner.Text(), ",") {
			n, _ := strconv.ParseUint(v, 10, 64)
			nvals[idx] = n
		}
		points = append(points, nvals)
	}
	fmt.Println(part1(points))
	fmt.Println(part2(points))
}
