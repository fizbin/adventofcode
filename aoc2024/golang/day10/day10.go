package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"slices"
)

func combineWithNbs[S ~[][]E, E any](init S, edge E, combiner func(E, E, E, E, E) E) S {
	retval := slices.Clone[[][]E](init)
	for idx, v := range retval {
		retval[idx] = slices.Clone(v)
		for colidx, midval := range v {
			var up E
			var left E
			var right E
			var down E
			if idx == 0 {
				up = edge
			} else {
				up = init[idx-1][colidx]
			}
			if colidx == 0 {
				left = edge
			} else {
				left = v[colidx-1]
			}
			if colidx+1 >= len(v) {
				right = edge
			} else {
				right = v[colidx+1]
			}
			if idx+1 >= len(init) {
				down = edge
			} else {
				down = init[idx+1][colidx]
			}
			retval[idx][colidx] = combiner(up, left, midval, right, down)
		}
	}
	return retval
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc10.in"
	} else {
		infile = argsWithoutProg[0]
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
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning: {}", err)
	}

	var heights [][]int8
	for _, row := range rawlines {
		hrow := make([]int8, len(row))
		for colidx, ch := range row {
			hrow[colidx] = int8(ch - '0')
		}
		heights = append(heights, hrow)
	}

	type location struct {
		row int
		col int
	}
	type map1type struct {
		height int8
		reach  map[location]bool
	}

	var working1 [][]map1type
	for rowidx, row := range heights {
		myrow := make([]map1type, len(row))
		for colidx, height := range row {
			if height < 9 {
				myrow[colidx] = map1type{height, nil}
			} else {
				mp := make(map[location]bool)
				mp[location{row: rowidx, col: colidx}] = true
				myrow[colidx] = map1type{9, mp}
			}
		}
		working1 = append(working1, myrow)
	}

	combiner1 := func(up map1type, left map1type, mid map1type, right map1type, down map1type) map1type {
		myh := mid.height
		mymap := mid.reach
		if mymap == nil {
			mymap = make(map[location]bool)
		}
		for _, other := range []map1type{up, left, right, down} {
			if other.height == myh+1 {
				for k := range other.reach {
					mymap[k] = true
				}
			}
		}
		return map1type{mid.height, mymap}
	}

	for range 9 {
		working1 = combineWithNbs(working1, map1type{99, nil}, combiner1)
	}

	var total int
	for _, row := range working1 {
		for _, val := range row {
			if val.height == 0 {
				total += len(val.reach)
			}
		}
	}

	fmt.Println("Part 1:", total)

	type map2type struct {
		height    int8
		pathCount int
	}

	var working2 [][]map2type
	for _, row := range heights {
		myrow := make([]map2type, len(row))
		for colidx, height := range row {
			myrow[colidx] = map2type{height, 0}
		}
		working2 = append(working2, myrow)
	}

	combiner2 := func(up map2type, left map2type, mid map2type, right map2type, down map2type) map2type {
		myh := mid.height
		if myh == 9 {
			return map2type{9, 1}
		}
		myPathCount := 0
		for _, other := range []map2type{up, left, right, down} {
			if other.height == myh+1 {
				myPathCount += other.pathCount
			}
		}
		return map2type{mid.height, myPathCount}
	}

	for range 10 {
		working2 = combineWithNbs(working2, map2type{99, 0}, combiner2)
	}
	total = 0
	for _, row := range working2 {
		for _, val := range row {
			if val.height == 0 {
				total += val.pathCount
			}
		}
	}
	fmt.Println("Part 2:", total)
}
