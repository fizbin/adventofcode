package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func combineWithNbs[S ~[][]E, E any, F any](init S, edge E, combiner func(E, [4]E) F) [][]F {
	retval := make([][]F, len(init))
	for idx, _ := range retval {
		v := init[idx]
		retval[idx] = make([]F, len(v))
		for colidx, midval := range v {
			var arg [4]E
			for aidx, dir := range [][]int{{-1, 0}, {0, -1}, {0, 1}, {1, 0}} {
				otheridx := idx + dir[0]
				othercol := colidx + dir[1]
				if otheridx < 0 || otheridx >= len(init) || othercol < 0 || othercol >= len(v) {
					arg[aidx] = edge
				} else {
					arg[aidx] = init[otheridx][othercol]
				}
			}
			retval[idx][colidx] = combiner(midval, arg)
		}
	}
	return retval
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc12.in"
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

	var regions [][]int
	rval := 1
	for _, row := range rawlines {
		hrow := make([]int, len(row))
		for colidx, ch := range row {
			hrow[colidx] = (rval << 8) + int(ch)
			rval += 1
		}
		regions = append(regions, hrow)
	}
	doneRegions := false
	for !doneRegions {
		doneRegions = true
		regions = combineWithNbs(regions, 0, func(midval int, nbs [4]int) int {
			nval := midval
			for _, nb := range nbs {
				if ((nb & 0xFF) == (midval & 0xFF)) && nb > nval {
					nval = nb
				}
			}
			if nval != midval {
				doneRegions = false
			}
			return nval
		})
	}
	regionmap := make(map[int]bool)
	for _, row := range regions {
		for _, rgnum := range row {
			regionmap[rgnum] = true
		}
	}
	total := 0
	regionareas := make(map[int]int)
	for rgnum := range regionmap {
		area := 0
		perimeter := 0
		combineWithNbs(regions, 0, func(midval int, nbs [4]int) int {
			if midval == rgnum {
				area += 1
				for _, nb := range nbs {
					if nb != midval {
						perimeter += 1
					}
				}
			}
			return 0
		})
		regionareas[rgnum] = area
		total += area * perimeter
	}
	fmt.Println("Part 1:", total)

	total = 0
	for rgnum := range regionmap {
		edgeDetect := combineWithNbs(regions, 0, func(midval int, nbs [4]int) int {
			if midval != rgnum {
				return 0
			}
			nval := 0
			factor := 1
			for _, nb := range nbs {
				if nb != midval {
					nval += factor
				}
				factor *= 2
			}
			return nval
		})
		nedges := 0
		combineWithNbs(edgeDetect, 0, func(midval int, nbs [4]int) int {
			factor := 1
			for idx := range nbs {
				if (midval&factor) > 0 && (nbs[idx^1]&factor == 0) {
					nedges += 1
				}
				factor *= 2
			}
			return 0
		})
		total += nedges * regionareas[rgnum]
	}
	fmt.Println("Part 2:", total)
}
