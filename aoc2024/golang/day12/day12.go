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

type edgeDetectState struct {
	regionNum int
	nbSame    [4]bool
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
				if ((nb & 0xFF) == (midval & 0xFF)) && nb < nval {
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
	regionperimeters := make(map[int]int)
	areaPerimeterGrid := combineWithNbs(regions, 0, func(midval int, nbs [4]int) [2]int {
		perimeter := 0
		for _, nb := range nbs {
			if nb != midval {
				perimeter += 1
			}
		}
		return [2]int{midval, perimeter}
	})
	for _, row := range areaPerimeterGrid {
		for _, val := range row {
			regionareas[val[0]] += 1
			regionperimeters[val[0]] += val[1]
		}
	}
	for rgnum, area := range regionareas {
		total += area * regionperimeters[rgnum]
	}
	fmt.Println("Part 1:", total)

	edgeDetectGrid := combineWithNbs(regions, 0, func(midval int, nbs [4]int) edgeDetectState {
		var sames [4]bool
		for idx := range sames {
			sames[idx] = (midval == nbs[idx])
		}
		return edgeDetectState{midval, sames}
	})
	edgeGrid := combineWithNbs(edgeDetectGrid, edgeDetectState{}, func(midval edgeDetectState, nbs [4]edgeDetectState) [2]int {
		edgeEnds := 0
		for idx := range nbs {
			oidx := 1 ^ idx // horiz if idx is vert, vert if idx is horiz
			if !(midval.nbSame[idx]) {
				if (!midval.nbSame[oidx]) || nbs[oidx].nbSame[idx] {
					edgeEnds += 1
				}
			}
		}
		return [2]int{midval.regionNum, edgeEnds}
	})
	regionEdges := make(map[int]int)
	for _, row := range edgeGrid {
		for _, val := range row {
			regionEdges[val[0]] += val[1]
		}
	}
	total = 0
	for rgnum, area := range regionareas {
		total += area * regionEdges[rgnum]
	}
	fmt.Println("Part 2:", total)
}
