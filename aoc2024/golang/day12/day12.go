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
		regions = combineWithNbs(regions, 0, func(up, left, midval, right, down int) int {
			nval := midval
			if ((up & 0xFF) == (midval & 0xFF)) && up > nval {
				nval = up
			}
			if ((left & 0xFF) == (midval & 0xFF)) && left > nval {
				nval = left
			}
			if ((right & 0xFF) == (midval & 0xFF)) && right > nval {
				nval = right
			}
			if ((down & 0xFF) == (midval & 0xFF)) && down > nval {
				nval = down
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
		combineWithNbs(regions, 0, func(up, left, midval, right, down int) int {
			if midval == rgnum {
				area += 1
				if up != midval {
					perimeter += 1
				}
				if down != midval {
					perimeter += 1
				}
				if left != midval {
					perimeter += 1
				}
				if right != midval {
					perimeter += 1
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
		edgeDetect := combineWithNbs(regions, 0, func(up, left, midval, right, down int) int {
			if midval != rgnum {
				return 0
			}
			nval := 0
			if up != midval {
				nval += 1
			}
			if left != midval {
				nval += 2
			}
			if right != midval {
				nval += 4
			}
			if down != midval {
				nval += 8
			}
			return nval
		})
		nedges := 0
		combineWithNbs(edgeDetect, 0, func(up, left, midval, right, down int) int {
			if (midval&1) > 0 && (left&1) == 0 {
				nedges += 1
			}
			if (midval&8) > 0 && (left&8) == 0 {
				nedges += 1
			}
			if (midval&2) > 0 && (up&2) == 0 {
				nedges += 1
			}
			if (midval&4) > 0 && (up&4) == 0 {
				nedges += 1
			}
			return 0
		})
		total += nedges * regionareas[rgnum]
	}
	fmt.Println("Part 2:", total)
}
