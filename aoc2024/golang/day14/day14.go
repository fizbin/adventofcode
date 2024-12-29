package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

const (
	width  = 101
	height = 103
)

type robot struct {
	x, y, vx, vy int
}

type location struct {
	x, y int
}

func (r robot) AtGen(gen int) location {
	return location{(width + (r.x+gen*r.vx)%width) % width, (height + (r.y+gen*r.vy)%height) % height}
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc14.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	parser := regexp.MustCompile(`-?\d+`)
	var robots []robot
	var quads [4]int
	for scanner.Scan() {
		numstrs := parser.FindAllString(scanner.Text(), -1)
		rx, _ := strconv.Atoi(numstrs[0])
		ry, _ := strconv.Atoi(numstrs[1])
		rvx, _ := strconv.Atoi(numstrs[2])
		rvy, _ := strconv.Atoi(numstrs[3])
		r := robot{rx, ry, rvx, rvy}
		robots = append(robots, r)
		spot := r.AtGen(100)
		if spot.x < width/2 {
			if spot.y < height/2 {
				quads[0]++
			} else if spot.y > height/2 {
				quads[1]++
			}
		} else if spot.x > width/2 {
			if spot.y < height/2 {
				quads[2]++
			} else if spot.y > height/2 {
				quads[3]++
			}
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning:", err)
	}
	fmt.Println("Part 1:", quads[0]*quads[1]*quads[2]*quads[3])
	bestScore := 0
	bestGen := 0
	for gen := range 103 * 101 {
		distinctx := make(map[int]bool)
		distincty := make(map[int]bool)
		for _, r := range robots {
			spot := r.AtGen(gen)
			distinctx[spot.x] = true
			distincty[spot.y] = true
		}
		score := len(distinctx)*len(distinctx) + len(distincty)*len(distincty)
		if bestScore == 0 || score < bestScore {
			bestScore = score
			bestGen = gen
		}
	}
	grid := make(map[location]bool)
	for _, r := range robots {
		grid[r.AtGen(bestGen)] = true
	}
	// for yidx := range height {
	// 	for xidx := range width {
	// 		if grid[location{xidx, yidx}] {
	// 			fmt.Print("#")
	// 		} else {
	// 			fmt.Print(".")
	// 		}
	// 	}
	// 	fmt.Println()
	// }
	fmt.Println("Part 2:", bestGen)
}
