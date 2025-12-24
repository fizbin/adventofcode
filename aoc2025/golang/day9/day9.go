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

type pDir int

const (
	Down pDir = 1
	Up   pDir = 2
)

type point struct {
	x uint64
	y uint64
}

func part1(datapoints [][2]uint64) uint64 {
	maxarea := uint64(0)
	for idx1 := range len(datapoints) - 1 {
		for idx2 := idx1 + 1; idx2 < len(datapoints); idx2++ {
			minx := min(datapoints[idx1][0], datapoints[idx2][0])
			miny := min(datapoints[idx1][1], datapoints[idx2][1])
			maxx := max(datapoints[idx1][0], datapoints[idx2][0])
			maxy := max(datapoints[idx1][1], datapoints[idx2][1])
			area := (maxx - minx + 1) * (maxy - miny + 1)
			maxarea = max(area, maxarea)
		}
	}
	return maxarea
}

func part2(datapoints [][2]uint64) uint64 {
	pointXs := make(map[uint64]bool)
	pointYs := make(map[uint64]bool)
	pathByX := make(map[uint64]map[uint64]pDir)
	for idx := range len(datapoints) {
		idx2 := (idx + 1) % len(datapoints)
		p1 := datapoints[idx]
		p2 := datapoints[idx2]
		pointXs[p1[0]] = true
		pointYs[p1[1]] = true
		pointXs[p1[0]+1] = true
		pointYs[p1[1]+1] = true
		if pathByX[p1[0]] == nil {
			pathByX[p1[0]] = make(map[uint64]pDir)
		}
		if pathByX[p2[0]] == nil {
			pathByX[p2[0]] = make(map[uint64]pDir)
		}
		if p1[1] == p2[1] && p1[0] <= p2[0] {
			pathByX[p1[0]][p1[1]] |= Down
			pathByX[p2[0]][p1[1]] |= Up
		} else if p1[1] == p2[1] && p1[0] >= p2[0] {
			pathByX[p1[0]][p1[1]] |= Up
			pathByX[p2[0]][p1[1]] |= Down
		}
	}
	pointXlist := make([]uint64, 0, len(pointXs))
	for x, tval := range pointXs {
		if tval {
			pointXlist = append(pointXlist, x)
		}
	}
	pointYlist := make([]uint64, 0, len(pointXs))
	for y, tval := range pointYs {
		if tval {
			pointYlist = append(pointYlist, y)
		}
	}
	slices.Sort(pointXlist)
	slices.Sort(pointYlist)
	acceptable := make(map[point]bool)
	fromAbove := make(map[uint64]pDir)
	for _, pointX := range pointXlist {
		if pathByX[pointX] == nil {
			pathByX[pointX] = make(map[uint64]pDir)
		}
		pbx := pathByX[pointX]
		var state pDir
		for _, pointY := range pointYlist {
			if (pbx[pointY] != 0) || (fromAbove[pointY] != 0) || (state != 0) {
				acceptable[point{pointX, pointY}] = true
			}
			state ^= pbx[pointY]
			if pbx[pointY]&Down != 0 {
				fromAbove[pointY] = Up | Down
			} else if pbx[pointY]&Up != 0 {
				fromAbove[pointY] = 0
			} else {
				state ^= fromAbove[pointY]
			}
		}
	}
	acceptableRectangle := func(top, bottom, left, right uint64) bool {
		ystartIdx := 0
		for _, pointX := range pointXlist {
			if pointX > bottom {
				break
			}
			if pointX >= top {
				for yIdx, pointY := range pointYlist[max(0, ystartIdx-1):] {
					if ystartIdx == 0 && pointY >= left {
						ystartIdx = yIdx + 1
					}
					if pointY > right {
						break
					}
					if pointY >= left {
						if !acceptable[point{pointX, pointY}] {
							return false
						}
					}
				}
			}
		}
		return true
	}
	maxrect := uint64(0)
	for idx1, p1 := range datapoints {
		for _, p2 := range datapoints[idx1+1:] {
			top := min(p1[0], p2[0])
			bottom := max(p1[0], p2[0])
			left := min(p1[1], p2[1])
			right := max(p1[1], p2[1])
			area := (bottom - top + 1) * (right - left + 1)
			if area > maxrect && acceptableRectangle(top, bottom, left, right) {
				maxrect = area
			}
		}
	}

	return maxrect
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc9.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var points [][2]uint64
	for scanner.Scan() {
		var nvals [2]uint64
		for idx, v := range strings.Split(scanner.Text(), ",") {
			n, _ := strconv.ParseUint(v, 10, 64)
			nvals[idx] = n
		}
		points = append(points, nvals)
	}
	fmt.Println(part1(points))
	fmt.Println(part2(points))
}
