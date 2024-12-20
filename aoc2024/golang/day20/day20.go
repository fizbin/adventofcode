package main

import (
	"bufio"
	"container/heap"
	"fmt"
	"log"
	"os"
)

// This is surely in some standard library somewhere, yes?
type pair[T, U any] struct {
	First  T
	Second U
}

type Location struct {
	x, y int
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

type heapItem struct {
	where Location
	dist  int
}

type heapType []heapItem

func (pq heapType) Len() int { return len(pq) }

func (pq heapType) Less(i, j int) bool {
	return pq[i].dist < pq[j].dist
}

func (pq heapType) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
}

// add x as element Len()
func (pq *heapType) Push(x any) {
	item := x.(heapItem)
	*pq = append(*pq, item)
}

// remove and return element Len() - 1.
func (pq *heapType) Pop() any {
	old := *pq
	n := len(old)
	item := old[n-1]
	old[n-1] = heapItem{}
	*pq = old[0 : n-1]
	return item
}

func getNeighborsWithCosts(grid map[Location]byte, minDist int, maxDist int, current Location) []pair[Location, int] {
	allNeighbors := make([]pair[Location, int], 0, (maxDist+1)*(maxDist+1)*2)
	for xoff := -maxDist; xoff <= maxDist; xoff++ {
		for yoff := -maxDist; yoff <= maxDist; yoff++ {
			cdist := abs(xoff) + abs(yoff)
			if cdist >= minDist && cdist <= maxDist {
				allNeighbors = append(allNeighbors, pair[Location, int]{Location{current.x + xoff, current.y + yoff}, cdist})
			}
		}
	}
	retval := make([]pair[Location, int], 0, 3)
	for _, nbr := range allNeighbors {
		sp := grid[nbr.First]
		if sp == '.' || sp == 'S' || sp == 'E' {
			retval = append(retval, nbr)
		}
	}
	return retval
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc20.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	height := 0
	// width := 0
	var startLoc, endLoc Location
	mymap := make(map[Location]byte)
	for scanner.Scan() {
		for colidx, ch := range scanner.Text() {
			mymap[Location{x: height, y: colidx}] = byte(ch)
			if ch == 'S' {
				startLoc = Location{x: height, y: colidx}
			}
			if ch == 'E' {
				endLoc = Location{x: height, y: colidx}
			}
		}
		// width = len(txt)
		height += 1
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning:", err)
	}
	distFromStart := make(map[Location]int)
	working := &heapType{heapItem{startLoc, 0}}
	heap.Init(working)
	for len(*working) > 0 {
		sd := heap.Pop(working).(heapItem)
		if _, ok := distFromStart[sd.where]; ok {
			continue
		}
		distFromStart[sd.where] = sd.dist
		for _, nbr := range getNeighborsWithCosts(mymap, 1, 1, sd.where) {
			heap.Push(working, heapItem{where: nbr.First, dist: sd.dist + nbr.Second})
		}
	}
	distFromEnd := make(map[Location]int)
	working = &heapType{heapItem{endLoc, 0}}
	heap.Init(working)
	for len(*working) > 0 {
		sd := heap.Pop(working).(heapItem)
		if _, ok := distFromEnd[sd.where]; ok {
			continue
		}
		distFromEnd[sd.where] = sd.dist
		for _, nbr := range getNeighborsWithCosts(mymap, 1, 1, sd.where) {
			heap.Push(working, heapItem{where: nbr.First, dist: sd.dist + nbr.Second})
		}
	}
	baseDist := distFromStart[endLoc]
	{
		goodCheats := make(map[pair[Location, Location]]bool)
		for spot, dFromS := range distFromStart {
			if baseDist-dFromS < 100 {
				continue
			}
			for _, nbr := range getNeighborsWithCosts(mymap, 2, 2, spot) {
				newDist := dFromS + nbr.Second + distFromEnd[nbr.First]
				if newDist < baseDist {
					if baseDist-newDist >= 100 {
						goodCheats[pair[Location, Location]{spot, nbr.First}] = true
					}
				}
			}
		}
		fmt.Println("Part 1:", len(goodCheats))
	}
	{
		goodCheats := make(map[pair[Location, Location]]bool)
		for spot, dFromS := range distFromStart {
			if baseDist-dFromS < 100 {
				continue
			}
			for _, nbr := range getNeighborsWithCosts(mymap, 2, 20, spot) {
				newDist := dFromS + nbr.Second + distFromEnd[nbr.First]
				if newDist < baseDist {
					if baseDist-newDist >= 100 {
						goodCheats[pair[Location, Location]{spot, nbr.First}] = true
					}
				}
			}
		}
		fmt.Println("Part 2:", len(goodCheats))
	}
}
