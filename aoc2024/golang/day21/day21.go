package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
)

type Location struct {
	x, y int
}

type pair[T, U any] struct {
	First  T
	Second U
}

var numpad = [...]string{"789", "456", "123", " 0A"}
var dirpad = [...]string{" ^A", "<v>"}

type cacheEntry struct {
	von, zu rune
	level   int
}

var cache = make(map[cacheEntry]uint64)

func minDirpadMoveLen(von, zu rune, level int) uint64 {
	if ans, ok := cache[cacheEntry{von, zu, level}]; ok {
		return ans
	}

	// Yeah, pretty much copy-paste of minNumpadMoveLen below, but...

	var loczu, locvon, avoid Location

	for rowidx, row := range dirpad {
		for colidx, ch := range row {
			here := Location{rowidx, colidx}
			if ch == ' ' {
				avoid = here
			}
			if ch == von {
				locvon = here
			}
			if ch == zu {
				loczu = here
			}
		}
	}

	var minval uint64 = math.MaxUint64
	q := []pair[Location, string]{{locvon, ""}}
	for len(q) > 0 {
		w := q[0]
		q = q[1:]
		if w.First == avoid {
			continue
		}
		if w.First == loczu {
			t := minDirpadSeqLen(w.Second+"A", level-1)
			if t < minval {
				minval = t
			}
			continue
		}
		if w.First.x < loczu.x {
			q = append(q, pair[Location, string]{Location{w.First.x + 1, w.First.y}, w.Second + "v"})
		}
		if w.First.x > loczu.x {
			q = append(q, pair[Location, string]{Location{w.First.x - 1, w.First.y}, w.Second + "^"})
		}
		if w.First.y < loczu.y {
			q = append(q, pair[Location, string]{Location{w.First.x, w.First.y + 1}, w.Second + ">"})
		}
		if w.First.y > loczu.y {
			q = append(q, pair[Location, string]{Location{w.First.x, w.First.y - 1}, w.Second + "<"})
		}
	}
	cache[cacheEntry{von, zu, level}] = minval
	return minval
}

func minDirpadSeqLen(moves string, level int) uint64 {
	if level == 0 {
		return uint64(len(moves))
	}

	pspot := 'A'
	ret := uint64(0)
	for _, ch := range moves {
		ret += minDirpadMoveLen(pspot, ch, level)
		pspot = ch
	}
	return ret
}

func minNumpadMoveLen(von, zu rune, intermediateDirpads int) uint64 {
	var loczu, locvon, avoid Location

	for rowidx, row := range numpad {
		for colidx, ch := range row {
			here := Location{rowidx, colidx}
			if ch == ' ' {
				avoid = here
			}
			if ch == von {
				locvon = here
			}
			if ch == zu {
				loczu = here
			}
		}
	}

	var minval uint64 = math.MaxUint64
	q := []pair[Location, string]{{locvon, ""}}
	for len(q) > 0 {
		w := q[0]
		q = q[1:]
		if w.First == avoid {
			continue
		}
		if w.First == loczu {
			t := minDirpadSeqLen(w.Second+"A", intermediateDirpads)
			// fmt.Println(w, t)
			if t < minval {
				minval = t
			}
			continue
		}
		if w.First.x < loczu.x {
			q = append(q, pair[Location, string]{Location{w.First.x + 1, w.First.y}, w.Second + "v"})
		}
		if w.First.x > loczu.x {
			q = append(q, pair[Location, string]{Location{w.First.x - 1, w.First.y}, w.Second + "^"})
		}
		if w.First.y < loczu.y {
			q = append(q, pair[Location, string]{Location{w.First.x, w.First.y + 1}, w.Second + ">"})
		}
		if w.First.y > loczu.y {
			q = append(q, pair[Location, string]{Location{w.First.x, w.First.y - 1}, w.Second + "<"})
		}
	}
	return minval
}

func minNumpadSeqLen(code string, intermediateDirpads int) uint64 {
	pspot := 'A'
	ret := uint64(0)
	for _, ch := range code {
		ret += minNumpadMoveLen(pspot, ch, intermediateDirpads)
		pspot = ch
	}
	return ret
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc21.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	total1 := uint64(0)
	total2 := uint64(0)
	for scanner.Scan() {
		line := scanner.Text()
		codenum, _ := strconv.ParseUint(line[:(len(line)-1)], 10, 32)
		step := minNumpadSeqLen(line, 2)
		total1 += step * codenum

		step2 := minNumpadSeqLen(line, 25)
		total2 += step2 * codenum
	}
	fmt.Println("Part 1:", total1)
	fmt.Println("Part 2:", total2)
}
