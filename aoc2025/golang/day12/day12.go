package main

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"regexp"
	"strconv"
	"strings"
)

type puzzle struct {
	width      int
	height     int
	pieceCount []int
}

func main() {
	flag.Parse()
	argsWithoutProg := flag.Args()
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc12.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()

	pieceParser := regexp.MustCompile(`(?m)^(\d+):\s+((?:[#.]+\s*)+)`)
	pieces := make(map[int][][]byte)
	allData, _ := io.ReadAll(file)
	for _, match := range pieceParser.FindAllSubmatch(allData, -1) {
		val, _ := strconv.Atoi(string(match[1]))
		piece := bytes.Split(bytes.TrimSpace(match[2]), []byte{'\n'})
		pieces[val] = piece
	}
	puzzles := make([]puzzle, 0)
	puzzleParser := regexp.MustCompile(`(\d+)x(\d+): *([\d ]+)`)
	for _, match := range puzzleParser.FindAllSubmatch(allData, -1) {
		width, _ := strconv.Atoi(string(match[1]))
		height, _ := strconv.Atoi(string(match[2]))
		puzzleCounts := make([]int, 0)
		for _, mstr := range strings.Split(strings.TrimSpace(string(match[3])), " ") {
			val, _ := strconv.Atoi(mstr)
			puzzleCounts = append(puzzleCounts, val)
		}
		puzzles = append(puzzles, puzzle{width, height, puzzleCounts})
	}
	fmt.Println(part1(pieces, puzzles))
}

func part1(pieces map[int][][]byte, puzzles []puzzle) int {
	// implement easy accept and easy reject, then panic for now
	maxHeight := 0
	maxWidth := 0
	puzzlePieceSz := make(map[int]int)
	for pidx, pshape := range pieces {
		hashCount := 0
		for _, row := range pshape {
			for _, bt := range row {
				if bt == '#' {
					hashCount++
				}
			}
			maxWidth = max(maxWidth, len(row))
		}
		puzzlePieceSz[pidx] = hashCount
		maxHeight = max(maxHeight, len(pshape))
	}
	ans := 0
	for _, puzzle := range puzzles {
		totalPiecesNeeded := 0
		totalMult := 0
		for pidx, mult := range puzzle.pieceCount {
			totalPiecesNeeded += mult * puzzlePieceSz[pidx]
			totalMult += mult
		}
		if totalPiecesNeeded > puzzle.height*puzzle.width {
			// easy reject
			continue
		}
		if (puzzle.width/maxWidth)*(puzzle.height/maxHeight) >= totalMult {
			// easy accept
			ans++
			continue
		}
		panic("unimplemented hard case")
	}
	return ans
}
