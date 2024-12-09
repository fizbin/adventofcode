package main

import (
	"fmt"
	"log"
	"os"
	"unicode"
)

type diskSpot struct {
	loc  int
	size int
}
type diskFileSpot struct {
	loc     int
	size    int
	filenum int16
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc9.in"
	} else {
		infile = argsWithoutProg[0]
	}

	data, err := os.ReadFile(infile)
	if err != nil {
		log.Fatalf("Couldn't open file: %v", err)
	}

	// int16 because I know that the max filenum is ~ 10,000
	diskmap := make([]int16, 0, 9*len(data))
	filenum := int16(0)
	isfile := true
	loc := 0
	for _, v := range data {
		if unicode.IsSpace(rune(v)) {
			continue
		}
		if isfile {
			for range int(v - '0') {
				diskmap = append(diskmap, filenum)
			}
			filenum += 1
		} else {
			for range int(v - '0') {
				diskmap = append(diskmap, -1)
			}
		}
		isfile = !isfile
		loc += int(v - '0')
	}
	rgt := len(diskmap) - 1
	lft := 0
	for lft < rgt {
		for (lft < rgt) && (diskmap[lft] != -1) {
			lft += 1
		}
		for (lft < rgt) && (diskmap[rgt] == -1) {
			rgt -= 1
		}
		for (lft < rgt) && (diskmap[lft] == -1) && (diskmap[rgt] != -1) {
			diskmap[lft], diskmap[rgt] = diskmap[rgt], diskmap[lft]
			lft += 1
			rgt -= 1
		}
	}
	checksum := int64(0)
	for idx, fn := range diskmap {
		if fn > 0 {
			checksum += int64(idx) * int64(fn)
		}
	}
	fmt.Printf("Part 1: %d\n", checksum)

	filemap := make([]diskFileSpot, 0, len(data))
	freemap := make([]diskSpot, 0, len(data))
	filenum = int16(0)
	isfile = true
	loc = 0
	for _, v := range data {
		if unicode.IsSpace(rune(v)) {
			continue
		}
		sz := int(v - '0')
		if isfile {
			filemap = append(filemap, diskFileSpot{loc: loc, size: sz, filenum: filenum})
			filenum++
		} else {
			freemap = append(freemap, diskSpot{loc: loc, size: sz})
		}
		isfile = !isfile
		loc += sz
	}
	for fileIdx := len(filemap) - 1; fileIdx >= 0; fileIdx-- {
		filespot := filemap[fileIdx]
		for freeIdx, freespot := range freemap {
			if (freespot.loc < filespot.loc) && (freespot.size >= filespot.size) {
				freemap[freeIdx] = diskSpot{freespot.loc + filespot.size, freespot.size - filespot.size}
				filemap[fileIdx] = diskFileSpot{freespot.loc, filespot.size, filespot.filenum}
				break
			}
		}
	}
	for idx := range diskmap {
		diskmap[idx] = -1
	}
	for _, dFS := range filemap {
		for offset := range dFS.size {
			diskmap[dFS.loc+offset] = dFS.filenum
		}
	}
	checksum = int64(0)
	for idx, fn := range diskmap {
		if fn > 0 {
			checksum += int64(idx) * int64(fn)
		}
	}
	fmt.Printf("Part 2: %d\n", checksum)
}
