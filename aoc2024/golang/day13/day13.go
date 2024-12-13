package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

func parseProblem(ptext string) [6]int {
	pparser := regexp.MustCompile(`Button A: X\+(\d+), Y\+(\d+)\s*Button B: X\+(\d+), Y\+(\d+)\s*Prize: X=(\d+), Y=(\d+)`)
	match := pparser.FindStringSubmatch(ptext)
	if match == nil {
		log.Fatal("Bad parse on", ptext)
	}
	var retval [6]int
	for idx, nstr := range match[1:] {
		n, e := strconv.Atoi(nstr)
		if e != nil {
			log.Fatal("Bad numparse", e, "on", nstr)
		}
		retval[idx] = n
	}
	return retval
}

func solve1(ax, ay, bx, by, px, py int) int {
	detcoeff := ax*by - ay*bx
	detwob := ax*py - ay*px
	detwoa := by*px - bx*py
	if detwob%detcoeff != 0 {
		return -1
	}
	if detwoa%detcoeff != 0 {
		return -1
	}
	apresses := detwoa / detcoeff
	bpresses := detwob / detcoeff
	if apresses < 0 || bpresses < 0 {
		return -1
	}
	return 3*apresses + bpresses
}

func solve2(ax, ay, bx, by, px, py int) int {
	realpx := int64(px) + 10000000000000
	realpy := int64(py) + 10000000000000
	detcoeff := int64(ax*by - ay*bx)
	detwob := int64(ax)*realpy - int64(ay)*realpx
	detwoa := int64(by)*realpx - int64(bx)*realpy
	if detwob%detcoeff != 0 {
		return -1
	}
	if detwoa%detcoeff != 0 {
		return -1
	}
	apresses := detwoa / detcoeff
	bpresses := detwob / detcoeff
	if apresses < 0 || bpresses < 0 {
		return -1
	}
	return int(3*apresses + bpresses)
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc13.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	var rawparas []string
	para := ""
	for scanner.Scan() {
		if scanner.Text() == "" {
			rawparas = append(rawparas, para)
			para = ""
		} else {
			para = para + scanner.Text() + "\n"
		}
	}
	if err := scanner.Err(); err != nil {
		log.Fatal("Error scanning: {}", err)
	}
	total := 0
	for _, ptext := range rawparas {
		problem := parseProblem(ptext)
		ans := solve1(problem[0], problem[1], problem[2],
			problem[3], problem[4], problem[5])
		if ans >= 0 {
			total += ans
		}
	}
	fmt.Println("Part 1:", total)

	total = 0
	for _, ptext := range rawparas {
		problem := parseProblem(ptext)
		ans := solve2(problem[0], problem[1], problem[2],
			problem[3], problem[4], problem[5])
		if ans >= 0 {
			total += ans
		}
	}
	fmt.Println("Part 2:", total)
}
