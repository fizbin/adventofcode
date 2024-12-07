package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"runtime"
	"strconv"
	"strings"
	"sync"
)

type iPair struct {
	fst uint64
	snd uint64
}

func intConcat(a uint64, b uint64) uint64 {
	base := uint64(1)
	for base <= b {
		base *= 10
	}
	return (a*base + b)
}

func getAns2(nums []uint64, idx int, sofar uint64, goal uint64) bool {
	if sofar > goal {
		return false
	}
	if idx == len(nums) {
		return (sofar == goal)
	}
	return getAns2(nums, idx+1, sofar+nums[idx], goal) ||
		getAns2(nums, idx+1, sofar*nums[idx], goal)
}

func getAns3(nums []uint64, idx int, sofar uint64, goal uint64) bool {
	if sofar > goal {
		return false
	}
	if idx == len(nums) {
		return (sofar == goal)
	}
	return getAns3(nums, idx+1, sofar+nums[idx], goal) ||
		getAns3(nums, idx+1, sofar*nums[idx], goal) ||
		getAns3(nums, idx+1, intConcat(sofar, nums[idx]), goal)
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc7.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatalf("Couldn't open file: %v", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	runtime.GOMAXPROCS(runtime.NumCPU())
	results := make(chan iPair, 1000)
	var wg sync.WaitGroup
	for scanner.Scan() {
		line := scanner.Text()
		splitted := strings.Split(line, ":")
		linevalStr := splitted[0]
		numsStr := splitted[1]
		lineval, e := strconv.ParseUint(linevalStr, 10, 64)
		if e != nil {
			log.Fatalf("Couldn't parse %v", linevalStr)
		}
		numStrs := strings.Fields(numsStr)
		var nums []uint64
		for _, nstr := range numStrs {
			n, e2 := strconv.ParseUint(nstr, 10, 64)
			if e2 != nil {
				log.Fatalf("Couldn't parse %v", nstr)
			}
			nums = append(nums, n)
		}
		wg.Add(1)
		go func() {
			defer wg.Done()
			if getAns2(nums, 1, nums[0], lineval) {
				results <- iPair{lineval, lineval}
			} else if getAns3(nums, 1, nums[0], lineval) {
				results <- iPair{0, lineval}
			}
		}()
	}
	wg.Wait()
	close(results)
	total1 := uint64(0)
	total2 := uint64(0)
	for mypair := range results {
		total1 += mypair.fst
		total2 += mypair.snd
	}
	fmt.Println("Part 1:", total1)
	fmt.Println("Part 2:", total2)
}
