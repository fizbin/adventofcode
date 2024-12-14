package main

import (
	"fmt"
	"log"
	"os"
	"runtime"
	"strconv"
	"strings"
	"sync"
)

func advanceNum(n int) []int {
	if n == 0 {
		return []int{1}
	}
	s := strconv.Itoa(n)
	if len(s)%2 == 0 {
		a1, _ := strconv.Atoi(s[0 : len(s)/2])
		a2, _ := strconv.Atoi(s[len(s)/2:])
		return []int{a1, a2}
	}
	return []int{n * 2024}
}

var countMemo map[[2]int]uint64
var countMemoLock sync.Mutex

func countForN(in int, count int) uint64 {
	countMemoLock.Lock()
	ans, ok := countMemo[[2]int{in, count}]
	countMemoLock.Unlock()
	if ok {
		return ans
	}
	if count <= 0 {
		return 1
	}
	total := uint64(0)
	nexts := advanceNum(in)
	if len(nexts) == 1 {
		total = countForN(nexts[0], count-1)
	} else {
		anschan := make(chan uint64, 1)
		go func(n int, c int) {
			anschan <- countForN(n, c)
			close(anschan)
		}(nexts[0], count-1)
		total = countForN(nexts[1], count-1)
		for val := range anschan {
			total += val
		}
	}
	countMemoLock.Lock()
	countMemo[[2]int{in, count}] = total
	countMemoLock.Unlock()
	return total
}

func main() {
	countMemo = make(map[[2]int]uint64)

	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc11.in"
	} else {
		infile = argsWithoutProg[0]
	}

	data, err := os.ReadFile(infile)
	if err != nil {
		log.Fatal("Couldn't open file: ", err)
	}
	nums := make([]int, 0)
	for _, numStr := range strings.Fields(string(data)) {
		n, e := strconv.Atoi(numStr)
		if e != nil {
			log.Fatalf("Couldn't parse '%s'", numStr)
		}
		nums = append(nums, n)
	}
	total := uint64(0)
	for _, v := range nums {
		total += countForN(v, 25)
	}
	fmt.Println("Part 1:", total)

	runtime.GOMAXPROCS(runtime.NumCPU())

	total = uint64(0)
	var wg sync.WaitGroup
	returns := make(chan uint64, len(nums)+1)
	for _, v := range nums {
		wg.Add(1)
		go func() {
			defer wg.Done()
			returns <- countForN(v, 75)
		}()
	}
	wg.Wait()
	close(returns)
	for val := range returns {
		total += val
	}
	fmt.Println("Part 2:", total)
}
