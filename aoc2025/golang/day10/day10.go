package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

type Machine struct {
	lights  []int
	buttons [][]int
	jolts   []int
}

func solveForIntsModulo(coeff [][]int, yVals []int, modulus int, prevSolns [][]int) ([][]int, error) {
	if len(coeff) != len(yVals) {
		return nil, fmt.Errorf("bad conditions: coeff had length %d and yVals had length %d", len(coeff), len(yVals))
	}
	if len(prevSolns) < 1 {
		return nil, fmt.Errorf("no previous solutions given")
	}
	nvars := len(coeff[0])
	ans := make([][]int, 0)
	halfmod := modulus / 2
	for testpat := uint64(0); testpat < (1 << nvars); testpat++ {
		base := make([]int, nvars)
		for idx := range nvars {
			if (1<<idx)&testpat != 0 {
				base[idx] += halfmod
			}
		}
	NEXTPREV:
		for _, prev := range prevSolns {
			for yIdx, coeffLine := range coeff {
				mySum := 0
				for xIdx, pv := range prev {
					mySum += coeffLine[xIdx] * (base[xIdx] + pv)
				}
				if mySum%modulus != yVals[yIdx]%modulus {
					continue NEXTPREV
				}
			}
			newSoln := slices.Clone(base)
			for xIdx, pv := range prev {
				newSoln[xIdx] += pv
			}
			ans = append(ans, newSoln)
		}
	}
	return ans, nil
}

// This is deliberately an attempt to build a mostly generic solver, not take advantage of
// the characteristics that we have here to build a specialized solver

// This depends on the idea that you're solving for non-negative integers and that all
// coefficients are also non-negative integers, but that's all.
func solveForInts(coeff [][]int, yVals []int) ([][]int, error) {
	working := [][]int{make([]int, len(coeff[0]))}
	for mod := 2; mod > 0; mod *= 2 {
		nworking, err := solveForIntsModulo(coeff, yVals, mod, working)
		// fmt.Printf("Modulo: %d; #solutions: %d\n", mod, len(nworking))
		if err != nil {
			return nil, err
		}
		nnworking := make([][]int, 0)
		keepGoing := false
	NEXTSOLN:
		for _, soln := range nworking {
			for yIdx, yVal := range yVals {
				ySum := 0
				for xIdx := range len(soln) {
					ySum += coeff[yIdx][xIdx] * soln[xIdx]
				}
				if ySum > yVal {
					continue NEXTSOLN
				} else if ySum < yVal {
					keepGoing = true
				}
			}
			nnworking = append(nnworking, soln)
		}
		if !keepGoing {
			return nnworking, nil
		}
		working = nnworking
	}
	return nil, fmt.Errorf("exceeded int bounds finding all the solutions")
}

func part1(machines []Machine) int {
	wholeSum := 0
	for _, mach := range machines {
		coeffs := make([][]int, len(mach.lights))
		for idx := range len(mach.lights) {
			coeffRow := make([]int, len(mach.buttons))
			for bidx, button := range mach.buttons {
				if slices.Contains(button, idx) {
					coeffRow[bidx] = 1
				}
			}
			coeffs[idx] = coeffRow
		}

		allGood, err := solveForIntsModulo(coeffs, mach.lights, 2, [][]int{make([]int, len(mach.buttons))})
		if err != nil {
			log.Fatal(err)
		}
		minpress := len(mach.buttons) + 1
		for _, soln := range allGood {
			press := 0
			for _, v := range soln {
				press += v
			}
			minpress = min(minpress, press)
		}
		wholeSum += minpress
	}

	return wholeSum
}

func part2(machines []Machine) int {
	wholeSum := 0
	for _, mach := range machines {
		coeffs := make([][]int, len(mach.jolts))
		for idx := range len(mach.jolts) {
			coeffRow := make([]int, len(mach.buttons))
			for bidx, button := range mach.buttons {
				if slices.Contains(button, idx) {
					coeffRow[bidx] = 1
				}
			}
			coeffs[idx] = coeffRow
		}

		allGood, err := solveForInts(coeffs, mach.jolts)
		if err != nil {
			log.Fatal(err)
		}
		minpress := slices.Max(mach.jolts) + 100
		for _, soln := range allGood {
			press := 0
			for _, v := range soln {
				press += v
			}
			minpress = min(minpress, press)
		}
		// log.Printf("Machine %d: %d\n", mIdx, minpress)
		wholeSum += minpress
	}

	return wholeSum
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) > 0 {
		infile = argsWithoutProg[0]
	} else {
		infile = "../aoc10.in"
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	parser := regexp.MustCompile(`\[([.#]+)\] *([(),0-9 ]+) *\{([0-9,]+)\}`)
	bparser := regexp.MustCompile(`\(([\d,]+)\)`)
	machines := make([]Machine, 0)
	for scanner.Scan() {
		groups := parser.FindSubmatch(scanner.Bytes())
		lights := make([]int, 0)
		for _, ch := range groups[1] {
			switch ch {
			case '.':
				lights = append(lights, 0)
			case '#':
				lights = append(lights, 1)
			}
		}
		buttons := make([][]int, 0)
		for _, butstr := range bparser.FindAllSubmatch(groups[2], -1) {
			button := make([]int, 0)
			for _, connStr := range strings.Split(string(butstr[1]), ",") {
				v, _ := strconv.ParseInt(connStr, 10, 32)
				button = append(button, int(v))
			}
			buttons = append(buttons, button)
		}
		jolts := make([]int, 0)
		for _, joltStr := range strings.Split(string(groups[3]), ",") {
			v, _ := strconv.ParseInt(joltStr, 10, 32)
			jolts = append(jolts, int(v))
		}
		machines = append(machines, Machine{lights: lights, buttons: buttons, jolts: jolts})
	}
	fmt.Println(part1(machines))
	fmt.Println(part2(machines))
}
