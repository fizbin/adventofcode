package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"slices"
	"strconv"
)

func runProg(regA, regB, regC int, prog []int) []int {
	ip := 0
	output := make([]int, 0)
	for ip >= 0 && ip < len(prog)-1 {
		opcode := prog[ip]
		arg := prog[ip+1]
		if opcode != 3 && opcode != 1 && arg >= 4 {
			// "Combo" arg
			switch arg {
			case 4:
				arg = regA
			case 5:
				arg = regB
			case 6:
				arg = regC
			default:
				log.Fatal("Bad combo arg ", arg, " at ", ip)
			}
		}
		// fmt.Println("DBG: ", regA, regB, regC, " at ", ip, " (", opcode, arg, ")")
		switch opcode {
		case 0:
			for range arg {
				regA /= 2
			}
		case 1:
			regB ^= arg
		case 2:
			regB = arg % 8
		case 3:
			if regA != 0 {
				ip = arg - 2
			}
		case 4:
			regB ^= regC
		case 5:
			output = append(output, arg%8)
		case 6:
			regB = regA
			for range arg {
				regB /= 2
			}
		case 7:
			regC = regA
			for range arg {
				regC /= 2
			}
		}
		ip += 2
	}
	return output
}

func mkQuine(prog []int) int {
	ncorrect := 0
	avals := []int{0}
	for ncorrect < len(prog) {
		navals := make([]int, 0)
		trustModulus := 1 << (3 * ncorrect)
		for idx, v := range avals {
			avals[idx] = v % trustModulus
		}
		slices.Sort(avals)
		avals = slices.Compact(avals)
		for _, abase := range avals {
			for inc := range 1024 {
				aval := (inc * trustModulus) + abase
				out := runProg(aval, 0, 0, prog)
				if len(out) > ncorrect && slices.Equal(out[:ncorrect+1], prog[:ncorrect+1]) {
					if ncorrect+1 == len(prog) {
						return aval
					}
					navals = append(navals, aval)
				}
			}
		}
		avals = navals
		ncorrect += 1
		if len(avals) == 0 {
			log.Fatal("Out of possibilities")
		}
	}
	return -1
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc17.in"
	} else {
		infile = argsWithoutProg[0]
	}

	data, err := os.ReadFile(infile)
	if err != nil {
		log.Fatal("Couldn't open file: {}", err)
	}

	getNums := regexp.MustCompile(`\d+`)
	numStrs := getNums.FindAll(data, -1)
	regA, _ := strconv.Atoi(string(numStrs[0]))
	regB, _ := strconv.Atoi(string(numStrs[1]))
	regC, _ := strconv.Atoi(string(numStrs[2]))

	prog := make([]int, 0)
	for _, nstr := range numStrs[3:] {
		n, _ := strconv.Atoi(string(nstr))
		prog = append(prog, n)
	}
	result := runProg(regA, regB, regC, prog)
	fmt.Print("Part 1: ")
	fmt.Print(result[0])
	for _, res := range result[1:] {
		fmt.Print(",", res)
	}
	fmt.Println()

	fmt.Println("Part 2:", mkQuine(prog))
}
