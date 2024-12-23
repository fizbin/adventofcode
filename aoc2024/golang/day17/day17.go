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
	// Progam analysis:
	// 2,4  regB = regA%8
	// 1,5  regB ^= 5
	// 7,5  regC = regA / (2**regB)
	// 1,6  regB ^= 6
	// 0,3  regA /= 8
	// 4,3  regB ^= regC
	// 5,5  output.append!(regb%8)
	// 3,0  if (regA != 0) {jump 0}

	// Therefore:
	// The loop works from the low-order bits of regA
	// regB and regC have no state to pass from loop iteration to loop iteration
	// Only the bottom three bits of regB and regC are ever significant
	// regB is fed from the bottom 3 bits of A each loop
	// regC is fed from three bits of regA that are somewhere in the bottom 10 bits of regA
	// (which three bits depends on the value of regB)
	// output of a loop is uniquely determined by the bottom 10 bits of A at the start of the loop
	// regA goes down by three bits each loop

	// Conclusion: output[:n] depends on only the bottom (7 + 3*n) bits of the initial regA
	// Therefore, we can find the bottom 10 bits to get the first output, and then work
	// our way up three bits at a time to match every subsequent input.

	avals := []int{}
	// first get the possibilities for the bottom 10 bits
	for aval := range 1024 {
		out := runProg(aval, 0, 0, prog)
		if out[0] == prog[0] {
			avals = append(avals, aval)
		}
	}

	// so we've gotten the first output number correct
	ncorrect := 1

	// now solve for three bits at a time, assuming that the bottom 3*ncorrect+7 are correct
	for ncorrect < len(prog) {
		newAvals := make([]int, 0, 8*len(avals))
		stepSize := 1 << (3*ncorrect + 7)
		for topThreeBits := range 8 {
			for _, abase := range avals {
				aval := (topThreeBits * stepSize) + abase
				out := runProg(aval, 0, 0, prog)
				if slices.Equal(out, prog) {
					// This happens earlier than ncorrect == len(prog), so exit here
					return aval
				}
				if len(out) > ncorrect && slices.Equal(out[:ncorrect+1], prog[:ncorrect+1]) {
					// We found three top bits to add on that give us one more correct answer
					newAvals = append(newAvals, aval)
				}
			}
		}
		avals = newAvals
		ncorrect += 1
		if len(avals) == 0 {
			log.Fatal("Out of possibilities")
		}
	}
	log.Fatal("Overshot the program without hitting it")
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
