package main

import (
	"bufio"
	"fmt"
	"log"
	"maps"
	"os"
	"slices"
	"strconv"
	"strings"
)

const (
	op_and = iota
	op_xor = iota
	op_or  = iota
)

type computeSpec struct {
	op int
	w1 string
	w2 string
}

func main() {
	argsWithoutProg := os.Args[1:]
	var infile string
	if len(argsWithoutProg) == 0 {
		infile = "../aoc24.in"
	} else {
		infile = argsWithoutProg[0]
	}

	file, err := os.Open(infile)
	if err != nil {
		log.Fatal("Couldn't open file:", err)
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)

	initMap := make(map[string]int8)
	for scanner.Scan() {
		if scanner.Text() == "" {
			break
		}
		flds := strings.Split(scanner.Text(), ": ")
		n, _ := strconv.Atoi(flds[1])
		initMap[flds[0]] = int8(n)
	}

	computeMap := make(map[string]computeSpec)
	for scanner.Scan() {
		flds := strings.Fields(scanner.Text())
		key := flds[4]
		op := op_and
		switch flds[1] {
		case "AND":
			op = op_and
		case "OR":
			op = op_or
		case "XOR":
			op = op_xor
		default:
			log.Fatalf("Bad op %v", flds[1])
		}
		computeMap[key] = computeSpec{op: op, w1: flds[0], w2: flds[2]}
	}

	fmt.Printf("Part 1: %v\n", doCompute(initMap, computeMap))
	goodSwaps := swapFixAdd(computeMap)
	// fmt.Println("DBG:", goodSwaps)
	part2 := make([]string, 0, 2*len(goodSwaps))
	for _, v := range goodSwaps {
		part2 = append(part2, v[0], v[1])
	}
	slices.Sort(part2)
	fmt.Println("Part 2:", strings.Join(part2, ","))
}

func evalOperation(spec computeSpec, values map[string]int8) int8 {
	var a, b int8
	if valA, ok := values[spec.w1]; !ok {
		return -1
	} else {
		a = valA
	}
	if valB, ok := values[spec.w2]; !ok {
		return -1
	} else {
		b = valB
	}
	switch spec.op {
	case op_and:
		return a & b
	case op_or:
		return a | b
	case op_xor:
		return a ^ b
	}
	return -1
}

func doCompute(initMap map[string]int8, computeMap map[string]computeSpec) uint64 {
	zSpec := make([]string, 0, 45)
	notifyMap := make(map[string][]string)
	for key, val := range computeMap {
		if strings.HasPrefix(key, "z") {
			zSpec = append(zSpec, key)
		}
		if notList, ok := notifyMap[val.w1]; ok {
			notifyMap[val.w1] = append(notList, key)
		} else {
			notifyMap[val.w1] = []string{key}
		}
		if notList, ok := notifyMap[val.w2]; ok {
			notifyMap[val.w2] = append(notList, key)
		} else {
			notifyMap[val.w2] = []string{key}
		}
	}
	slices.Sort(zSpec)
	q := make([]string, 0, len(initMap)+len(computeMap))
	for key := range initMap {
		if notList, ok := notifyMap[key]; ok {
			q = append(q, notList...)
		}
	}
	var valueMap map[string]int8 = maps.Clone(initMap)
	for len(q) > 0 {
		w := q[0]
		q = q[1:]
		if spec, ok := computeMap[w]; ok {
			if _, ok := valueMap[w]; !ok {
				newVal := evalOperation(spec, valueMap)
				if newVal >= 0 {
					if notList, ok := notifyMap[w]; ok {
						q = append(q, notList...)
					}
					valueMap[w] = newVal
				}
			}
		}
	}
	total := uint64(0)
	for idx := len(zSpec); idx > 0; idx-- {
		total *= 2
		total += uint64(valueMap[zSpec[idx-1]])
	}
	return total
}

func findUpstream(computeMap map[string]computeSpec, start string, maxdepth int) []string {
	retval := make([]string, 0)
	retval = append(retval, start)
	working := []string{start}
	for maxdepth > 0 {
		nworking := make([]string, 0)
		for _, thing := range working {
			if spec, ok := computeMap[thing]; ok {
				nworking = append(nworking, spec.w1, spec.w2)
			}
		}
		retval = append(retval, nworking...)
		working = nworking
		maxdepth--
	}
	slices.Sort(retval)
	return slices.Compact(retval)
}

// returns the bits that are wrong
func tryAddition(computeMap map[string]computeSpec, a, b uint64) uint64 {
	xWires := make([]string, 0)
	yWires := make([]string, 0)
	for _, spec := range computeMap {
		if strings.HasPrefix(spec.w1, "x") {
			xWires = append(xWires, spec.w1)
		}
		if strings.HasPrefix(spec.w2, "x") {
			xWires = append(xWires, spec.w2)
		}
		if strings.HasPrefix(spec.w1, "y") {
			yWires = append(yWires, spec.w1)
		}
		if strings.HasPrefix(spec.w2, "y") {
			yWires = append(yWires, spec.w2)
		}
	}
	slices.Sort(xWires)
	xWires = slices.Compact(xWires)
	slices.Sort(yWires)
	yWires = slices.Compact(yWires)
	initMap := make(map[string]int8)
	a0, b0 := a, b
	for _, xWire := range xWires {
		initMap[xWire] = int8(a % 2)
		a /= 2
	}
	for _, yWire := range yWires {
		initMap[yWire] = int8(b % 2)
		b /= 2
	}
	val1 := doCompute(initMap, computeMap)
	return ((a0 + b0) ^ val1)
}

func swapFixAdd(computeMap map[string]computeSpec) [][2]string {
	zSpec := make([]string, 0, 45)
	for key := range computeMap {
		if strings.HasPrefix(key, "z") {
			zSpec = append(zSpec, key)
		}
	}
	slices.Sort(zSpec)
	retval := make([][2]string, 0)

	done := false
	for !done {
		done = true
		for xexp := range len(zSpec) - 1 {
			if tryAddition(computeMap, 1<<xexp, 0) != 0 {
				badBits := tryAddition(computeMap, 1<<xexp, 0)
				for xmul := range uint64(32) {
					xval := xmul * (uint64(1) << max(0, xexp-2))
					for ymul := range uint64(32) {
						yval := ymul * (uint64(1) << max(0, xexp-2))
						badBits |= tryAddition(computeMap, xval, yval)
					}
				}
				starters := make([]string, 0)
				for _, zWire := range zSpec {
					if badBits%2 != 0 {
						starters = append(starters, zWire)
					}
					badBits /= 2
				}
				wiresToTry := make([]string, 0)
				for _, w := range starters {
					wiresToTry = append(wiresToTry, findUpstream(computeMap, w, 6)...)
				}
				slices.Sort(wiresToTry)
				wiresToTry = slices.Compact(wiresToTry)
				myswaps := make([][2]string, 0)
				for idx1 := 0; idx1+1 < len(wiresToTry); idx1++ {
					for idx2 := idx1 + 1; idx2 < len(wiresToTry); idx2++ {
						if _, ok := computeMap[wiresToTry[idx1]]; !ok {
							continue
						}
						if _, ok := computeMap[wiresToTry[idx2]]; !ok {
							continue
						}
						computeMap[wiresToTry[idx1]], computeMap[wiresToTry[idx2]] = computeMap[wiresToTry[idx2]], computeMap[wiresToTry[idx1]]
						badBits = 0
						for xmul := range uint64(32) {
							xval := xmul * (uint64(1) << max(0, xexp-2))
							for ymul := range uint64(32) {
								yval := ymul * (uint64(1) << max(0, xexp-2))
								badBits |= tryAddition(computeMap, xval, yval)
								if badBits != 0 {
									break
								}
							}
							if badBits != 0 {
								break
							}
						}
						computeMap[wiresToTry[idx1]], computeMap[wiresToTry[idx2]] = computeMap[wiresToTry[idx2]], computeMap[wiresToTry[idx1]]
						if badBits != 0 {
							continue
						}
						myswaps = append(myswaps, [2]string{wiresToTry[idx1], wiresToTry[idx2]})
					}
				}
				if len(myswaps) == 0 {
					log.Fatalf("Couldn't find fix for 1 << %v\n", xexp)
				}
				if len(myswaps) > 1 {
					log.Fatalf("Ambiguous fix for 1 << %v: %v\n", xexp, myswaps)
				}
				retval = append(retval, myswaps...)
				computeMap[myswaps[0][0]], computeMap[myswaps[0][1]] = computeMap[myswaps[0][1]], computeMap[myswaps[0][0]]
				done = false
			}
		}
	}

	return retval
}
