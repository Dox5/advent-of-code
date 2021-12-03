package main

import (
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"

	"bufio"
	"bytes"
	"fmt"
	"strconv"
)

var exampleData = `00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010`

func calcGammaEpsilon(ones []uint, total uint) (gamma uint, epsilon uint) {
	gamma = 0
	epsilon = 0
	for _, bitOnesCount := range ones {
		gamma = gamma << 1

		remaining := total - bitOnesCount

		if bitOnesCount > remaining {
			gamma |= 0b1
		}
	}

	var mask uint = (1 << len(ones)) - 1

	epsilon = (^gamma) & mask

	return
}

func part1(input []byte) {
	scanner := bufio.NewScanner(bytes.NewReader(input))

	var ones []uint
	var bitCount uint = 0

	for scanner.Scan() {
		if ones == nil {
			ones = make([]uint, len(scanner.Text()))
		}

		bitCount++

		// Count ones - which lets us infer everything else
		for bitIndex, bitValue := range scanner.Text() {
			if bitValue == '1' {
				ones[bitIndex]++
			}
		}
	}

	gamma, epsilon := calcGammaEpsilon(ones, bitCount)
	fmt.Printf("epsilon: %v, gamma: %v = %v\n", epsilon, gamma, epsilon*gamma)
}

func splitByBit(bitStrings []string, index uint) (ones, zeros []string) {
	ones = make([]string, 0, len(bitStrings))
	zeros = make([]string, 0, len(bitStrings))

	for _, bitString := range bitStrings {
		if bitString[index] == '0' {
			zeros = append(zeros, bitString)
		} else {
			ones = append(ones, bitString)
		}
	}

	return
}

type finder func(one, zeros []string) []string

func reduce(bitStrings []string, index uint, find finder) string {
	ones, zeros := splitByBit(bitStrings, index)

	next := find(ones, zeros)

	if len(next) == 1 {
		return next[0]
	}
	return reduce(next, index+1, find)
}

func parseBitString(bitString string) uint {
	v, err := strconv.ParseInt(bitString, 2, 32)
	if err != nil {
		panic(fmt.Errorf("Cannot convert bitstring: %v", err))
	}

	return uint(v)
}

func findCo2(ones, zeros []string) []string {
	if len(ones) < len(zeros) {
		return ones
	} else {
		return zeros
	}
}

func findOxygen(ones, zeros []string) []string {
	if len(ones) < len(zeros) {
		return zeros
	} else {
		return ones
	}
}

func part2(input []byte) {
	scanner := bufio.NewScanner(bytes.NewReader(input))
	bitStrings := make([]string, 0, 1000)
	for scanner.Scan() {
		bitStrings = append(bitStrings, scanner.Text())
	}

	// minor ineffiency, first split can be done once!
	oxygenStr := reduce(bitStrings, 0, findOxygen)
	oxygen := parseBitString(oxygenStr)

	co2Str := reduce(bitStrings, 0, findCo2)
	co2 := parseBitString(co2Str)

	fmt.Printf("oxygen %v, co2 %v = %v\n", oxygen, co2, oxygen*co2)
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "3")
	if err != nil {
		fmt.Println("Failed to load input,", err)
	}

	fmt.Println("### Part1 ###")
	fmt.Println(" ExampleData")
	part1([]byte(exampleData))
	fmt.Println("  RealData")
	part1(raw_input)

	fmt.Println("### Part2 ###")
	fmt.Println(" ExampleData")
	part2([]byte(exampleData))
	fmt.Println("  RealData")
	part2(raw_input)

}
