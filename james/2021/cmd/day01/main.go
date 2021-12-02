package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"strconv"
)

func part1(depths []int) {
	increases := 0

	last := depths[0]

	for _, depth := range depths[1:] {
		if depth > last {
			increases++
		}
		last = depth
	}

	fmt.Printf("There were %v increases\n", increases)
}

func sum(vs []int) int {
	accum := 0
	for _, v := range vs {
		accum += v
	}

	return accum
}

func part2(depths []int) {
	windowLen := 3
	windowCount := len(depths) - windowLen + 1

	increases := 0

	last := sum(depths[0:3])
	for i := 1; i < windowCount; i++ {
		this := sum(depths[i : i+3])

		if this > last {
			increases++
		}

		last = this
	}

	fmt.Printf("There were %v increases\n", increases)
}

func parseDepths(input []byte) ([]int, error) {
	scanner := bufio.NewScanner(bytes.NewReader(input))
	depths := make([]int, 0, 500)

	for scanner.Scan() {
		v, err := strconv.ParseInt(scanner.Text(), 10, 32)

		if err != nil {
			return nil, fmt.Errorf("Failed to convert %v to an integer: %v", scanner.Text(), err)
		}

		depths = append(depths, int(v))
	}

	return depths, nil
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "1")
	if err != nil {
		fmt.Println("Failed to load input,", err)
	}

	depths, err := parseDepths(raw_input)
	if err != nil {
		fmt.Println("Failed to get list of depths:", err)
		return
	}

	exmpleData := []int{199, 200, 208, 210, 200, 207, 240, 269, 260, 263}

	fmt.Println("#### PART 1 ####")
	fmt.Println("Example Data")
	part1(exmpleData)

	fmt.Println("Real")
	part1(depths)

	fmt.Println()
	fmt.Println("#### PART 2 ####")
	fmt.Println("Example Data")
	part2(exmpleData)

	fmt.Println("Real")
	part2(depths)
}
