package main

import (
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"sort"
	"strconv"
	"strings"
)

var exampleData = `16,1,2,0,4,2,7,1,2,14`

type CrabPos struct {
	X     int
	Count int
}
type Crabs = []CrabPos

func ParseCrabs(s string) Crabs {
	input := strings.Trim(s, "\n")
	strNums := strings.Split(input, ",")

	crabs := make([]int, 0, len(strNums))

	for _, n := range strNums {
		v, err := strconv.ParseInt(n, 10, 32)
		if err != nil {
			panic(err)
		}

		crabs = append(crabs, int(v))
	}

	sort.Ints(crabs)

	dedupedCrabs := make(Crabs, 0, len(crabs))
	dedupedCrabs = append(dedupedCrabs, CrabPos{X: crabs[0], Count: 1})
	for i := 1; i < len(crabs); i++ {
		lastIndex := len(dedupedCrabs) - 1
		if dedupedCrabs[lastIndex].X == crabs[i] {
			dedupedCrabs[lastIndex].Count++
		} else {
			dedupedCrabs = append(dedupedCrabs, CrabPos{X: crabs[i], Count: 1})
		}
	}

	return dedupedCrabs
}

func binomial(n int) int {
	return ((n * n) + n) / 2
}

func part1(s string) {
	crabs := ParseCrabs(s)

	// Calculate the cost to move to position 0
	costToMoveToZero := 0
	for _, crab := range crabs {
		costToMoveToZero += crab.X * crab.Count
	}

	// Now sweep the rest of the positions and update the total cost
	cost := costToMoveToZero // track the cost to the last looked at position
	cheapest := cost         // Track the cheapest cost (the answer)
	crabSplit := 0           // How many crabs are to the left of this position
	for x := 1; x < crabs[len(crabs)-1].X; x++ {
		// First workout if our crab split has changed, find the first crab at
		// or above this positon
		for ; crabs[crabSplit].X < x; crabSplit++ {
		}

		// we've only move a single step - so adjust cost appropriately
		// Everything (strictly) to the left adds one to the score
		for i := 0; i < crabSplit; i++ {
			cost += crabs[i].Count // Every crab we step away from adds 1
		}

		for i := crabSplit; i < len(crabs); i++ {
			cost -= crabs[i].Count // Every crab we step towards subtracts 1
			// including possibly the one we are (it's movement would be zero!)
		}

		//fmt.Println("cost to move to x:", x, "=", cost)
		if cost < cheapest {
			cheapest = cost
		}
	}

	fmt.Println("Cheapest cost was:", cheapest)
}

func distance(x1, x2 int) int {
	if x1 > x2 {
		return x1 - x2
	} else {
		return x2 - x1
	}
}

func part2(s string) {
	crabs := ParseCrabs(s)

	// Now sweep the rest of the positions and update the total cost
	cheapest := -1 // Track the cheapest cost (the answer)

outer:
	for x := 0; x < crabs[len(crabs)-1].X; x++ {
		total := 0
		for _, crab := range crabs {
			dist := distance(crab.X, x)
			fuel := binomial(dist)
			total += fuel * crab.Count

			if cheapest != -1 && total > cheapest {
				// can't be this position, try the next
				continue outer
			}
		}

		// must be the cheapest if we haven't exited the inner
		cheapest = total
	}

	fmt.Println("Cheapest cost was:", cheapest)
}

func main() {
	fmt.Println("##### Part1 #####")
	fmt.Println("   ExampleData")
	part1(exampleData)

	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "7")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	fmt.Println("     RealData")
	part1(string(raw_input))

	fmt.Println("##### Part2 #####")
	fmt.Println("   ExampleData")
	part2(exampleData)
	fmt.Println("     RealData")
	part2(string(raw_input))
}
