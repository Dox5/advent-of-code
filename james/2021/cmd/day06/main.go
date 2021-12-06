package main

import (
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"strconv"
	"strings"
)

const (
	MAX_TIME_TO_SPAWN = 8
)

var exampleData = `3,4,3,1,2`

type State = []int

func NewState() State {
	// Need to be able to index [0,8]
	return make([]int, MAX_TIME_TO_SPAWN+1)
}

func NewPopulatedState(initial []byte) State {
	input := strings.Trim(string(initial), "\n")
	strNums := strings.Split(input, ",")

	state := NewState()

	for _, n := range strNums {
		v, err := strconv.ParseInt(n, 10, 32)
		if err != nil {
			panic(err)
		}

		state[v]++
	}

	return state
}

func simulate(state State, simDays int) State {
	for day := 0; day < simDays; day++ {
		next := NewState()

		// First spawn new fish - they go stright to 8 days till spawning
		next[8] = state[0]

		// Then deal with the awkward wrap around for fish - they go to 6
		next[6] = state[0]

		// Rest of the fish move one day closer to spawning
		for i := 0; i < MAX_TIME_TO_SPAWN; i++ {
			// next is indexed [0, 7) and state is indexed [1,8)
			next[i] += state[i+1] // Need to add as the newly spawned fish will
			// 'collide' with existing ones
		}

		state = next
	}

	return state
}

func fishSim(initial []byte, simDays int) {
	final := simulate(NewPopulatedState(initial), simDays)

	totalFish := 0

	for _, fishCount := range final {
		totalFish += fishCount
	}

	fmt.Printf("Total of %v fish after %v days: %+v\n", totalFish, simDays, final)
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "6")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	fmt.Println("##### Part1 #####")
	fmt.Println("   ExampleData")
	fishSim([]byte(exampleData), 18)
	fishSim([]byte(exampleData), 80)
	fishSim([]byte(exampleData), 256)

	fmt.Println("     RealData")
	fishSim([]byte(raw_input), 80)
	fishSim([]byte(raw_input), 256)
}
