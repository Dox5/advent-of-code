package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"strconv"
)

var exampleData = `forward 5
down 5
forward 8
up 3
down 8
forward 2
`

type Direction int

const (
	Forward Direction = iota
	Backward
	Up
	Down
)

type Submarine struct {
	horizontal int
	depth      int
	aim        int
}

type instruction struct {
	dir    Direction
	amount int
}

func directionFromString(s string) (Direction, error) {
	mapping := map[string]Direction{
		"forward":  Forward,
		"backward": Backward,
		"up":       Up,
		"down":     Down,
	}

	if v, ok := mapping[s]; ok {
		return v, nil
	} else {
		return Forward, fmt.Errorf("Don't know what '%v' means", s)
	}
}

func parseInstructions(input []byte) ([]instruction, error) {
	scanner := bufio.NewScanner(bytes.NewReader(input))
	scanner.Split(bufio.ScanWords)

	instructions := make([]instruction, 0, 1000)

	for scanner.Scan() {
		command := scanner.Text()

		if !scanner.Scan() {
			return nil, fmt.Errorf("Hit EOF/error when trying to read 'amount': %v", scanner.Err())
		}

		dir, err := directionFromString(command)
		if err != nil {
			return nil, err
		}

		amount, err := strconv.ParseInt(scanner.Text(), 10, 32)
		if err != nil {
			return nil, err
		}

		instructions = append(instructions, instruction{dir: dir, amount: int(amount)})
	}

	return instructions, nil
}

func applyMovement(instructions []instruction, sub Submarine) Submarine {
	for _, inst := range instructions {
		switch inst.dir {
		case Forward:
			sub.horizontal += inst.amount
		case Backward:
			sub.horizontal -= inst.amount
		case Up:
			sub.depth -= inst.amount
		case Down:
			sub.depth += inst.amount
		}
	}

	return sub
}

func applyMovementWithAim(instructions []instruction, sub Submarine) Submarine {
	for _, inst := range instructions {
		switch inst.dir {
		case Forward:
			sub.horizontal += inst.amount
			sub.depth += inst.amount * sub.aim
		case Backward:
			sub.horizontal -= inst.amount
			sub.depth -= inst.amount * sub.aim
		case Up:
			sub.aim -= inst.amount
		case Down:
			sub.aim += inst.amount
		}
	}

	return sub
}

func part1(instructions []instruction) {
	initialSub := Submarine{}
	finalSub := applyMovement(instructions, initialSub)
	fmt.Printf("Submarine: %+v -> %+v (%v)\n", initialSub, finalSub, finalSub.horizontal*finalSub.depth)
}

func part2(instructions []instruction) {
	initialSub := Submarine{}
	finalSub := applyMovementWithAim(instructions, initialSub)
	fmt.Printf("Submarine: %+v -> %+v (%v)\n", initialSub, finalSub, finalSub.horizontal*finalSub.depth)
}

func main() {
	exampleInst, err := parseInstructions([]byte(exampleData))
	if err != nil {
		fmt.Println("Failed to parse instructions :(")
		return
	}

	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "2")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	realInst, err := parseInstructions(raw_input)
	if err != nil {
		fmt.Println("Failed to parse instructions,", err)
		return
	}

	fmt.Println("##### Part1 #####")
	fmt.Println("   ExampleData")
	part1(exampleInst)
	fmt.Println("     RealData")
	part1(realInst)

	fmt.Println("##### Part2 #####")
	fmt.Println("   ExampleData")
	part2(exampleInst)
	fmt.Println("     RealData")
	part2(realInst)

}
