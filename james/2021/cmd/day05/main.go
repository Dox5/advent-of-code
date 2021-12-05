package main

import (
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"github.com/alecthomas/participle/v2"
)

var exampleData = `0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2`

type Point struct {
	X int `@Int`
	Y int `"," @Int`
}

type Line struct {
	Start Point `@@`
	End   Point `"-" ">" @@`
}

type HydroThermalVents struct {
	VentLocations []Line `@@*`
}

var parser = participle.MustBuild(&HydroThermalVents{})

// Ensure that a < b
func increasing(a, b int) (int, int) {
	if a > b {
		return b, a
	} else {
		return a, b
	}
}

// What increment to use 1 or -1 to get from a to b one step at a time. If a == b
// return 0, no increment needed
func stepBetween(a, b int) (int, int) {
	if a < b {
		return 1, b - a + 1
	} else if a > b {
		return -1, a - b + 1
	} else {
		return 0, 0
	}
}

func pickStepCount(a, b int) int {
	if a == b {
		return a
	} else if b == 0 {
		return a
	} else if a == 0 {
		return b
	}

	panic("Can't cope!")
}

// For every line, write out every coordinate it touches to the channel.
func alignedLineToPoints(vents []Line, points chan<- Point, allowDiag bool) {
	defer close(points)
	for _, ventLine := range vents {

		isHorrizontal := ventLine.Start.Y == ventLine.End.Y
		isVirtical := ventLine.Start.X == ventLine.End.X
		if !allowDiag && !(isHorrizontal || isVirtical) {
			continue
		}

		// Walk between the two points and output them all
		xInc, xSteps := stepBetween(ventLine.Start.X, ventLine.End.X)
		yInc, ySteps := stepBetween(ventLine.Start.Y, ventLine.End.Y)
		x := ventLine.Start.X
		y := ventLine.Start.Y

		steps := pickStepCount(xSteps, ySteps)
		for i := 0; i < steps; i++ {
			point := Point{X: x, Y: y}
			points <- point

			x += xInc
			y += yInc
		}
	}
}

func countByPoint(points <-chan Point) (counts map[Point]int) {
	counts = make(map[Point]int)
	for point := range points {
		if c, ok := counts[point]; ok {
			counts[point] = c + 1
		} else {
			counts[point] = 1
		}
	}

	return
}

func calculateOverlaps(vents []Line, allowDiag bool) {
	points := make(chan Point, 32)

	go alignedLineToPoints(vents, points, allowDiag)
	counts := countByPoint(points)
	overlappedPoints := 0

	for _, v := range counts {
		if v > 1 {
			overlappedPoints++
		}
	}

	fmt.Println("Overlapping points:", overlappedPoints)
}

func part1(vents *HydroThermalVents) {
	calculateOverlaps(vents.VentLocations, false)
}

func part2(vents *HydroThermalVents) {
	calculateOverlaps(vents.VentLocations, true)
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "5")
	if err != nil {
		fmt.Println("Failed to load input,", err)
	}

	exampleVents := &HydroThermalVents{}
	vents := &HydroThermalVents{}
	err = parser.ParseString("exampleData", exampleData, exampleVents)
	if err != nil {
		fmt.Println("Error parsing imput:", err)
		return
	}

	err = parser.ParseBytes("realData", raw_input, vents)
	if err != nil {
		fmt.Println("Error parsing imput:", err)
		return
	}

	fmt.Println("##### Part1 #####")
	fmt.Println("   ExampleData")
	part1(exampleVents)
	fmt.Println("     RealData")
	part1(vents)

	fmt.Println("##### Part2 #####")
	fmt.Println("   ExampleData")
	part2(exampleVents)
	fmt.Println("     RealData")
	part2(vents)
}
