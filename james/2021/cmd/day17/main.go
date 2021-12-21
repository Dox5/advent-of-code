package main

import (
	"fmt"
	"math"
	"sort"
)

var epsilon64 float64 = math.Nextafter(1.0, 2.0) - 1.0

type TargetArea struct {
	X1, X2, Y1, Y2 int
}

var exampleTarget = TargetArea{
	X1: 20,
	X2: 30,
	Y1: -10,
	Y2: -5,
}

var realData = TargetArea{
	X1: 156,
	X2: 202,
	Y1: -110,
	Y2: -69,
}

func triangleRoot(n int) (int, bool) {
	// Triangle numbers follow rule that `8n+1` will be square, (cited to
	// "elements of algebra" on Wikipedia).
	// We use that to quickly know if n is indeed a triangle number
	root := math.Sqrt(float64(8*n + 1))
	if math.Mod(root, 1) >= epsilon64 {
		// Not triangle
		return 0, false
	}

	return int((root - 1) / 2), true
}

func nthTriangleNumber(n int) int {
	return (n * (n + 1)) / 2
}

func part1(target TargetArea) {
	bestShotY := 0
	// The distance between the target and the source on the Y axis determines
	// the difference between the triangle numbers required to hit that point
	// exactly. Firing upward with exactly that power will cause a return to the
	// origin - so then the next step must land on the tile being considered
	for x := target.X1; x <= target.X2; x++ {
		// Can we even stop on this X?
		_, ok := triangleRoot(x)
		if !ok {
			continue
		}

		// Can stop in the column - how high can we get
		for y := target.Y1; y <= target.Y2; y++ {
			if y >= 0 {
				panic("Can't handle positive depth!!!")
			}

			// Shoot in the positive direction (up) with 1 less than abs(y). This
			// causes the probe to exactly land on y=0 but at a point where the
			// next step will step exactly onto this target Y
			yPower := (y * -1) - 1
			if yPower > bestShotY {
				bestShotY = yPower
			}
		}
	}

	fmt.Printf("Best shot: %v [height: %v]\n", bestShotY, nthTriangleNumber(bestShotY))
}

type axisShot struct {
	Power int
	Steps int  // can reach pos in Steps steps
	Stops bool // Will stop in on Pos
}

type shot struct {
	X, Y int
}

func (s shot) LessThan(other shot) bool {
	if s.X < other.X {
		return true
	} else {
		if s.X == other.X {
			return s.Y < other.Y
		} else {
			return false
		}
	}
}

func findViable(xShots, yShots []axisShot) []shot {
	shots := make([]shot, 0, 512)

	for _, xShot := range xShots {
		for _, yShot := range yShots {
			if xShot.Stops {
				// any Y shot with >= xShot.Steps must match
				if yShot.Steps >= xShot.Steps {
					shots = append(shots, shot{X: xShot.Power, Y: yShot.Power})
				}
			} else {
				// only Yshots with exactly xSort.steps match
				if yShot.Steps == xShot.Steps {
					shots = append(shots, shot{X: xShot.Power, Y: yShot.Power})
				}
			}
		}
	}

	// Can find the same shot through different paths so dedupe
	sort.Slice(shots, func(i, j int) bool {
		return shots[i].LessThan(shots[j])
	})

	k := 0
	for _, shot := range shots {
		if shots[k] != shot {
			k++
			shots[k] = shot
		}
	}

	return shots[:k+1]
}

func part2(target TargetArea) []shot {
	xShots := make([]axisShot, 0, 256)
	for initialPower := target.X2; initialPower > 0; initialPower-- {
		power := initialPower
		step := 0

		for xPos := initialPower; xPos <= target.X2 && power > 0; xPos += power {
			step++

			if xPos >= target.X1 {
				// Landed within targets X
				xShots = append(xShots, axisShot{Power: initialPower,
					Steps: step,
					Stops: power == 1})
			}

			power--
		}
	}

	yShots := make([]axisShot, 0, 256)

	maxYPower := (-target.Y1) - 1
	for initialPower := target.Y1; initialPower <= maxYPower; initialPower++ {
		power := initialPower
		step := 0

		for yPos := initialPower; yPos >= target.Y1; yPos += power {
			step++
			if yPos <= target.Y2 {
				yShots = append(yShots, axisShot{Power: initialPower,
					Steps: step,
					Stops: false}) // Y never stops
			}
			power--
		}
	}

	viable := findViable(xShots, yShots)
	fmt.Printf("Found %v viable shots\n", len(viable))
	return viable
}

func checkShots(actual []shot) {
	expected := []shot{
		{X: 23, Y: -10}, {X: 25, Y: -9}, {X: 27, Y: -5}, {X: 29, Y: -6},
		{X: 22, Y: -6}, {X: 21, Y: -7}, {X: 9, Y: 0}, {X: 27, Y: -7},
		{X: 24, Y: -5}, {X: 25, Y: -7}, {X: 26, Y: -6}, {X: 25, Y: -5},
		{X: 6, Y: 8}, {X: 11, Y: -2}, {X: 20, Y: -5}, {X: 29, Y: -10},
		{X: 6, Y: 3}, {X: 28, Y: -7}, {X: 8, Y: 0}, {X: 30, Y: -6},
		{X: 29, Y: -8}, {X: 20, Y: -10}, {X: 6, Y: 7}, {X: 6, Y: 4},
		{X: 6, Y: 1}, {X: 14, Y: -4}, {X: 21, Y: -6}, {X: 26, Y: -10},
		{X: 7, Y: -1}, {X: 7, Y: 7}, {X: 8, Y: -1}, {X: 21, Y: -9},
		{X: 6, Y: 2}, {X: 20, Y: -7}, {X: 30, Y: -10}, {X: 14, Y: -3},
		{X: 20, Y: -8}, {X: 13, Y: -2}, {X: 7, Y: 3}, {X: 28, Y: -8},
		{X: 29, Y: -9}, {X: 15, Y: -3}, {X: 22, Y: -5}, {X: 26, Y: -8},
		{X: 25, Y: -8}, {X: 25, Y: -6}, {X: 15, Y: -4}, {X: 9, Y: -2},
		{X: 15, Y: -2}, {X: 12, Y: -2}, {X: 28, Y: -9}, {X: 12, Y: -3},
		{X: 24, Y: -6}, {X: 23, Y: -7}, {X: 25, Y: -10}, {X: 7, Y: 8},
		{X: 11, Y: -3}, {X: 26, Y: -7}, {X: 7, Y: 1}, {X: 23, Y: -9},
		{X: 6, Y: 0}, {X: 22, Y: -10}, {X: 27, Y: -6}, {X: 8, Y: 1},
		{X: 22, Y: -8}, {X: 13, Y: -4}, {X: 7, Y: 6}, {X: 28, Y: -6},
		{X: 11, Y: -4}, {X: 12, Y: -4}, {X: 26, Y: -9}, {X: 7, Y: 4},
		{X: 24, Y: -10}, {X: 23, Y: -8}, {X: 30, Y: -8}, {X: 7, Y: 0},
		{X: 9, Y: -1}, {X: 10, Y: -1}, {X: 26, Y: -5}, {X: 22, Y: -9},
		{X: 6, Y: 5}, {X: 7, Y: 5}, {X: 23, Y: -6}, {X: 28, Y: -10},
		{X: 10, Y: -2}, {X: 11, Y: -1}, {X: 20, Y: -9}, {X: 14, Y: -2},
		{X: 29, Y: -7}, {X: 13, Y: -3}, {X: 23, Y: -5}, {X: 24, Y: -8},
		{X: 27, Y: -9}, {X: 30, Y: -7}, {X: 28, Y: -5}, {X: 21, Y: -10},
		{X: 7, Y: 9}, {X: 6, Y: 6}, {X: 21, Y: -5}, {X: 27, Y: -10},
		{X: 7, Y: 2}, {X: 30, Y: -9}, {X: 21, Y: -8}, {X: 22, Y: -7},
		{X: 24, Y: -9}, {X: 20, Y: -6}, {X: 6, Y: 9}, {X: 29, Y: -5},
		{X: 8, Y: -2}, {X: 27, Y: -8}, {X: 30, Y: -5}, {X: 24, Y: -7},
	}

	sort.Slice(actual, func(i, j int) bool {
		return actual[i].LessThan(actual[j])
	})

	sort.Slice(expected, func(i, j int) bool {
		return expected[i].LessThan(expected[j])
	})

	k := 0
	j := 0
	for k < len(actual) && j < len(expected) {
		if actual[k] == expected[j] {
			k++
			j++
		} else {
			// Mismatch - but are we missing an entry or have an extra?
			if actual[k].LessThan(expected[j]) {
				// Extra entry
				fmt.Println("Extra", actual[k])
				k++
			} else {
				fmt.Println("Missing", expected[j])
				// missing entry
				j++
			}
		}
	}

	for ; k < len(actual); k++ {
		fmt.Println("Extra", actual[k])
	}

	for ; j < len(expected); j++ {
		fmt.Println("Missing", expected[j])
	}
}

func main() {
	fmt.Println("##### Part1 #####")
	part1(exampleTarget)
	part1(realData)

	fmt.Println("##### Part2 #####")
	exampleShots := part2(exampleTarget)
	checkShots(exampleShots)
	part2(realData)
}
