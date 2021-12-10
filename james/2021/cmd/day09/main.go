package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"sort"
)

var exampleData = `2199943210
3987894921
9856789892
8767896789
9899965678
`

type Point struct {
	X, Y, Height int
	Seen         bool
}

// Index X, Y - top left is origin
type PointGrid [][]Point

func runeToInt(r rune) int {
	switch r {
	case '0':
		return 0
	case '1':
		return 1
	case '2':
		return 2
	case '3':
		return 3
	case '4':
		return 4
	case '5':
		return 5
	case '6':
		return 6
	case '7':
		return 7
	case '8':
		return 8
	case '9':
		return 9
	default:
		panic(fmt.Errorf("No idea what %v is!", r))
	}
}

func NewGrid(input []byte) PointGrid {
	scanner := bufio.NewScanner(bytes.NewReader(input))

	rows := make([]string, 0, 100)
	for scanner.Scan() {
		rows = append(rows, scanner.Text())
	}

	grid := make(PointGrid, len(rows[0]))
	for i := range grid {
		grid[i] = make([]Point, len(rows))
	}

	for y, row := range rows {
		for x, h := range row {
			grid[x][y] = Point{
				X:      x,
				Y:      y,
				Height: runeToInt(h),
				Seen:   false,
			}
		}
	}

	return grid
}

// All neighbours of x, y
func (g *PointGrid) Neighbours(x, y int) []*Point {
	width := len(*g)
	height := len((*g)[0])

	neighbours := make([]*Point, 0, 4)

	if x-1 >= 0 {
		neighbours = append(neighbours, &(*g)[x-1][y])
	}

	// Check right
	if x+1 < width {
		neighbours = append(neighbours, &(*g)[x+1][y])
	}

	// Check up
	if y-1 >= 0 {
		neighbours = append(neighbours, &(*g)[x][y-1])
	}

	// Check down
	if y+1 < height {
		neighbours = append(neighbours, &(*g)[x][y+1])
	}

	return neighbours
}

func (g *PointGrid) IsLowest(x, y int) bool {
	for _, neighbour := range g.Neighbours(x, y) {
		if neighbour.Height <= (*g)[x][y].Height {
			return false
		}
	}

	return true
}

func (g *PointGrid) FindLows() []*Point {
	// Start with the dumb approach, can later use information to eliminate known
	// non-low points if needed
	lowPoints := make([]*Point, 0, 100)
	for x := 0; x < len(*g); x++ {
		for y := 0; y < len((*g)[x]); y++ {

			if (*g)[x][y].Seen {
				// Point already seen so no need to check again
				continue
			}

			// Check neighbours
			if g.IsLowest(x, y) {
				// Do more interesting things
				lowPoints = append(lowPoints, &(*g)[x][y])
			}
		}
	}

	return lowPoints
}

func part1(input []byte) {
	grid := NewGrid(input)
	lowPoints := grid.FindLows()

	totalRisk := 0
	for _, p := range lowPoints {
		totalRisk += p.Height + 1
	}

	fmt.Printf("Total risk: %v\n", totalRisk)
}

func part2(input []byte) {
	grid := NewGrid(input)
	lowPoints := grid.FindLows()

	basins := make([][]*Point, 0, 100)

	for _, basinStart := range lowPoints {
		inBasin := make([]*Point, 0, 100)

		toConsider := append(make([]*Point, 0, 100), basinStart)

		for len(toConsider) > 0 {
			p := toConsider[len(toConsider)-1]
			toConsider = toConsider[:len(toConsider)-1]

			if p.Seen {
				// Already dealt with this node through another path
				continue
			}

			p.Seen = true

			inBasin = append(inBasin, p)

			neighbours := grid.Neighbours(p.X, p.Y)

			for _, neighbour := range neighbours {
				// Already seen this one, no need to do anything more OR
				// height is 9 which belongs to no basin
				if neighbour.Seen || neighbour.Height == 9 {
					continue
				}

				if p.Height < neighbour.Height {
					// This node is also in the basin - follow the path and check
					// it's neighbours
					toConsider = append(toConsider, neighbour)
				}
			}

		}

		basins = append(basins, inBasin)

		// Seen value not reset because no overlap between basins
	}

	sort.Slice(basins, func(i, j int) bool {
		return len(basins[i]) > len(basins[j])
	})

	totalSize := 1
	for i := 0; i < 3; i++ {
		totalSize *= len(basins[i])
	}

	fmt.Println("Produce of top 3 basins:", totalSize)

}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "9")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	fmt.Println("##### Part1 #####")
	fmt.Println("   ExampleData")
	part1([]byte(exampleData))
	fmt.Println("     RealData")
	part1(raw_input)

	fmt.Println("##### Part2 #####")
	fmt.Println("   ExampleData")
	part2([]byte(exampleData))
	fmt.Println("     RealData")
	part2(raw_input)
}
