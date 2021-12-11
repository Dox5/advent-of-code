package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
)

var exampleData = `5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526`

type Point struct {
	X, Y, Energy int
	flashed      bool
}

// Index X, Y - top left is origin
type PointGrid [][]Point

type Simulation struct {
	grid PointGrid
}

type PointStack struct {
	stack []*Point
}

func (p *PointStack) Pop() (point *Point, ok bool) {
	if len(p.stack) > 0 {
		point = p.stack[len(p.stack)-1]
		ok = true
		p.stack = p.stack[:len(p.stack)-1]
		return
	} else {
		ok = false
		point = nil
		return
	}
}

func NewPointStack() PointStack {
	return PointStack{
		stack: make([]*Point, 0, 128),
	}
}

func (p *PointStack) Push(point *Point) {
	p.stack = append(p.stack, point)
}

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

func NewSimulation(input []byte) *Simulation {
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
		for x, e := range row {
			grid[x][y] = Point{
				X:       x,
				Y:       y,
				Energy:  runeToInt(e),
				flashed: false,
			}
		}
	}

	return &Simulation{grid: grid}
}

func (s *Simulation) incrementGrid() {
	for x, row := range s.grid {
		for y, _ := range row {
			s.grid[x][y].Energy++
		}
	}
}

// bound defined as [min, max)
func inBound(min, v, max int) bool {
	return v >= min && v < max
}

func (s *Simulation) pushNeighbours(x, y int, stack *PointStack) {
	width := len(s.grid)
	height := len(s.grid[0])

	for xAdj := -1; xAdj < 2; xAdj++ {
		for yAdj := -1; yAdj < 2; yAdj++ {
			if inBound(0, x+xAdj, width) && inBound(0, y+yAdj, height) {
				stack.Push(&s.grid[x+xAdj][y+yAdj])
			}
		}
	}
}

func (s *Simulation) reset() {
	for x, row := range s.grid {
		for y, _ := range row {
			if s.grid[x][y].flashed {
				s.grid[x][y].flashed = false
			}
		}
	}
}

func (s *Simulation) Step() (flashes int) {
	// Remember points that need to be incremented
	incrementStack := NewPointStack()
	flashes = 0

	// Everything needs to increment to start with
	for x, row := range s.grid {
		for y, _ := range row {
			incrementStack.Push(&s.grid[x][y])
		}
	}

	// stack now contains all nodes that may need further incrementing
	for {
		point, ok := incrementStack.Pop()
		if !ok {
			// Drained the stack - step complete
			break
		}

		if point.flashed {
			// Already flashed via another route - nothing to do
			continue
		}

		if point.Energy == 9 {
			// This increment causes a flash
			// Remember neighbours for increments later
			s.pushNeighbours(point.X, point.Y, &incrementStack)

			// mark this node as flashed
			point.flashed = true
			point.Energy = 0
			flashes++
		} else {
			point.Energy++
		}
	}

	s.reset()
	return
}

func (s *Simulation) Show() {
	for y, _ := range s.grid[0] {
		for x, _ := range s.grid {
			fmt.Printf("%d", s.grid[x][y].Energy)
		}
		fmt.Printf("\n")
	}
}

func (s *Simulation) OctoCount() int {
	return len(s.grid) * len(s.grid[0])
}

func findSyncStep(input []byte) int {
	sim := NewSimulation([]byte(input))

	octoCount := sim.OctoCount()

	step := 0

	for {
		flashes := sim.Step()
		step++

		if flashes == octoCount {
			return step
		}
	}
}

func runTiny() {
	var data = `11111
19991
19191
19991
11111`

	exampleSim := NewSimulation([]byte(data))
	for i := 0; i < 3; i++ {
		exampleSim.Show()
		exampleSim.Step()
		fmt.Println("---------")
	}
}

func countFlashes(input []byte, steps int) int {
	totalFlashes := 0
	sim := NewSimulation([]byte(input))
	for i := 0; i < steps; i++ {
		totalFlashes += sim.Step()
	}

	return totalFlashes
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "11")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return

	}

	fmt.Println("   ExampleData")
	fmt.Println(" flashes:", countFlashes([]byte(exampleData), 100))
	fmt.Println(" sync on:", findSyncStep([]byte(exampleData)))
	fmt.Println("     RealData")
	fmt.Println(" flashes:", countFlashes(raw_input, 100))
	fmt.Println(" sync on:", findSyncStep(raw_input))

}
