package main

import (
	"bufio"
	"io"
	"strings"
	//"os"
	"bytes"
	//"time"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"github.com/Dox5/advent-of-code/james/2021/internal/pkg/grid2d"
	//"runtime/pprof"
	//"sync"
)

var exampleData = `1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581`

// Index X, Y - top left is origin
type RiskGrid [][]int

type CaveMap struct {
	Risk   RiskGrid
	Layout *grid2d.Grid2D
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
		panic(fmt.Sprintf("No idea what %v is!", r))
	}
}

func NewCaveMap(reader io.Reader) *CaveMap {
	scanner := bufio.NewScanner(reader)

	rows := make([]string, 0, 100)
	for scanner.Scan() {
		rows = append(rows, scanner.Text())
	}

	grid := make(RiskGrid, len(rows[0]))
	for i := range grid {
		grid[i] = make([]int, len(rows))
	}

	for y, row := range rows {
		for x, r := range row {
			grid[x][y] = runeToInt(r)
		}
	}

	return &CaveMap{Risk: grid,
		Layout: grid2d.NewGrid2D(len(grid), len(grid[0]))}
}

func shortestPathRisk(cave *CaveMap) int {
	costOf := func(from, to grid2d.Point2D) int {
		return cave.Risk[to.X][to.Y]
	}

	cheapest, err := cave.Layout.ShortestPath(
		grid2d.Point2D{X: 0, Y: 0},
		grid2d.Point2D{X: cave.Layout.Width - 1, Y: cave.Layout.Height - 1},
		grid2d.Cardinal,
		costOf,
	)

	if err != nil {
		panic(err)
	}

	return cheapest
}

func buildHugeCave(cave *CaveMap) *CaveMap {
	multipler := 5

	width := cave.Layout.Width * multipler
	height := cave.Layout.Height * multipler

	grid := make(RiskGrid, cave.Layout.Width*multipler)
	for i := range grid {
		grid[i] = make([]int, cave.Layout.Height*multipler)
	}

	for x := 0; x < width; x++ {
		for y := 0; y < height; y++ {
			sourceY := y % cave.Layout.Height
			sourceX := x % cave.Layout.Width

			// Convert to range of [0, 9) then mod 9 then add one to get back
			// into the range[1, 10) skipping 0
			risk := (cave.Risk[sourceX][sourceY] - 1) + (y / cave.Layout.Height) + (x / cave.Layout.Width)
			grid[x][y] = (risk % 9) + 1
		}
	}

	return &CaveMap{
		Risk:   grid,
		Layout: grid2d.NewGrid2D(len(grid), len(grid[0])),
	}
}

//func profile(wg *sync.WaitGroup) {
//	f, err := os.Create("cpu.pprof")
//	if err != nil {
//		panic(err)
//	}
//	defer f.Close()
//
//	// Wait for other thread to be ready
//	wg.Done()
//	wg.Wait()
//
//	if err := pprof.StartCPUProfile(f) ; err != nil {
//		panic(err)
//	}
//
//	defer pprof.StopCPUProfile()
//
//	time.Sleep(15 * time.Second)
//	panic("Got sample")
//}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "15")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	exampleCave := NewCaveMap(strings.NewReader(exampleData))
	hugeExample := buildHugeCave(exampleCave)

	fmt.Println("##### ExampleData ####")
	fmt.Println("Lowest risk cost to exit:", shortestPathRisk(exampleCave))
	fmt.Println("Lowest risk cost to exit (huge):", shortestPathRisk(hugeExample))

	fmt.Println("##### RealData ####")
	cave := NewCaveMap(bytes.NewReader(raw_input))
	hugeCave := buildHugeCave(cave)
	fmt.Println("Lowest risk cost to exit:", shortestPathRisk(cave))
	//var wg sync.WaitGroup
	//wg.Add(1)

	//go profile(&wg)

	//wg.Wait()
	fmt.Println("Lowest risk cost to exit (huge):", shortestPathRisk(hugeCave))
}
