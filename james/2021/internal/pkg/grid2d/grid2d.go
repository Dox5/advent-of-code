package grid2d

import (
	"fmt"
)

type Grid2D struct {
	Width, Height int
}

type Point2D struct {
	X, Y int
}

type Directions int

type Path2D []Point2D

const (
	Cardinal Directions = 0
	CardinalAndDiagnol
)

func NewGrid2D(width, height int) *Grid2D {
	return &Grid2D{
		Width:  width,
		Height: height,
	}
}

// bound defined as [min, max)
func inBound(min, v, max int) bool {
	return v >= min && v < max
}

func (g Grid2D) Neighbours(of Point2D, directions Directions) (neighbours []Point2D) {
	neighbours = make([]Point2D, 0, 4)

	for xAdj := -1; xAdj < 2; xAdj++ {
		for yAdj := -1; yAdj < 2; yAdj++ {
			if directions == Cardinal && xAdj != 0 && yAdj != 0 {
				// Not looking at diagnols
				continue
			}

			neighbour := Point2D{X: of.X + xAdj, Y: of.Y + yAdj}

			if inBound(0, neighbour.X, g.Width) && inBound(0, neighbour.Y, g.Height) {
				neighbours = append(neighbours, neighbour)
			}
		}
	}

	return
}

// Given two nodes - return the cost of moving between them
type CostCalculator func(from, to Point2D) int

func (g Grid2D) ShortestPath(from Point2D,
	to Point2D,
	directions Directions,
	costOf CostCalculator) (int, error) {

	toExplore := newPriorityQueue(g.Width, g.Height)
	toExplore.updateLower(from, 0) // first node costs nothing

	costToNode := make(map[Point2D]int) // Track best-cost so far to avoid expensive
	// walks
	visited := make(map[Point2D]bool)

	for {
		point, cheapest, ok := toExplore.pop()

		if !ok {
			// costs is now populated
			break
		}

		// Visiting our target node - this has to be the cheapest way to get
		// here
		if point == to {
			return cheapest, nil
		}

		visited[point] = true

		for _, n := range g.Neighbours(point, directions) {
			if _, ok := visited[n]; !ok {
				cost := costOf(point, n) + cheapest

				// Not yet visited - update if this is the cheapest path
				if c, ok := costToNode[n]; ok && c < cost {
					// More expensive that best known so give up
					continue
				}
				// Either no cost recorded yet or cheapest - update queue
				costToNode[n] = cost
				toExplore.updateLower(n, cost)
			}
		}
	}

	panic("Meant to be unreachable")
}

func (g *Grid2D) Show(value func(p Point2D) int) {
	for y := 0; y < g.Height; y++ {
		for x := 0; x < g.Width; x++ {
			fmt.Printf("%d", value(Point2D{X: x, Y: y}))
		}
		fmt.Printf("\n")
	}
}
