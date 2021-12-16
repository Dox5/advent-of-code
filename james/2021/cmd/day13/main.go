package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"io"
	"sort"
	"strconv"
	"strings"
)

var exampleData = `6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5`

type Point struct {
	X, Y int
}

type Plane int

type Paper struct {
	Dots          []Point
	Width, Height int
}

const (
	Horrizontal Plane = iota
	Virtical
)

type Fold struct {
	Pl  Plane
	Pos int
}

func parsePoints(scanner *bufio.Scanner) ([]Point, error) {
	points := make([]Point, 0, 128)

	for scanner.Scan() {
		if scanner.Text() == "" {
			break
		}
		coords := strings.Split(strings.Trim(scanner.Text(), " \n"), ",")
		if len(coords) != 2 {
			return nil, fmt.Errorf("Malformed point %q expected two parts", scanner.Text())
		}

		intCoords := make([]int, 2)
		for i, c := range coords {
			v, err := strconv.ParseInt(c, 10, 32)
			if err != nil {
				return nil, err
			}
			intCoords[i] = int(v)
		}

		points = append(points, Point{X: intCoords[0], Y: intCoords[1]})
	}

	return points, nil
}

func parseFolds(scanner *bufio.Scanner) ([]Fold, error) {
	folds := make([]Fold, 0, 32)
	for scanner.Scan() {
		parts := strings.Split(strings.Trim(scanner.Text(), " \n"), " ")
		if len(parts) != 3 {
			return nil, fmt.Errorf("Malformed fold %q expected 3 parts", scanner.Text())
		}

		// Just the <plane>=<pos> part
		planeAndPos := strings.Split(parts[2], "=")
		if len(planeAndPos) != 2 {
			return nil, fmt.Errorf("Malformed fold part %q expected x=3 form", parts[2])
		}

		var plane Plane
		if planeAndPos[0] == "x" {
			// All points along fold have same X - so virtical
			plane = Virtical
		} else if planeAndPos[0] == "y" {
			// All points along fold have same Y - so horrizontal
			plane = Horrizontal
		} else {
			return nil, fmt.Errorf("Don't know what axis %q is", planeAndPos[0])
		}

		pos, err := strconv.ParseInt(planeAndPos[1], 10, 32)
		if err != nil {
			return nil, err
		}

		folds = append(folds, Fold{Pl: plane, Pos: int(pos)})
	}

	return folds, nil
}

func NewPaper(dots []Point) *Paper {
	// Find bounds
	width := 0
	height := 0
	for _, p := range dots {
		if p.X > width {
			width = p.X
		}

		if p.Y > height {
			height = p.Y
		}
	}

	// Found the furthest most coordinates - length will be one more!
	width++
	height++

	return &Paper{
		Dots:   dots,
		Width:  width,
		Height: height,
	}
}

func (p *Paper) sort() {
	sort.Slice(p.Dots, func(i, j int) bool {
		// Lower Y always comes first
		if p.Dots[i].Y < p.Dots[j].Y {
			return true
		} else if p.Dots[i].Y == p.Dots[j].Y {
			// Then lower X
			if p.Dots[i].X < p.Dots[j].X {
				return true
			}
		}

		return false
	})
}

func (p *Paper) String() string {
	// Sort points into a sane order for printing. Top left points first
	p.sort()

	nextPoint := 0

	builder := strings.Builder{}

	for y := 0; y < p.Height; y++ {
		for x := 0; x < p.Width; x++ {
			// Determine how to draw this point
			if nextPoint < len(p.Dots) && p.Dots[nextPoint].X == x && p.Dots[nextPoint].Y == y {
				// Need to print a # as this is a dot
				builder.WriteRune('#')

				// Move to look at the next point that isn't an overlap
				for nextPoint < len(p.Dots) && p.Dots[nextPoint].X == x && p.Dots[nextPoint].Y == y {
					nextPoint++
				}
			} else {
				// Boring!
				builder.WriteRune('.')
			}
		}
		builder.WriteRune('\n')
	}

	return builder.String()
}

func (p *Paper) dedupe() {
	// Like elements needs to be next to eachother
	p.sort()

	k := 0
	for i := 1; i < len(p.Dots); i++ {
		if p.Dots[k] != p.Dots[i] {
			k++
			p.Dots[k] = p.Dots[i]
		}
	}

	p.Dots = p.Dots[:k+1]
}

func (p *Paper) FoldAlong(fold Fold) {
	// Folding achieved by
	// 1. filtering to points in the 'fold zone' (coord > foldLine)
	// 2. find distance to the 'end' of the paper
	// 3. use this distance as the new coord in that axis

	var planeLen int
	if fold.Pl == Horrizontal {
		planeLen = p.Height
		p.Height = (p.Height - 1) / 2 // Losing a line to the fold
	} else {
		planeLen = p.Width
		p.Width = (p.Width - 1) / 2 // Losing a line to the fold
	}

	for i := range p.Dots {
		var mag int
		if fold.Pl == Horrizontal {
			mag = p.Dots[i].Y
		} else {
			mag = p.Dots[i].X
		}

		// No dots ever on fold line, dots less than fold line are left alone
		if mag < fold.Pos {
			continue
		}

		// Calculate new position!
		newPos := planeLen - mag - 1

		// Apply
		if fold.Pl == Horrizontal {
			p.Dots[i].Y = newPos
		} else {
			p.Dots[i].X = newPos
		}
	}

	p.dedupe()
}

func parsePuzzle(input io.Reader) (*Paper, []Fold, error) {
	scanner := bufio.NewScanner(input)

	dots, err := parsePoints(scanner)
	if err != nil {
		return nil, nil, err
	}

	folds, err := parseFolds(scanner)
	if err != nil {
		return nil, nil, err
	}

	paper := NewPaper(dots)
	return paper, folds, nil
}

func part1(input io.Reader, show bool) {
	paper, folds, err := parsePuzzle(input)

	if err != nil {
		panic(err)
	}

	fmt.Println("### Part1 ###")

	if show {
		fmt.Println("## Starting paper ##")
		fmt.Println(paper)
	}

	paper.FoldAlong(folds[0])

	if show {
		fmt.Println("## Paper after 1 fold ##")
		fmt.Println(paper)
	}

	fmt.Println("Dots after 1 fold:", len(paper.Dots))
}

func part2(input io.Reader, show bool) {
	paper, folds, err := parsePuzzle(input)

	if err != nil {
		panic(err)
	}

	fmt.Println("### Part2 ###")

	if show {
		fmt.Println("## Starting paper ##")
		fmt.Println(paper)
	}

	for i, fold := range folds {
		paper.FoldAlong(fold)

		if show {
			fmt.Printf("## Paper after %v fold ##\n", i+1)
			fmt.Println(paper)
		}
	}

	fmt.Println("Final paper")
	fmt.Println(paper)

}

func main() {
	fmt.Println("##### Example Data #####")
	part1(strings.NewReader(exampleData), true)
	part2(strings.NewReader(exampleData), true)

	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "13")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	fmt.Println("##### Real Data #####")
	part1(bytes.NewReader(raw_input), false)
	part2(bytes.NewReader(raw_input), false)
}
