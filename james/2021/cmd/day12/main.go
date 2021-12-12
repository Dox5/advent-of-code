package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"strings"
	"unicode"
)

type Cave struct {
	Name  string
	IsBig bool

	Neighbours []*Cave
}

func isBigCave(caveName string) bool {
	// First character for start/end will be lower case and they aren't big
	asRunes := []rune(caveName)
	return unicode.IsUpper(asRunes[0])
}

func NewCave(adjList []byte) *Cave {
	scanner := bufio.NewScanner(bytes.NewReader(adjList))

	nodes := make(map[string]*Cave)

	for scanner.Scan() {
		connection := strings.Split(scanner.Text(), "-")

		for _, name := range connection {
			if _, ok := nodes[name]; !ok {
				// Need to create a new node
				nodes[name] = &Cave{
					Name:       name,
					IsBig:      isBigCave(name),
					Neighbours: make([]*Cave, 0, 4),
				}
			}
		}

		// All nodes must exist now, connect them up
		nodes[connection[0]].Neighbours = append(nodes[connection[0]].Neighbours, nodes[connection[1]])
		nodes[connection[1]].Neighbours = append(nodes[connection[1]].Neighbours, nodes[connection[0]])
	}

	// Be nice and return the graph from 'start'
	return nodes["start"]
}

type Path []*Cave

func (p Path) Show() {
	p.ShowWithDouble(nil)
}

func (p Path) ShowWithDouble(double *Cave) {
	for i, c := range p {
		if i > 0 {
			fmt.Printf(" -> ")
		}
		if c == double {
			fmt.Printf("|%v|", c.Name)
		} else {
			fmt.Printf("%v", c.Name)
		}
	}
}

// Each path needs to remember what it's seen already
type pathBuilder struct {
	path        Path
	visited     map[*Cave]int
	doubleVisit *Cave
}

func (p *pathBuilder) extend(c *Cave) *pathBuilder {
	builder := &pathBuilder{
		path:        make(Path, len(p.path)+1),
		visited:     make(map[*Cave]int),
		doubleVisit: p.doubleVisit,
	}

	copy(builder.path, p.path)
	builder.path[len(builder.path)-1] = c

	for k, v := range p.visited {
		builder.visited[k] = v
	}

	return builder
}

func (p *pathBuilder) markAsSeen(c *Cave) {
	if v, ok := p.visited[c]; ok {
		p.visited[c] = v + 1
	} else {
		p.visited[c] = 1
	}
}

// Visit c, possibly creating new path builders
func (p *pathBuilder) Visit(c *Cave, allowDouble bool) (*pathBuilder, bool) {

	var childDoubleVisit *Cave = p.doubleVisit
	if v, ok := p.visited[c]; ok {
		if allowDouble && p.doubleVisit == nil && v < 2 && c.Name != "start" && c.Name != "end" {
			// Allowed double and don't have one in the path yet, so allow this
			// node to double up and continue. Unless we've already done that
			// once before! Also not allowed to walk back through start and end
			childDoubleVisit = c
		} else {
			// Already seen this node for this path
			return nil, false
		}
	}

	if c.IsBig {
		// Just extend
		return p.extend(c), true
	} else {
		builder := p.extend(c)
		builder.markAsSeen(c)
		builder.doubleVisit = childDoubleVisit
		return builder, true
	}
}

func (p *pathBuilder) Last() *Cave {
	return p.path[len(p.path)-1]
}

// Walk the cave (starting on c) and write all paths.
// small caves may only be visited once
// Will only write paths than end on 'end'
func (c *Cave) AllPathsToEnd(complete chan Path, allowDouble bool) {
	defer close(complete)

	// Stack of paths we're building
	paths := make([]*pathBuilder, 1, 128)
	paths[0] = &pathBuilder{
		path:    append(make(Path, 0, 32), c),
		visited: make(map[*Cave]int),
	}
	paths[0].visited[c] = 1 // Not allowed to use start node again!

	for {
		if len(paths) == 0 {
			// Must have seen everything
			break
		}

		path := paths[len(paths)-1]
		paths = paths[:len(paths)-1]

		lastNode := path.Last()

		if lastNode.Name == "end" {
			// Complete path, output it
			complete <- path.path
			continue
		}

		// Build new paths from the end of this one
		for _, n := range lastNode.Neighbours {
			if builder, ok := path.Visit(n, allowDouble); ok {
				paths = append(paths, builder)
			}
		}

	}
}

func findAllPaths(adjList []byte, allowDouble bool, showPaths bool) {
	cave := NewCave(adjList)

	paths := make(chan Path, 16)
	go cave.AllPathsToEnd(paths, allowDouble)

	totalPaths := 0
	for p := range paths {
		if showPaths {
			fmt.Printf("\t")
			p.Show()
			fmt.Println()
		}

		totalPaths++
	}

	fmt.Println("Found", totalPaths, "paths to end")
}

func example1(allowDouble bool, showPaths bool) {
	var adjList = `start-A
start-b
A-c
A-b
b-d
A-end
b-end`

	fmt.Println()
	fmt.Println("##### Example1 #####")
	findAllPaths([]byte(adjList), allowDouble, showPaths)
}

func example2(allowDouble bool, showPaths bool) {
	var adjList = `dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc`

	fmt.Println()
	fmt.Println("##### Example2 #####")
	findAllPaths([]byte(adjList), allowDouble, showPaths)
}

func example3(allowDouble bool, showPaths bool) {
	var adjList = `fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW`

	fmt.Println()
	fmt.Println("##### Example3 #####")
	findAllPaths([]byte(adjList), allowDouble, showPaths)
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "12")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	fmt.Println("##### Part1 #####")
	example1(false, false)
	example2(false, false)
	example3(false, false)

	fmt.Println()
	findAllPaths(raw_input, false, false)

	fmt.Println()
	fmt.Println("##### Part2 #####")
	example1(true, false)
	example2(true, false)
	example3(true, false)

	fmt.Println()
	findAllPaths(raw_input, true, false)
}
