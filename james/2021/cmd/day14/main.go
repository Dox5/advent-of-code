package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"io"
	"math"
	"sort"
	"strings"
)

type PolymerPair struct {
	L, R byte
}

func (p PolymerPair) LessThanEqualTo(other PolymerPair) bool {
	if p.L < other.L {
		return true
	} else if p.L > other.L {
		return false
	} else {
		// Second criteria
		if p.R < other.R {
			return true
		} else if p.R > other.R {
			return false
		} else {
			// Exact match
			return true
		}
	}
}

type InsertionRule struct {
	PolymerPair string // Matching pair
	Polymer     string // Single insertion
}

var exampleData = `NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C`

type PolymerLookupTable struct {
	pairs   []PolymerPair
	inserts []byte
}

func NewPolymerLookupTable(rules []InsertionRule) *PolymerLookupTable {
	table := &PolymerLookupTable{
		pairs:   make([]PolymerPair, len(rules)),
		inserts: make([]byte, len(rules)),
	}

	local := make([]InsertionRule, len(rules))
	copy(local, rules)

	sort.Slice(local, func(i, j int) bool {
		return local[i].PolymerPair < local[j].PolymerPair
	})

	for i, rule := range local {
		table.pairs[i].L = rule.PolymerPair[0]
		table.pairs[i].R = rule.PolymerPair[1]

		table.inserts[i] = rule.Polymer[0]
	}

	return table
}

func (t *PolymerLookupTable) Lookup(pair PolymerPair) (byte, bool) {
	at := sort.Search(len(t.pairs), func(i int) bool {
		return pair.LessThanEqualTo(t.pairs[i])
	})

	if at < len(t.pairs) && t.pairs[at] == pair {
		return t.inserts[at], true
	} else {
		return 0, false
	}
}

func parse(reader io.Reader) (string, []InsertionRule) {
	scanner := bufio.NewScanner(reader)

	if !scanner.Scan() {
		panic("Missing template!")
	}
	polymerTemplate := strings.Trim(scanner.Text(), " \n")

	insertRules := make([]InsertionRule, 0, 128)

	for scanner.Scan() {
		ruleSpec := strings.Trim(scanner.Text(), " \n")
		if ruleSpec == "" {
			continue
		}

		ruleAndInsert := strings.Split(ruleSpec, " -> ")

		insertRules = append(insertRules,
			InsertionRule{
				PolymerPair: ruleAndInsert[0],
				Polymer:     ruleAndInsert[1]})

	}

	return polymerTemplate, insertRules
}

func scorePolymer(counters MolCounts) uint64 {
	var min, max uint64
	min = math.MaxUint64
	max = 0

	for _, count := range counters {
		if count > max {
			max = count
		}

		if count < min {
			min = count
		}
	}

	return max - min
}

func recurse(root PolymerPair, growTable *PolymerLookupTable, cache *PolymerCountCache, steps int) MolCounts {
	if counts, ok := cache.Lookup(root, steps); ok {
		// Already done this pair & depth, use cached value
		return counts
	}

	if steps == 0 {
		// Hit the bottom - no more steps to take
		counts := make(MolCounts)
		// Only count things on the right to avoid duplicates
		counts[root.R] = 1

		return counts
	}

	mol, ok := growTable.Lookup(root)
	if !ok {
		panic("No growing?!")
	}

	// Walk both the left and right tree of pairs and calculate the cache

	leftCount := recurse(PolymerPair{L: root.L, R: mol}, growTable, cache, steps-1)
	rightCount := recurse(PolymerPair{L: mol, R: root.R}, growTable, cache, steps-1)

	counts := make(MolCounts)
	for k, v := range leftCount {
		counts[k] = v
	}

	for k, v := range rightCount {
		if c, ok := counts[k]; ok {
			counts[k] = v + c
		} else {
			counts[k] = v
		}
	}

	// Remember this count
	cache.Put(root, steps, counts)
	return counts
}

func countMolecules(template string, growTable *PolymerLookupTable, cache *PolymerCountCache, steps int) MolCounts {
	counts := make([]MolCounts, len(template)-1)

	for i := range counts {
		counts[i] = recurse(PolymerPair{L: template[i],
			R: template[i+1]},
			growTable,
			cache,
			steps)
	}

	// Collapse counts
	for i := 1; i < len(counts); i++ {
		for k, v := range counts[i] {
			if c, ok := counts[0][k]; ok {
				counts[0][k] = c + v
			} else {
				counts[0][k] = v
			}
		}
	}

	// Account for the left most molecule of the template (counting is done on
	// all the right hand nodes)
	counts[0][template[0]]++

	return counts[0]
}

func runPuzzle(reader io.Reader, steps int) {
	template, rules := parse(reader)
	growTable := NewPolymerLookupTable(rules)
	cache := &PolymerCountCache{}

	counts := countMolecules(template, growTable, cache, steps)
	fmt.Println("Score is:", scorePolymer(counts))
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "14")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	fmt.Println("   ExampleData")
	runPuzzle(strings.NewReader(exampleData), 10)
	runPuzzle(strings.NewReader(exampleData), 40)

	fmt.Println("     RealData")
	runPuzzle(bytes.NewReader(raw_input), 10)
	runPuzzle(bytes.NewReader(raw_input), 40)
}
