package main

import (
	"github.com/alecthomas/participle/v2"
	"fmt"
	"sort"
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

func splitByPlane(vents *HydroThermalVents) (horrizontal, vertical, others []Line) {
	horrizontal = make([]Line, 0, len(vents.VentLocations))
	vertical = make([]Line, 0, len(vents.VentLocations))
	others = make([]Line, 0, len(vents.VentLocations))

	for _, line := range vents.VentLocations {
		if line.Start.X == line.End.X && line.Start.Y == line.End.Y {
			panic("Cruel - a single point, is it horrizontal or vertical?!")
		}

		switch {
		case line.Start.Y == line.End.Y:
			horrizontal = append(horrizontal, line)
		case line.Start.X == line.End.X:
			vertical = append(vertical, line)
		default:
			others = append(others, line)
		}
	}

	return
}


// Return the comparison, differentiation for the overlapping check
// eg this would return X, Y for a horrizontal check and Y, X for a vertical one
type CompareAndDiff func(Point) (int, int)

type overlapEntry struct {
	secondary int
	overlaps int
}

type OverlapTracker struct {
	overlaps []overlapEntry
}

func NewOverlapTracker() (*OverlapTracker) {
	return &OverlapTracker {
		overlaps: make([]overlapEntry, 0, 100),
	}
}

// Apply the modifier to the tracker
func (ot *OverlapTracker) Apply(secondary, modifier int) {
	at := sort.Search(len(ot.overlaps), func(i int) bool {
		return ot.overlaps[i].secondary >= secondary
	})

	if at < len(ot.overlaps) && ot.overlaps[at].secondary == secondary {
		ot.overlaps[at].overlaps += modifier
	} else {
		// Create a new entry
		ot.overlaps = append(ot.overlaps, overlapEntry{})
		copy(ot.overlaps[at+1:], ot.overlaps[at:])
		ot.overlaps[at] = overlapEntry { secondary: secondary, overlaps: modifier }
	}
}

// Count where overlaps > 1
func (ot *OverlapTracker) CountOverlaps() int {
	count := 0
	for _, overlap := range ot.overlaps {
		if overlap.overlaps > 1 {
			count++
		}
	}

	return count
}

func countOverlaps(vents []Line, fn CompareAndDiff) int {
	// Bad name - this records when each vent starts and stops
	type X struct {
		coord int // pos in the axis we're looking at
		diff  int // pos in the other axis
		modifier int // +1 or -1 to adjust number of overlapping vents
	}

	algo := make([]X, 0, len(vents)*2)


	// Populate the structure first
	for _, vent := range vents {
		start, otherAxis := fn(vent.Start)
		end, _ := fn(vent.End) // other axis is always the same
		algo = append(algo,
			X{coord: start, diff: otherAxis, modifier: 1}, // Start happens where we 'expect'
			X{coord: end, diff: otherAxis, modifier: -1})
	}

	sort.Slice(algo, func(i, j int) bool {
		// Sort by primary access first - then secondary for dedupe purposes!
		if algo[i].coord < algo[j].coord {
			return true
		} else if algo[i].coord == algo[j].coord {
			return algo[i].diff <= algo[j].diff
		} else {
			return false
		}

	})

	// Now collapse out duplicate coordinates
	k := 0
	for _, maybeDupe := range algo[1:] {
		if maybeDupe.diff == algo[k].diff && maybeDupe.coord == algo[k].coord {
			// Duplicate
			algo[k].modifier += maybeDupe.modifier
		} else {
			// New unique vent, move on the 'write' pointer and put this vent
			// here
			k++
			algo[k] = maybeDupe
		}
	}
	algo = algo[:k]

	fmt.Printf("Algo data: %+v\n", algo)

	tracker := NewOverlapTracker()

	totalOverlaps := 0

	// Sweep the axis 0 -> <end>
	lastPrimary := 0
	for _, change := range algo {
		fmt.Printf("change: %+v\n", change)
		overlappedInLastRange := tracker.CountOverlaps()
		lastJumpLen := change.coord - lastPrimary + 1
		lastPrimary = change.coord

		totalOverlaps += overlappedInLastRange * lastJumpLen

		// Always apply updats last
		tracker.Apply(change.diff, change.modifier)

	}

	return totalOverlaps
}

func part1(vents *HydroThermalVents) {
	_, vertical, _ := splitByPlane(vents)
	results := make(chan int)

	//go func() {
	//	hOverlap := countOverlaps(horrizontal, func(p Point) (int, int) { return p.X, p.Y})
	//	results <- hOverlap
	//}()

	go func() {
		vOverlap := countOverlaps(vertical, func(p Point) (int, int) { return p.Y, p.X})
		results <- vOverlap
	}()

	totalOverlaps := 0
	for i := 0; i < 1; i++ {
		totalOverlaps += <- results
	}

	fmt.Println("Found", totalOverlaps, "overlaps")
}

func main() {
	vents := &HydroThermalVents{};
	err := parser.ParseString("exampleData", exampleData, vents)
	if err != nil {
		fmt.Println("Error parsing imput:", err)
		return
	}

	part1(vents)

	fmt.Printf("%+v\n", vents)

}
