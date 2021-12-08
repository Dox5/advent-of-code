package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"strings"
)

var exampleData = `be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce`

var exampleData2 = `acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf`

// What letters could be this segment
type Segment map[rune]int

type SevenSegment struct {
	// order is
	/* 00
	      1  2
		  1  2
		   33
		  4  5
		  4  5
		   66
	*/
	segments [7]Segment
}

func NewSevenSegment() *SevenSegment {
	sevenSeg := &SevenSegment{}

	for i := range sevenSeg.segments {
		for _, c := range "abcdegf" {
			sevenSeg.segments[i][c] = 1
		}
	}

	return sevenSeg
}

type Observed struct {
	Patterns     []string
	OutputDigits []string
}

func Parse(input []byte) []Observed {
	scanner := bufio.NewScanner(bytes.NewReader(input))

	out := make([]Observed, 0, 100)

	for scanner.Scan() {
		patternsAndValue := strings.Split(scanner.Text(), "|")

		patterns := strings.Split(strings.Trim(patternsAndValue[0], " "), " ")
		outputDigits := strings.Split(strings.Trim(patternsAndValue[1], " "), " ")

		out = append(out, Observed{
			Patterns:     patterns,
			OutputDigits: outputDigits,
		})
	}

	return out
}

func count1_4_7_8(segments []Observed) int {
	total := 0

	for _, seg := range segments {
		for _, digit := range seg.OutputDigits {
			if len(digit) == 2 || len(digit) == 4 || len(digit) == 3 || len(digit) == 7 {
				total++
			}
		}
	}

	return total
}

type Pattern int

func Union(p1, p2 Pattern) Pattern {
	return p1 | p2
}

func UnionM(ps ...Pattern) Pattern {
	out := Pattern(0)
	for _, p := range ps {
		out = Union(out, p)
	}

	return out
}

func Intersection(p1, p2 Pattern) Pattern {
	return p1 & p2
}

// p1 < p2 in below notation
func IsStrictSubset(p1, p2 Pattern) bool {
	return (p1 & p2) == p1
}

func SetSize(p Pattern) int {
	count := 0
	for p != 0 {
		count += int(p & 1)
		p >>= 1
	}

	return count
}

// p1 - p2 in below notation
func Difference(p1, p2 Pattern) Pattern {
	inBoth := Intersection(p1, p2)
	return p1 ^ inBoth
}

func ToPattern(strPat string) Pattern {
	pattern := Pattern(0)
	for _, c := range strPat {
		switch c {
		case 'a':
			pattern |= 0b0000001
		case 'b':
			pattern |= 0b0000010
		case 'c':
			pattern |= 0b0000100
		case 'd':
			pattern |= 0b0001000
		case 'e':
			pattern |= 0b0010000
		case 'f':
			pattern |= 0b0100000
		case 'g':
			pattern |= 0b1000000
		default:
			panic(fmt.Errorf("wtf is %v", c))
		}
	}

	if pattern == 0 {
		panic(fmt.Errorf("Input %v produced 0 at pattern", strPat))
	}

	return pattern
}

func FromPattern(p Pattern) string {
	b := strings.Builder{}

	if p&0b0000001 != 0 {
		b.WriteString("a")
	}

	if p&0b0000010 != 0 {
		b.WriteString("b")
	}

	if p&0b0000100 != 0 {
		b.WriteString("c")
	}

	if p&0b0001000 != 0 {
		b.WriteString("d")
	}

	if p&0b0010000 != 0 {
		b.WriteString("e")
	}

	if p&0b0100000 != 0 {
		b.WriteString("f")
	}

	if p&0b1000000 != 0 {
		b.WriteString("g")
	}

	return b.String()
}

func convert(ps []string) []Pattern {
	out := make([]Pattern, len(ps))
	for i, strPat := range ps {
		out[i] = ToPattern(strPat)
	}

	return out
}

func findKnownAndRemove(patterns []Pattern) (one, four, seven, eight Pattern, remaining []Pattern) {
	out := 0
	for _, pattern := range patterns {
		switch SetSize(pattern) {
		case 2:
			one = pattern
		case 3:
			seven = pattern
		case 4:
			four = pattern
		case 7:
			eight = pattern
		default:
			patterns[out] = pattern
			out++
		}
	}

	remaining = patterns[:out]
	return
}

type PatTest func(Pattern) bool

func findSatisfying(patterns []Pattern, test PatTest) (Pattern, []Pattern) {
	out := 0
	found := Pattern(0)
	for _, pattern := range patterns {
		if test(pattern) {
			found = pattern
		} else {
			patterns[out] = pattern
			out++
		}
	}

	patterns = patterns[:out]

	return found, patterns
}

// 0 through 9, each pattern is how the number is represented
type CodeBook []Pattern

/*
	notation:
		{X} = set of signals that represent a digit
		A < B = A is strict subset of B
		A u B = union - all elements from both
		A n B = intersection - elements only in both
		A - B = A without the elements that also appear in B
		s(A) = size of the set

*/

func breakCode(observed Observed) CodeBook {
	unknowns := convert(observed.Patterns)
	// First gather the starting facts
	one, four, seven, eight, unknowns := findKnownAndRemove(unknowns)

	// a = {7} - {1}
	a := Difference(seven, one)

	// find 3 - only digit that satisfies (s(X) == 5 && {1} < X )
	three, unknowns := findSatisfying(unknowns, func(p Pattern) bool {
		return IsStrictSubset(one, p) && SetSize(p) == 5
	})

	// d = ({3} - {7}) n {4}
	threeSevenDiff := Difference(three, seven)
	d := Intersection(threeSevenDiff, four)

	// g = ({3} - {7}) - d
	g := Difference(threeSevenDiff, d)

	// b = (({4} - d) - {7})
	b := Difference(Difference(four, d), seven)

	// find 0 - {8} - d
	zero := Difference(eight, d)

	// find 5 - only (remaining) digit that satisfies (s(X) == 5 && b < X)
	five, unknowns := findSatisfying(unknowns, func(p Pattern) bool {
		return SetSize(p) == 5 && IsStrictSubset(b, p)
	})

	// find 2 - only remainging digit that satisfies (s(X) == 5)
	two, unknowns := findSatisfying(unknowns, func(p Pattern) bool {
		return SetSize(p) == 5
	})

	// c = {2} n {1}
	c := Intersection(two, one)

	// f = {1} - c
	f := Difference(one, c)

	// e = {2} - {a, b, c, d, f, g}
	e := Difference(two, UnionM(a, b, c, d, f, g))

	return CodeBook{
		zero,
		one,
		two,
		three,
		four,
		five,
		UnionM(a, b, d, e, f, g),
		seven,
		eight,
		UnionM(a, b, c, d, f, g),
	}

}

func part1(segments []Observed) {
	fmt.Println("1,4,7,8 count:", count1_4_7_8(segments))
}

func decode(codeBook CodeBook, digits []string) int {
	asPatterns := convert(digits)
	decodedValue := 0

	for _, value := range asPatterns {
		for decimal, code := range codeBook {
			if value == code {
				decodedValue *= 10
				decodedValue += decimal
				break
			}
		}
	}

	return decodedValue
}

func part2(observations []Observed) {
	total := 0

	for _, obs := range observations {
		codeBook := breakCode(obs)
		total += decode(codeBook, obs.OutputDigits)
	}

	fmt.Println("Total of output values:", total)
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "8")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	exampleSegments := Parse([]byte(exampleData))
	exampleSegments2 := Parse([]byte(exampleData2))
	segments := Parse(raw_input)

	fmt.Println("##### Part1 #####")
	fmt.Println("   ExampleData")
	part1(exampleSegments)
	fmt.Println("    RealData")
	part1(segments)

	fmt.Println("##### Part2 #####")
	fmt.Println("ExampleData Small")
	part2(exampleSegments2)
	fmt.Println("ExampleData Large")
	part2(exampleSegments)
	part2(segments)
}
