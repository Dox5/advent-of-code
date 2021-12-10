package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"sort"
)

var chunkMap = map[byte]byte{
	'{': '}',
	'[': ']',
	'<': '>',
	'(': ')',
}

var scoreTable = map[byte]int{
	'}': 1197,
	']': 57,
	'>': 25137,
	')': 3,
}

//var exampleData = `{([(<{}[<>[]}>{[]{[(<()>`
var exampleData = `[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]`

type ParseResult struct {
	Remaining string
}

type ParseState struct {
	closerStack []byte
}

// Put an expected closer onto the stack
func (s *ParseState) PushCloser(b byte) {
	s.closerStack = append(s.closerStack, b)
}

func (s *ParseState) PeekCloser() (byte, bool) {
	if len(s.closerStack) == 0 {
		return 0, false
	} else {
		return s.closerStack[len(s.closerStack)-1], true
	}
}

func (s *ParseState) PopCloser() {
	s.closerStack = s.closerStack[:len(s.closerStack)-1]
}

type UnexpectedCharacterError struct {
	Found byte
}

func (err UnexpectedCharacterError) Error() string {
	return fmt.Sprintf("Unexpected character found %q", err.Found)
}

type UnxpectedEol struct{}

func (err UnxpectedEol) Error() string {
	return "Unxpected end of input"
}

// Very simple recursive decent parser
func parse(str string, state *ParseState) (ParseResult, error) {
	chunkOpen := str[0]

	expectedClose, ok := chunkMap[chunkOpen]
	if !ok {
		// Unexpected character
		return ParseResult{Remaining: str}, UnexpectedCharacterError{Found: str[0]}
	}
	state.PushCloser(expectedClose)

	if len(str) == 1 {
		// Only enough space for the opener - unexpected end of line
		return ParseResult{}, UnxpectedEol{}
	} else {
		// Chunks can be concatinated so try and parse until we can complete

		res := ParseResult{Remaining: str[1:]}

		for {
			// Hit EOL
			if len(res.Remaining) == 0 {
				// Ran out of input
				return res, UnxpectedEol{}
			}

			// Found the matching closer
			if expected, ok := state.PeekCloser(); ok && expected == res.Remaining[0] {
				state.PopCloser()
				return ParseResult{Remaining: res.Remaining[1:]}, nil
			}

			// must need to parse more (concatinated chunks)
			var err error
			res, err = parse(res.Remaining, state)

			if err != nil {
				return res, err
			}
		}
	}
}

func autocomplete(state *ParseState) string {
	closers := make([]byte, 0, 10)

	for {
		closer, ok := state.PeekCloser()
		if !ok {
			break
		}
		closers = append(closers, closer)

		state.PopCloser()
	}

	return string(closers)
}

func scoreAutocompletion(s string) int {
	scoring := map[byte]int{
		')': 1,
		']': 2,
		'}': 3,
		'>': 4,
	}

	score := 0

	for i := range s {
		score *= 5
		if s, ok := scoring[s[i]]; ok {
			score += s
		}
	}

	return score
}

func part1(input []byte) {
	scanner := bufio.NewScanner(bytes.NewReader(input))

	totalCorruptScore := 0
	autoCompleteScores := make([]int, 0, 100)

	for scanner.Scan() {
		fmt.Println("Checking", scanner.Text())
		remaining := ParseResult{Remaining: scanner.Text()}
		for len(remaining.Remaining) > 0 {
			// Parse as much as we can in one go - there may be more so loop until
			// either an error or everything consumed
			var err error
			state := ParseState{}
			remaining, err = parse(remaining.Remaining, &state)
			if err != nil {
				if _, ok := err.(UnxpectedEol); ok {
					fmt.Println("\t- unexpected EOL")
					closers := autocomplete(&state)
					autoCompleteScores = append(autoCompleteScores, scoreAutocompletion(closers))
				} else if charErr, ok := err.(UnexpectedCharacterError); ok {
					fmt.Println("\t- Unexpected character error:", charErr)
					if score, ok := scoreTable[charErr.Found]; ok {
						totalCorruptScore += score
					} else {
						panic(fmt.Sprintf("Can't score %q", charErr.Found))
					}
				} else {
					panic(err)
				}

				// Found error so stop parsing
				break
			}
		}
	}

	fmt.Println("Total corrupt score:", totalCorruptScore)
	sort.Ints(autoCompleteScores)
	middle := (len(autoCompleteScores) - 1) / 2 // Guarenteed that there is an odd number
	fmt.Println("Autocomplete score:", autoCompleteScores[middle])
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "10")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	fmt.Println("##### Part1 #####")
	part1([]byte(exampleData))
	part1(raw_input)
}
