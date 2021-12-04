package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"sort"
	"strconv"
	"strings"
)

const (
	ROW_WIDTH  = 5
	COL_HEIGHT = 5
)

var exampleData = `7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7`

type BoardTrackerEntry struct {
	rowMatches []int
	colMatches []int
}

type BoardTracker struct {
	index      *BingoIndex
	trackers   []BoardTrackerEntry
	rows, cols int
}

type BoardCoord struct {
	board int
	row   int
	col   int
}

type BingoIndexEntry struct {
	bingoNum int
	boards   []BoardCoord
}

// This really probably should've contained a [int][]BoardCoord map...
// oh well
type BingoIndex struct {
	entries []BingoIndexEntry
}

type Board struct {
	eliminated bool
	rows       []Row
}

type Row struct {
	nums []int
}

func (bi *BingoIndex) findEntryFor(num int) int {
	return sort.Search(len(bi.entries), func(i int) bool {
		return bi.entries[i].bingoNum >= num
	})
}

func NewIndex(boards []Board) *BingoIndex {
	index := &BingoIndex{}

	for boardIndex, board := range boards {
		for rowIndex, row := range board.rows {
			for colIndex, num := range row.nums {
				coord := BoardCoord{row: rowIndex, col: colIndex, board: boardIndex}
				index.Insert(num, coord)
			}
		}
	}

	return index
}

func (bi *BingoIndex) Insert(num int, coord BoardCoord) {
	at := bi.findEntryFor(num)

	if at < len(bi.entries) && bi.entries[at].bingoNum == num {
		// Found it, add this to the list of boards to update on drawing
		// this number
		bi.entries[at].boards = append(bi.entries[at].boards, coord)
	} else {
		// It goes here
		bi.entries = append(bi.entries, BingoIndexEntry{})
		copy(bi.entries[at+1:], bi.entries[at:])
		bi.entries[at] = BingoIndexEntry{bingoNum: num, boards: make([]BoardCoord, 1, 16)}
		bi.entries[at].boards[0] = coord
	}
}

func (bi *BingoIndex) Called(called int) []BoardCoord {
	at := bi.findEntryFor(called)
	if at < len(bi.entries) && bi.entries[at].bingoNum == called {
		return bi.entries[at].boards
	} else {
		panic(fmt.Errorf("Asked for bingo number %v but it wasn't in the index", called))
	}
}

func (bi *BingoIndex) DropBoard(boardIndex int) {
	for entryIndex := range bi.entries {
		i := 0
		for _, coord := range bi.entries[entryIndex].boards {
			if coord.board != boardIndex {
				bi.entries[entryIndex].boards[i] = coord
				i++
			}
		}
		bi.entries[entryIndex].boards = bi.entries[entryIndex].boards[:i]
	}
}

func NewBoardTracker(boards []Board) *BoardTracker {
	tracker := &BoardTracker{
		index:    NewIndex(boards),
		trackers: make([]BoardTrackerEntry, len(boards)),
		rows:     len(boards[0].rows),
		cols:     len(boards[0].rows[0].nums),
	}

	for i := range tracker.trackers {
		tracker.trackers[i].rowMatches = make([]int, tracker.rows)
		tracker.trackers[i].colMatches = make([]int, tracker.cols)
	}

	return tracker
}

// Make a call
func (bt *BoardTracker) Mark(call int) (winningBoards []int) {
	marks := bt.index.Called(call)
	winningBoards = make([]int, 0, 8)

	for _, toMark := range marks {
		tracker := bt.trackers[toMark.board]
		tracker.rowMatches[toMark.row]++
		tracker.colMatches[toMark.col]++

		if tracker.rowMatches[toMark.row] == bt.rows ||
			tracker.colMatches[toMark.col] == bt.cols {
			winningBoards = append(winningBoards, toMark.board)
		}
	}

	// Dedupe any double matches (eg this move made a board win in row and
	// column!
	sort.Ints(winningBoards)
	k := 1
	for i := 1; i < len(winningBoards); i++ {
		if winningBoards[i] != winningBoards[i-1] {
			winningBoards[k] = winningBoards[i]
			k++
		}
	}

	return
}

func (bt *BoardTracker) Elimenate(boardIndex int) {
	bt.index.DropBoard(boardIndex)

	tracker := bt.trackers[boardIndex]

	for i, _ := range tracker.rowMatches {
		tracker.rowMatches[i] = 0
	}

	for i, _ := range tracker.colMatches {
		tracker.colMatches[i] = 0
	}
}

func readCalled(scanner *bufio.Scanner) ([]int, error) {
	if !scanner.Scan() {
		return nil, fmt.Errorf("No text to get called bingo numbers from")
	}

	numsStr := strings.Split(scanner.Text(), ",")

	calledNums := make([]int, 0, len(numsStr))
	for _, num := range numsStr {
		i, err := strconv.ParseInt(num, 10, 32)
		if err != nil {
			return nil, fmt.Errorf("Failed to parse called number: %v", err)
		}

		calledNums = append(calledNums, int(i))
	}

	return calledNums, nil
}

func readRow(scanner *bufio.Scanner) (Row, error) {
	if !scanner.Scan() {
		return Row{}, fmt.Errorf("No row found: %v", scanner.Err())
	}

	// Skip empty lines
	if len(scanner.Text()) == 0 {
		for scanner.Scan() {
			if len(scanner.Text()) > 0 {
				break
			}
		}
	}

	rowStr := strings.Split(scanner.Text(), " ")

	row := Row{nums: make([]int, 0, len(rowStr))}

	for _, num := range rowStr {
		// Spaces that align the board nicely cause extra empty strings in our
		// row - skip them
		if len(num) == 0 {
			continue
		}

		i, err := strconv.ParseInt(num, 10, 32)
		if err != nil {
			return Row{}, fmt.Errorf("Failed to parse board number: %v", err)
		}

		row.nums = append(row.nums, int(i))
	}

	return row, nil
}

func readBoard(scanner *bufio.Scanner) (Board, error) {
	rows := make([]Row, 0, 10)

	firstRow, err := readRow(scanner)
	if err != nil {
		return Board{}, err
	}

	rows = append(rows, firstRow)

	for rowIndex := 1; rowIndex < len(firstRow.nums); rowIndex++ {
		row, err := readRow(scanner)
		if err != nil {
			return Board{}, err
		}

		rows = append(rows, row)
	}

	return Board{eliminated: false, rows: rows}, nil
}

func allCalled(allCalls []int, winningCall int) []int {
	called := make([]int, 0, len(allCalls))

	for _, call := range allCalls {
		called = append(called, call)
		if call == winningCall {
			return called
		}
	}

	return allCalls
}

func calcBoardScore(called []int, board Board) int {
	// Flatten board and sort,
	boardNums := make([]int, 0, ROW_WIDTH*COL_HEIGHT)
	for _, row := range board.rows {
		for _, num := range row.nums {
			boardNums = append(boardNums, num)
		}
	}
	sort.Ints(boardNums)

	// Sort called numbers
	sort.Ints(called)

	// Any called number gets zeros (then they don't take part in the additon)
	calledIndex := 0
	for i := range boardNums {
		// Find the next possible point
		for ; calledIndex < len(called) && boardNums[i] > called[calledIndex]; calledIndex++ {
		}

		if calledIndex == len(called) {
			break
		} else if boardNums[i] == called[calledIndex] {
			boardNums[i] = 0
		}
	}

	// Sum remaining
	total := 0
	for _, n := range boardNums {
		total += n
	}

	return total
}

func readInput(input []byte) ([]int, []Board, error) {
	scanner := bufio.NewScanner(bytes.NewReader(input))

	calls, err := readCalled(scanner)
	if err != nil {
		return nil, nil, err
	}

	boards := make([]Board, 0, 10)

	for true {
		board, err := readBoard(scanner)
		if err != nil {
			break
		}

		boards = append(boards, board)
	}

	return calls, boards, nil
}

func part1(input []byte) error {
	calls, boards, err := readInput(input)
	if err != nil {
		return err
	}

	tracker := NewBoardTracker(boards)

	called := make([]int, 0, len(calls))

	for _, call := range calls {
		called = append(called, call)

		maybeWinners := tracker.Mark(call)

		if len(maybeWinners) > 0 {
			for _, winner := range maybeWinners {
				score := calcBoardScore(called, boards[winner])
				fmt.Printf("Board %v won when %v was called with a score of %v (puzzle answer: %v)\n", winner, call, score, score*call)
			}
			break
		}
	}

	return nil
}

func part2(input []byte) error {
	calls, boards, err := readInput(input)
	if err != nil {
		return err
	}

	tracker := NewBoardTracker(boards)

	called := make([]int, 0, len(calls))

	remaining := len(boards)

	for _, call := range calls {
		called = append(called, call)

		maybeWinners := tracker.Mark(call)

		for _, winner := range maybeWinners {
			tracker.Elimenate(winner)
			boards[winner].eliminated = true
			remaining--

			if remaining == 0 {
				score := calcBoardScore(called, boards[winner])
				fmt.Printf("Board %v remains when %v was called with a score of %v (puzzle answer: %v)\n", winner, call, score, score*call)
				return nil
			}
		}
	}
	return fmt.Errorf("oopsie")
}

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "4")
	if err != nil {
		fmt.Println("Failed to load input,", err)
	}

	fmt.Println("######## Part1 ########")
	fmt.Println("      ExampleData")

	if err := part1([]byte(exampleData)); err != nil {
		fmt.Println("Failed :( ...", err)
	}

	fmt.Println("        RealData")
	if err := part1(raw_input); err != nil {
		fmt.Println("Failed...", err)
	}

	fmt.Println("######## Part2 ########")
	fmt.Println("      ExampleData")

	if err := part2([]byte(exampleData)); err != nil {
		fmt.Println("Failed :( ...", err)
	}

	fmt.Println("        RealData")
	if err := part2(raw_input); err != nil {
		fmt.Println("Failed...", err)
	}
}
