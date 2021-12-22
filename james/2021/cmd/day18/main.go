package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"io"
	"strings"
)

type SnailfishNumber struct {
	lhs, rhs *SnailfishNumber // Either SnailfishNumber or int
	leaf     int              // If this is a leaf (both lhs & rhs nil) this contains the value
}

func DigitToInt(c byte) (int, bool) {
	switch c {
	case '0':
		return 0, true
	case '1':
		return 1, true
	case '2':
		return 2, true
	case '3':
		return 3, true
	case '4':
		return 4, true
	case '5':
		return 5, true
	case '6':
		return 6, true
	case '7':
		return 7, true
	case '8':
		return 8, true
	case '9':
		return 9, true

	default:
		return 0, false
	}
}

func (n SnailfishNumber) IsLeaf() bool {
	return n.lhs == nil && n.rhs == nil
}

func (n SnailfishNumber) String() string {
	if !n.IsLeaf() {
		return fmt.Sprintf("[%v,%v]", n.lhs, n.rhs)
	} else {
		return fmt.Sprintf("%v", n.leaf)
	}
}

func (n *SnailfishNumber) Clone() *SnailfishNumber {
	if !n.IsLeaf() {
		return &SnailfishNumber{
			lhs: n.lhs.Clone(),
			rhs: n.rhs.Clone(),
		}
	} else {
		return &SnailfishNumber{leaf: n.leaf}
	}
}

func (n *SnailfishNumber) Magnitude() int {
	if !n.IsLeaf() {
		return n.lhs.Magnitude()*3 + n.rhs.Magnitude()*2
	} else {
		return n.leaf
	}
}

func ParseInt(reader *bufio.Reader) (int, error) {
	val := 0

	multipler := 1

	for {
		digit, err := reader.ReadByte()
		if err != nil {
			// Must've hit the end of the number
			break
		}

		d, isDigit := DigitToInt(digit)

		if !isDigit {
			err = reader.UnreadByte()
			if err != nil {
				// Eek - unreading failed!
				return 0, err
			}
			break
		}

		val *= 10
		val += d
		multipler *= 10
	}

	return val, nil
}

func ParseLiteral(reader *bufio.Reader, c byte) error {
	if actual, err := reader.ReadByte(); err != nil {
		return err
	} else if actual != c {
		return fmt.Errorf("Expected '%q' but found %q", c, actual)
	}
	return nil
}

func Parse(reader *bufio.Reader, root *SnailfishNumber) error {
	firstChar, err := reader.Peek(1)
	if err != nil {
		return err
	}

	// Assume integer
	if firstChar[0] != '[' {
		leaf, err := ParseInt(reader)
		if err == nil {
			root.leaf = leaf
		}

		return err
	}

	if err := ParseLiteral(reader, '['); err != nil {
		return err
	}

	// Parse left hand subtree
	root.lhs = &SnailfishNumber{}
	if err := Parse(reader, root.lhs); err != nil {
		return err
	}

	if err := ParseLiteral(reader, ','); err != nil {
		return err
	}

	// Parse right hand subtree
	root.rhs = &SnailfishNumber{}
	if err := Parse(reader, root.rhs); err != nil {
		return err
	}

	if err := ParseLiteral(reader, ']'); err != nil {
		return err
	}

	return nil
}

func ParseList(reader io.Reader) []SnailfishNumber {
	nums := make([]SnailfishNumber, 0, 128)
	scanner := bufio.NewScanner(reader)
	for scanner.Scan() {
		nums = append(nums, SnailfishNumber{})
		if err := Parse(bufio.NewReader(strings.NewReader(scanner.Text())), &nums[len(nums)-1]); err != nil {
			panic(err)
		}
	}

	return nums
}

// Given a root take as many right-hand nodes as possible till a leaf is found
func decendRight(root *SnailfishNumber) *SnailfishNumber {
	for root.rhs != nil {
		root = root.rhs
	}
	return root
}

func decendLeft(root *SnailfishNumber) *SnailfishNumber {
	for root.lhs != nil {
		root = root.lhs
	}
	return root
}

// Takes the current stack of Snailfish numebrs - return true to end walk
// walk will return the result of the vistior function
type Vistior func([]*SnailfishNumber) bool

func (root *SnailfishNumber) walk(visitor Vistior) bool {
	// Visited nodes
	stack := make([]*SnailfishNumber, 1, 128)
	stack[0] = root

	// Start decending the tree
	decending := true

	for {
		node := stack[len(stack)-1]

		// If the visitor finds what they want we're done
		if visitor(stack) {
			return true
		} else if decending {
			// Decending - always put the left node onto the stack
			if node.lhs != nil {
				stack = append(stack, node.lhs)
			} else {
				// Hit a leaf - go back up and try the right handside now
				decending = false
			}
			continue // Setup for next round
		} else {
			// Ascending
			if len(stack) > 1 {
				parent := stack[len(stack)-2]
				stack = stack[:len(stack)-1] // Always done with the current node

				// If we haven't already explored the RHS of this node then do so
				if parent.rhs != node {
					// Now decend back down this RHS
					stack = append(stack, parent.rhs)
					decending = true
				}
			} else {
				// Stack is empty so we must've explored all possible routes
				break
			}
		}
	}

	return false
}

func explode(root *SnailfishNumber) bool {

	return root.walk(func(stack []*SnailfishNumber) bool {
		node := stack[len(stack)-1]
		depth := len(stack) - 1

		if depth != 4 || node.lhs == nil || node.rhs == nil {
			return false
		}

		// Found something to explode - it's on the bottom of stack
		// First find something to the left, walk back up looking for an untaken left
		// hand branch (then decend the right hand side till a leaf is found
		var leftNeighbour *SnailfishNumber = nil
		for i := len(stack) - 2; i >= 0; i-- {
			if stack[i].lhs == stack[i+1] {
				// This is part of the path we already took
				continue
			} else {
				// Found a lhs - take it and decend to the right
				leftNeighbour = decendRight(stack[i].lhs)
				break
			}
		}

		var rightNeighbour *SnailfishNumber = nil
		for i := len(stack) - 2; i >= 0; i-- {
			if stack[i].rhs == stack[i+1] {
				// This is part of the path we already took
				continue
			} else {
				// Found a lhs - take it and decend to the right
				rightNeighbour = decendLeft(stack[i].rhs)
				break
			}
		}

		// if either neighbour is nil - we got all the way back to root without finding
		// a neighbour

		toExplode := stack[len(stack)-1]
		if leftNeighbour != nil {
			leftNeighbour.leaf += toExplode.lhs.leaf
		}

		if rightNeighbour != nil {
			rightNeighbour.leaf += toExplode.rhs.leaf
		}

		toExplode.lhs = nil
		toExplode.rhs = nil
		toExplode.leaf = 0

		return true
	})

}

func split(root *SnailfishNumber) bool {
	return root.walk(func(stack []*SnailfishNumber) bool {
		node := stack[len(stack)-1]

		if node.lhs != nil || node.rhs != nil {
			// Not a leaf
			return false
		}

		if node.leaf < 10 {
			return false
		}

		// Need to split this one
		newNode := &SnailfishNumber{
			lhs: &SnailfishNumber{leaf: int(node.leaf / 2)},
			rhs: &SnailfishNumber{leaf: int((node.leaf + 1) / 2)},
		}

		// Must be a parent if we are a leaf
		parent := stack[len(stack)-2]
		if parent.lhs == node {
			parent.lhs = newNode
		} else if parent.rhs == node {
			parent.rhs = newNode
		} else {
			panic("Parent is not our parent!")
		}

		return true
	})
}

func Reduce(n *SnailfishNumber) {
	for {
		if explode(n) {
			continue
		} else if split(n) {
			continue
		}

		// No other actions to take
		break
	}
}

func Add(a, b *SnailfishNumber) *SnailfishNumber {
	res := &SnailfishNumber{
		lhs: a.Clone(),
		rhs: b.Clone(),
	}

	Reduce(res)
	return res
}

func Sum(vals []SnailfishNumber) *SnailfishNumber {
	total := &SnailfishNumber{
		lhs:  vals[0].lhs,
		rhs:  vals[0].rhs,
		leaf: vals[0].leaf,
	}

	for i := 1; i < len(vals); i++ {
		newRoot := &SnailfishNumber{
			lhs: total,
			rhs: &SnailfishNumber{
				lhs:  vals[i].lhs,
				rhs:  vals[i].rhs,
				leaf: vals[i].leaf,
			},
		}
		Reduce(newRoot)
		total = newRoot
	}

	return total
}

func part1(reader io.Reader) {
	nums := ParseList(reader)
	sum := Sum(nums)
	fmt.Println("Sum:", sum)
	fmt.Println("Magnatude", sum.Magnitude())
}

func part2(reader io.Reader) {
	bestMag := 0
	nums := ParseList(reader)

	for i := range nums {
		for j := range nums {
			if j == i {
				continue
			}

			mag := Add(&nums[i], &nums[j]).Magnitude()
			if mag > bestMag {
				bestMag = mag
			}
		}
	}

	fmt.Println("Best magnatude is", bestMag)
}

var exampleData = `[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]
`

func main() {
	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "18")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	part1(strings.NewReader(exampleData))
	part1(bytes.NewReader(raw_input))
	part2(bytes.NewReader(raw_input))
}
