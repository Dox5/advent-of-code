package main

import (
	"bufio"
	"fmt"
	"strings"
	"testing"
)

func TestParsing(t *testing.T) {
	cases := []string{
		"6",
		"[1,2]",
		"[1,[2,[3,4]]]",
		"[1,[[2,3],4]]",
		"[13,0]",
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("ParseSnailfish_%q", c), func(t *testing.T) {
			reader := bufio.NewReader(strings.NewReader(c))
			actual := SnailfishNumber{}
			if err := Parse(reader, &actual); err != nil {
				t.Log("Expected to parse but got error", err)
				t.Fail()
			}

			backToString := actual.String()
			if backToString != c {
				t.Logf("Expected %v but got %v", c, backToString)
				t.Fail()
			}
		})
	}
}

func TestExploding(t *testing.T) {
	cases := []struct {
		input  string
		result string
	}{
		{input: "[[[[[9,8],1],2],3],4]", result: "[[[[0,9],2],3],4]"},
		{input: "[7,[6,[5,[4,[3,2]]]]]", result: "[7,[6,[5,[7,0]]]]"},
		{input: "[[6,[5,[4,[3,2]]]],1]", result: "[[6,[5,[7,0]]],3]"},
		{input: "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", result: "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"},
		{input: "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", result: "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("ExplodeSnailfish_%q", c.input), func(t *testing.T) {
			reader := bufio.NewReader(strings.NewReader(c.input))
			num := SnailfishNumber{}
			if err := Parse(reader, &num); err != nil {
				t.Log("Expected to parse but got error", err)
				t.Fail()
			}

			explode(&num)

			backToString := num.String()
			if backToString != c.result {
				t.Logf("Expected %v but got %v", c.result, backToString)
				t.Fail()
			}
		})
	}
}

func TestSplitting(t *testing.T) {
	cases := []struct {
		input  string
		result string
	}{
		{input: "[10,1]", result: "[[5,5],1]"},
		{input: "[11,1]", result: "[[5,6],1]"},
		{input: "[0,13]", result: "[0,[6,7]]"},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("SplitSnailfish_%q", c.input), func(t *testing.T) {
			reader := bufio.NewReader(strings.NewReader(c.input))
			num := SnailfishNumber{}
			if err := Parse(reader, &num); err != nil {
				t.Log("Expected to parse but got error", err)
				t.Fail()
			}

			split(&num)

			backToString := num.String()
			if backToString != c.result {
				t.Logf("Expected %v but got %v", c.result, backToString)
				t.Fail()
			}
		})
	}
}

func TestReduce(t *testing.T) {
	cases := []struct {
		input  string
		result string
	}{
		{input: "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]", result: "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("ReduceSnailfish_%q", c.input), func(t *testing.T) {
			reader := bufio.NewReader(strings.NewReader(c.input))
			num := SnailfishNumber{}
			if err := Parse(reader, &num); err != nil {
				t.Log("Expected to parse but got error", err)
				t.Fail()
			}

			Reduce(&num)

			backToString := num.String()
			if backToString != c.result {
				t.Logf("Expected %v but got %v", c.result, backToString)
				t.Fail()
			}
		})
	}
}

func TestSum(t *testing.T) {
	cases := []struct {
		input  string
		result string
	}{
		{input: "[1,1]\n[2,2]\n[3,3]\n[4,4]", result: "[[[[1,1],[2,2]],[3,3]],[4,4]]"},
		{input: "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]", result: "[[[[3,0],[5,3]],[4,4]],[5,5]]"},
		{input: "[1,1]\n[2,2]\n[3,3]\n[4,4]\n[5,5]\n[6,6]", result: "[[[[5,0],[7,4]],[5,5]],[6,6]]"},
		{input: "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]\n", result: "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("SumSnailfish_%q", c.input), func(t *testing.T) {
			reader := strings.NewReader(c.input)
			nums := ParseList(reader)

			res := Sum(nums)

			backToString := res.String()
			if backToString != c.result {
				t.Logf("Expected %v but got %v", c.result, backToString)
				t.Fail()
			}
		})
	}
}
