package bits

import (
	"bytes"
	"fmt"
	"testing"
)

func makeReader(bs ...byte) *bitReader {
	return newBitReader(bytes.NewReader(bs))
}

func TestSingleAlignedRead(t *testing.T) {
	cases := []struct {
		size     uint8
		input    []byte
		expected uint64
	}{
		{size: 8, input: []byte{0xAD}, expected: 0xAD},
		{size: 16, input: []byte{0xAD, 0x0F}, expected: 0xAD0F},
		{size: 24, input: []byte{0x52, 0x12, 0xA2}, expected: 0x5212A2},
		{size: 3, input: []byte{0b1010_1100}, expected: 0b101},
		{size: 9, input: []byte{0b1010_1100, 0b1000_0000}, expected: 0b1010_1100_1},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%v bit read", c.size), func(t *testing.T) {
			reader := newBitReader(bytes.NewReader(c.input))

			actual, err := reader.ReadBitsAsInt(c.size)
			if err != nil {
				t.Log("Expected no error but got", err)
			}

			if c.expected != actual {
				t.Log("Expected", c.expected, "but read", actual)
				t.Fail()
			}
		})
	}
}

func TestSingleUnalignedRead(t *testing.T) {
	cases := []struct {
		offset   []uint8
		size     uint8
		input    []byte
		expected uint64
	}{
		{offset: []uint8{1}, size: 7, input: []byte{0b1010_0011}, expected: 0b010_0011},
		{offset: []uint8{1}, size: 6, input: []byte{0b1010_0011}, expected: 0b010_001},
		{offset: []uint8{5}, size: 6, input: []byte{0b1010_0011, 0b0101_1100}, expected: 0b011_010},
		{offset: []uint8{3, 15},
			size:     3,
			input:    []byte{0b110_11100, 0b11111111, 0b10_001_000},
			expected: 1},
	}

	for _, c := range cases {
		t.Run(fmt.Sprintf("%v bit read (offset %v)", c.size, c.offset), func(t *testing.T) {
			reader := newBitReader(bytes.NewReader(c.input))

			for _, o := range c.offset {
				_, err := reader.ReadBitsAsInt(o)
				if err != nil {
					t.Log("Offsetting read failed:", err)
					t.Fail()
				}
			}

			actual, err := reader.ReadBitsAsInt(c.size)
			if err != nil {
				t.Log("Expected no error but got", err)
				t.Fail()
			}

			if c.expected != actual {
				t.Log("Expected", c.expected, "but read", actual)
				t.Fail()
			}
		})
	}
}
