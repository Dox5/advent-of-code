package bits

import (
	"bufio"
	"fmt"
	"io"
)

const (
	BITS_IN_BYTE uint8 = 8
)

type bitReader struct {
	source    *bufio.Reader
	bitOffset uint8
	consumed  uint
}

func newBitReader(byteString io.Reader) *bitReader {
	return &bitReader{
		source:    bufio.NewReader(byteString),
		bitOffset: 0,
		consumed:  0,
	}
}

func makeByteMask(bits, offsetFromLeft uint8) uint8 {
	if bits == BITS_IN_BYTE {
		// Taking the whole thing - offset is meaningless
		return 0xFF
	} else {
		var mask uint8 = 1
		mask = (mask << bits) - 1
		gap := BITS_IN_BYTE - bits
		return mask << (gap - offsetFromLeft)
	}
}

// count [1, 64]
func (r *bitReader) ReadBitsAsInt(count uint8) (uint64, error) {
	if count > 64 || count == 0 {
		panic(fmt.Sprintf("Precondition failed: count [1, 64] - actually %v", count))
	}

	// Grab enough bytes to extract our whole 'count' bits
	wholeBytesNeeded := (count + r.bitOffset) / BITS_IN_BYTE
	block := make([]byte, wholeBytesNeeded, wholeBytesNeeded+1)

	if wholeBytesNeeded > 0 {
		read, err := io.ReadFull(r.source, block)

		if err != nil {
			return 0, fmt.Errorf("Failed to read %v bits: %v", count, err)
		}

		if uint8(read) != wholeBytesNeeded {
			return 0, fmt.Errorf("Failed to read all %v bytes only got %v", wholeBytesNeeded, read)
		}
	}

	// Need extra byte in case of partial reads (bitOffset or (count % 8 != 0))
	if (r.bitOffset+count)%8 != 0 {
		extraByte, err := r.source.Peek(1)
		if err != nil {
			return 0, fmt.Errorf("Failed to peek extra byte needed: %v", err)
		}

		block = append(block, extraByte[0])
	}

	// Construct the output word
	var out uint64 = 0
	remaining := count

	for _, b := range block {
		blockAvailable := BITS_IN_BYTE - r.bitOffset

		if remaining >= blockAvailable {
			// Will consume the rest of this block
			remaining -= blockAvailable

			// remaining tells us how many more bits we need to fit to the right
			// of these - so that is the shift amount

			mask := makeByteMask(blockAvailable, r.bitOffset)
			bits := uint64(uint8(b)&mask) << remaining
			out |= bits

			// Consumed a whole block so bitOffset returns to 0
			r.bitOffset = 0

		} else {
			// Only need part of this byte (and no further ones!)
			// Need to mask and shift right

			mask := makeByteMask(remaining, r.bitOffset)
			bits := uint64(uint8(b) & mask)
			gap := (BITS_IN_BYTE - remaining) - r.bitOffset
			bits >>= gap
			out |= bits

			r.bitOffset = (r.bitOffset + remaining) % 8

			break
		}
	}

	//fmt.Printf(fmt.Sprintf("%%0%vb ", count), out)

	r.consumed += uint(count)
	return out, nil
}

func (r *bitReader) Skip(count uint8) error {
	return fmt.Errorf("Not implemented")
}

func (r bitReader) Consumed() uint {
	return r.consumed
}
