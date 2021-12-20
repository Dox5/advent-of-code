package bits

import (
	"encoding/hex"
	"io"
)

type TagType uint8

const (
	SumTag          TagType = 0
	ProductTag              = 1
	MinimumTag              = 2
	MaximumTag              = 3
	LiteralValueTag         = 4
	GreaterThanTag          = 5
	LessThanTag             = 6
	EqualToTag              = 7
)

type LengthEncodingType uint8

const (
	BitCountEncoding LengthEncodingType = iota
	PacketCountEncoding
)

type Packet struct {
	Version uint8
	Tag     TagType

	Data interface{}
}

func (p Packet) GetChildren() ([]Packet, bool) {
	children, ok := p.Data.([]Packet)
	return children, ok
}

func decodeHeader(pkt *Packet, reader *bitReader) error {
	version, err := reader.ReadBitsAsInt(3)
	if err != nil {
		return err
	}

	tag, err := reader.ReadBitsAsInt(3)
	if err != nil {
		return err
	}

	pkt.Version = uint8(version)
	pkt.Tag = TagType(tag)

	return nil
}

func decodeLiteral(pkt *Packet, reader *bitReader) error {
	var literal uint64 = 0

	// If bit4 is set then there is another 4 bits of literal
	moreBit := uint64(0b10000)

	// If this hits zero then the 64 bit uint isn't big enough
	shiftsRemaining := 64 / 4

	for {
		if shiftsRemaining == 0 {
			panic("Ran out of space in 64bit int for literal!")
		}

		word, err := reader.ReadBitsAsInt(5)
		if err != nil {
			return err
		}

		// Make space
		literal <<= 4

		shiftsRemaining--

		// Take the bottom 4 bits only
		literal |= word & 0b01111

		// End of literal
		if moreBit&word == 0 {
			break
		}
	}

	pkt.Data = literal

	return nil
}

func decodeBitLengthOperator(pkt *Packet, reader *bitReader) error {
	length, err := reader.ReadBitsAsInt(15)
	if err != nil {
		return err
	}

	children := make([]Packet, 0, 8)

	sentinal := reader.Consumed() + uint(length)
	// Parse sub packets
	for reader.Consumed() < sentinal {
		var child Packet
		if err := decodePacket(&child, reader); err != nil {
			return err
		}

		children = append(children, child)
	}

	pkt.Data = children
	return nil
}

func decodePacketLengthOperator(pkt *Packet, reader *bitReader) error {
	count, err := reader.ReadBitsAsInt(11)
	if err != nil {
		return err
	}

	children := make([]Packet, count)

	// Parse sub packets
	for i := uint64(0); i < count; i++ {
		if err := decodePacket(&children[i], reader); err != nil {
			return err
		}
	}

	pkt.Data = children
	return nil
}

func decodeOperator(pkt *Packet, reader *bitReader) error {
	encodingType, err := reader.ReadBitsAsInt(1)
	if err != nil {
		return err
	}

	if encodingType == 0 {
		return decodeBitLengthOperator(pkt, reader)
	} else {
		return decodePacketLengthOperator(pkt, reader)
	}
}

func decodePacket(pkt *Packet, reader *bitReader) error {
	// First decode the header
	if err := decodeHeader(pkt, reader); err != nil {
		return err
	}

	// Decode body based on tag
	if pkt.Tag == LiteralValueTag {
		return decodeLiteral(pkt, reader)
	} else {
		// Operator
		return decodeOperator(pkt, reader)
	}
}

// Takes a hex string and decodes the packets
func Decode(hexString io.Reader) (Packet, error) {
	reader := newBitReader(hex.NewDecoder(hexString))

	var pkt Packet
	err := decodePacket(&pkt, reader)
	return pkt, err
}
