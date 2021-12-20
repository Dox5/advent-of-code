package main

import (
	"bytes"
	"fmt"
	"github.com/Dox5/advent-of-code/james/2021/aoc_fetch"
	"github.com/Dox5/advent-of-code/james/2021/internal/pkg/bits"
	"strings"
)

var exampleData = []string{
	"8A004A801A8002F478",
	"620080001611562C8802118E34",
	"C0015000016115A2E0802F182340",
	"A0016C880162017C3686B18A3D4780",
	"C200B40A82",
	"04005AC33890",
	"880086C3E88112",
	"CE00C43D881120",
	"D8005AC2A8F0",
	"F600BC2D8F",
	"9C005AC2F8F0",
	"9C0141080250320F1802104A08",
}

func part1(transmission bits.Packet) {
	versionSum := 0
	toExplore := make([]*bits.Packet, 1, 128)
	toExplore[0] = &transmission
	for {
		if len(toExplore) == 0 {
			break
		}

		p := toExplore[len(toExplore)-1]
		toExplore = toExplore[:len(toExplore)-1]

		versionSum += int(p.Version)
		if children, ok := p.GetChildren(); ok {
			for i, _ := range children {
				toExplore = append(toExplore, &children[i])
			}
		}
	}

	fmt.Println("Sum of versions:", versionSum)
}

func part2(transmission bits.Packet) {
	value, err := bits.Evaluate(transmission)
	if err != nil {
		fmt.Println("Error evaulating packet", err)
		return
	}

	fmt.Println("Packet evaluates to:", value)
}

func main() {
	for i, transmission := range exampleData {
		fmt.Printf("##### ExampleData %v #####\n", i)
		pkt, err := bits.Decode(strings.NewReader(transmission))
		if err != nil {
			panic(err)
		}

		part1(pkt)
		part2(pkt)
		fmt.Println()
	}

	raw_input, err := aoc_fetch.PuzzleInputFor("2021", "16")
	if err != nil {
		fmt.Println("Failed to load input,", err)
		return
	}

	pkt, err := bits.Decode(bytes.NewReader(raw_input))
	if err != nil {
		panic(err)
	}

	fmt.Println("##### RealData #####")
	part1(pkt)
	part2(pkt)
	fmt.Println()
}
