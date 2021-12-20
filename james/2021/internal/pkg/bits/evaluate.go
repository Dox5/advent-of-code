package bits

import (
	"fmt"
	"math"
)

func evalFold(pkts []Packet, foldOp func(int, int) int) (int, error) {
	accum, err := Evaluate(pkts[0])
	if err != nil {
		return 0, err
	}

	for i := 1; i < len(pkts); i++ {
		next, err := Evaluate(pkts[i])
		if err != nil {
			return 0, err
		}

		accum = foldOp(accum, next)
	}

	return accum, nil
}

func evalBinary(pkts []Packet, binaryOp func(int, int) int) (int, error) {
	if len(pkts) != 2 {
		return 0, fmt.Errorf("Expected exactly 2 inputs for binary op - got %v", len(pkts))
	}

	lhs, err := Evaluate(pkts[0])
	if err != nil {
		return 0, err
	}

	rhs, err := Evaluate(pkts[1])
	if err != nil {
		return 0, err
	}

	return binaryOp(lhs, rhs), nil
}

func Evaluate(pkt Packet) (int, error) {
	children, _ := pkt.GetChildren()

	switch pkt.Tag {
	case SumTag:
		return evalFold(children, func(accum int, next int) int {
			return accum + next
		})

	case ProductTag:
		return evalFold(children, func(accum int, next int) int {
			return accum * next
		})

	case MinimumTag:
		return evalFold(children, func(accum int, next int) int {
			if next < accum {
				return next
			} else {
				return accum
			}
		})

	case MaximumTag:
		return evalFold(children, func(accum int, next int) int {
			if next > accum {
				return next
			} else {
				return accum
			}
		})

	case LiteralValueTag:
		value, ok := pkt.Data.(uint64)

		if !ok {
			return 0, fmt.Errorf("Expected literal packet to contain a number!")
		}

		if value > math.MaxInt {
			return 0, fmt.Errorf("Value in packet too large for evauluation: %v", value)
		}

		return int(value), nil

	case GreaterThanTag:
		return evalBinary(children, func(l, r int) int {
			if l > r {
				return 1
			} else {
				return 0
			}
		})

	case LessThanTag:
		return evalBinary(children, func(l, r int) int {
			if l < r {
				return 1
			} else {
				return 0
			}
		})

	case EqualToTag:
		return evalBinary(children, func(l, r int) int {
			if l == r {
				return 1
			} else {
				return 0
			}
		})

	default:
		return 0, fmt.Errorf("Unknown tag %v", pkt.Tag)
	}
}
