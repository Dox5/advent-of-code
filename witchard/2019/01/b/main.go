package main

import (
	"log"
	"math"
	"strconv"

	"2019/util"
)

func ComputeFuel(mass int) int {
	v := float64(mass) / 3
	v = math.Floor(v)
	return int(v) - 2
}

func ComputeAllFuel(mass int) int {
	total := 0
	fuel := ComputeFuel(mass)
	for fuel >= 0 {
		total += fuel
		fuel = ComputeFuel(fuel)
	}
	return total
}

func main() {
	input, err := util.Get(1)
	if err != nil {
		log.Fatalln("Couldn't get input:", err)
	}

	total := 0
	for _, in := range input {
		if in != "" {
			num, err := strconv.Atoi(in)
			if err != nil {
				log.Fatalln("Couldn't parse input:", in, "-", err)
			}
			total += ComputeAllFuel(num)
		}
	}
	log.Println(total)
}
