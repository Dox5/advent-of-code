package main

import (
	"log"
	"strconv"
	"math"

	"2019/util"
)

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
			v := float64(num) / 3
			v = math.Floor(v)
			total += int(v)-2
		}
	}
	log.Println(total)
}
