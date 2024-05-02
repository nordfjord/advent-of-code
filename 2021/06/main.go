package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

func main() {
	start := time.Now()
	initial := [9]int{}
	reader := bufio.NewReader(os.Stdin)
	for {
		r, err := reader.ReadByte()
		if err != nil {
			break
		}
		if '0' <= r && r <= '9' {
			initial[int(r)-'0'] += 1
		}
	}

	p1 := 0

	for i := range 256 {
		zeroes := initial[0]
		initial[0] = initial[1]
		initial[1] = initial[2]
		initial[2] = initial[3]
		initial[3] = initial[4]
		initial[4] = initial[5]
		initial[5] = initial[6]
		initial[6] = initial[7] + zeroes
		initial[7] = initial[8]
		initial[8] = zeroes
		if i == 79 {
			p1 = sum(initial)
		}
	}
	p2 := sum(initial)

	fmt.Printf("Part 1: %d\n", p1)
	fmt.Printf("Part 2: %d\n", p2)
	fmt.Printf("Execution time: %s\n", time.Since(start))
}

func sum(a [9]int) int {
	s := 0
	for _, x := range a {
		s += x
	}
	return s
}
