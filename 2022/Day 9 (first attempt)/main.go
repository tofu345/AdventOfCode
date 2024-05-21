package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Pos struct {
	x, y int
}

const rows, cols = 10000, 10000

func main() {
	fileName := "input.txt"
	file, err := os.Open(fileName)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	// Split file into lines
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)
	partOne(scanner)
}

func partOne(scanner *bufio.Scanner) {
	head := Pos{0, rows - 1}
	tail := Pos{0, rows - 1}
	tailMoveCount := 0
	// displayPosition(head, tail)

	for scanner.Scan() {
		line := scanner.Text()
		split := strings.Split(line, " ")
		direction := split[0]
		amount, err := strconv.Atoi(split[1])
		if err != nil {
			panic(err)
		}

		for i := 0; i < amount; i++ {
			switch direction {
			case "R":
				head.x++
			case "L":
				head.x--
			case "U":
				head.y--
			case "D":
				head.y++
			}

			// fmt.Println(head, tail, math.Abs(float64(head.y-tail.y)), math.Abs(float64(head.x-tail.x)), direction, amount)
			// displayPosition(head, tail)

			if math.Abs(float64(head.x-tail.x)) > 1 {
				switch direction {
				case "R":
					tail.x++
				case "L":
					tail.x--
				}
				if math.Abs(float64(head.y-tail.y)) == 1 {
					tail.y += head.y - tail.y
				}
				tailMoveCount++
			}

			if math.Abs(float64(head.y-tail.y)) > 1 {
				switch direction {
				case "D":
					tail.y++
				case "U":
					tail.y--
				}
				if math.Abs(float64(head.x-tail.x)) == 1 {
					tail.x += head.x - tail.x
				}
				tailMoveCount++
			}

			// displayPosition(head, tail)
		}
	}

	fmt.Println(tailMoveCount)
}

func displayPosition(head, tail Pos) {
	for i := 0; i < rows; i++ {
		for j := 0; j < cols; j++ {
			if head.y == i && head.x == j {
				fmt.Print("H")
			} else if tail.y == i && tail.x == j {
				fmt.Print("T")
			} else {
				fmt.Print(".")
			}
			fmt.Print(" ")
		}
		fmt.Println()
	}

	fmt.Println(strings.Repeat("--", cols))
}
