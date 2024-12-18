// Advent of Code
// Day 5
// https://adventofcode.com/2022/day/5

package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	fileName := "input.txt"

	var crates = [9][]string{
		{"G", "T", "R", "W"},
		{"G", "C", "H", "P", "M", "S", "V", "W"},
		{"C", "L", "T", "S", "G", "M"},
		{"J", "H", "D", "M", "W", "R", "F"},
		{"P", "Q", "L", "H", "S", "W", "F", "J"},
		{"P", "J", "D", "N", "F", "M", "S"},
		{"Z", "B", "D", "F", "G", "C", "S", "J"},
		{"R", "T", "B"},
		{"H", "N", "W", "L", "C"},
	}

	if file, err := os.Open(fileName); err != nil {
		fmt.Println(err)
	} else {
		// Split file into lines
		scanner := bufio.NewScanner(file)
		scanner.Split(bufio.ScanLines)

		partOne(scanner, crates)

		file.Close()
	}

	if file, err := os.Open(fileName); err != nil {
		fmt.Println(err)
	} else {
		// Split file into lines
		scanner := bufio.NewScanner(file)
		scanner.Split(bufio.ScanLines)

		partTwo(scanner, crates)

		file.Close()
	}
}

func partOne(scanner *bufio.Scanner, crates [9][]string) {
	// Reads from file line by line
	for scanner.Scan() {
		line := scanner.Text()
		text := strings.Split(line, " ")

		if text[0] == "move" {
			nums, _ := strconv.Atoi(text[1])
			from, _ := strconv.Atoi(text[3])
			to, _ := strconv.Atoi(text[5])

			// Convert to array indices
			from -= 1
			to -= 1

			for i := 0; i < nums; i++ {
				// Select element
				end := len(crates[from]) - 1
				el := crates[from][end]

				// Remove from original slice
				crates[from] = crates[from][:end]

				// Add in "to" slice
				crates[to] = append(crates[to], el)
			}
		}
	}

	fmt.Println("Part One:")
	for _, v := range crates {
		fmt.Print(v[len(v)-1])
	}
	fmt.Println()
	// fmt.Println(crates)
}

func partTwo(scanner *bufio.Scanner, crates [9][]string) {
	// Reads from file line by line
	for scanner.Scan() {
		line := scanner.Text()
		text := strings.Split(line, " ")

		if text[0] == "move" {
			nums, _ := strconv.Atoi(text[1])
			from, _ := strconv.Atoi(text[3])
			to, _ := strconv.Atoi(text[5])

			// Convert to array indices
			from -= 1
			to -= 1

			// Select element
			end := len(crates[from]) - nums
			els := crates[from][end:]

			// fmt.Println(end, len(crates[from]), nums, els)

			// Remove from original slice
			crates[from] = crates[from][:end]

			// Add in "to" slice
			crates[to] = append(crates[to], els...)
		}
	}

	fmt.Println("Part two:")
	for _, v := range crates {
		fmt.Print(v[len(v)-1])
	}
	fmt.Println()

	// fmt.Println(crates)
}
