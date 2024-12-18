// Advent of Code
// Day 6
// https://adventofcode.com/2022/day/6

package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	fileName := "input.txt"

	if file, err := os.Open(fileName); err != nil {
		fmt.Println(err)
	} else {
		// Split file into lines
		scanner := bufio.NewScanner(file)
		scanner.Split(bufio.ScanLines)

		index := partOne(scanner)
		fmt.Println("Part one:", index)

		file.Close()
	}

	if file, err := os.Open(fileName); err != nil {
		fmt.Println(err)
	} else {
		// Split file into lines
		scanner := bufio.NewScanner(file)
		scanner.Split(bufio.ScanLines)

		index := partTwo(scanner)
		fmt.Println("Part two:", index)

		file.Close()
	}
}

func partOne(scanner *bufio.Scanner) int {
	// Reads from file line by line
	for scanner.Scan() {
		line := scanner.Text()

		for i := 0; i < len(line); i++ {
			// Marker starts at 4th character
			if i >= 3 {
				// Add current el and last 3 chars to map
				chars := make(map[byte]byte)
				for j := (i - 3); j <= i; j++ {
					chars[line[j]] = 0
				}

				// fmt.Println(chars, v, i, text[(i-3):i+1])

				// Check if there are 4 characters in the map
				// Meaning no characters are the same
				if len(chars) == 4 {
					return i + 1
				}
			}
		}
	}

	return -1
}

func partTwo(scanner *bufio.Scanner) int {
	// Reads from file line by line
	for scanner.Scan() {
		line := scanner.Text()

		for i := 0; i < len(line); i++ {
			// Marker starts at 4th character
			if i >= 13 {
				// Add current el and last 13 chars to map
				chars := make(map[byte]byte)
				for j := (i - 13); j <= i; j++ {
					chars[line[j]] = 0
				}

				// fmt.Println(chars, v, i, text[(i-3):i+1])

				// Check if there are 14 characters in the map
				// Meaning no characters are the same
				if len(chars) == 14 {
					return i + 1
				}
			}
		}
	}

	return -1
}
