// Advent of Code
// January 12th, 2023
// https://adventofcode.com/2022/day/3

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
		scanner := bufio.NewScanner(file)
		scanner.Split(bufio.ScanLines)
		partOne(scanner)
		file.Close()
	}

	if file, err := os.Open(fileName); err != nil {
		fmt.Println(err)
	} else {
		scanner := bufio.NewScanner(file)
		scanner.Split(bufio.ScanLines)
		partTwo(scanner)
		file.Close()
	}
}

func partOne(scanner *bufio.Scanner) {
	score := 0
	for scanner.Scan() {
		line := []rune(scanner.Text())
		mid := len(line) / 2
		comp1 := line[:mid]
		comp2 := line[mid:]
		common := runeInterSection(comp1, comp2)

		// Calculate Score
		if common < 'a' {
			score += (int(common-'A') + 27)
		} else {
			score += (int(common-'a') + 1)
		}

		// fmt.Print(line, comp1, comp2, common, score, "\n\n")
	}

	fmt.Println("Score of:", score)
}

func runeInterSection(slice1 []rune, slice2 []rune) rune {
	for _, v1 := range slice1 {
		for _, v2 := range slice2 {
			if v1 == v2 {
				return v1
			}
		}
	}

	return '0'
}

func partTwo(scanner *bufio.Scanner) {
	score := 0
	last3lines := [3][]rune{}
	index := 0
	for scanner.Scan() {
		line := []rune(scanner.Text())
		last3lines[index] = line
		index++

		if index >= 3 {
			index = 0
			common := tripleRuneInterSection(last3lines[0], last3lines[1], last3lines[2])

			// Calculate Score
			if common < 'a' {
				score += (int(common-'A') + 27)
			} else {
				score += (int(common-'a') + 1)
			}
		}
	}

	fmt.Println("Part 2 Score of:", score)
}

func tripleRuneInterSection(slice1 []rune, slice2 []rune, slice3 []rune) rune {
	// Couldn't think of a better way to do this :(
	// i.e find common element in 3 slices
	for _, v1 := range slice1 {
		for _, v2 := range slice2 {
			for _, v3 := range slice3 {
				if v1 == v2 {
					if v2 == v3 {
						return v1
					}
				}
			}
		}
	}

	return '0'
}
