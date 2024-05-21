// Advent Of Code
// January 12th, 2023
// https://adventofcode.com/2022/day/1

package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
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

// PART ONE
// Parse File and identify calories and blank lines
// Loop and add inputs until a blank line is encountered
// Checks if sum i greater than max value if any
// return max value
func partOne(scanner *bufio.Scanner) {
	var max, sum int

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			if sum > max {
				max = sum
			}
			sum = 0
		} else {
			num, err := strconv.Atoi(line)
			if err != nil {
				panic(err)
			}

			sum += num
		}
	}

	fmt.Println("Highest calories:", max)
}

// PART TWO
// 3 element array to store top 3 calories in ascending order 3,2,1
// repeat steps 1 and 2 from part 1
func partTwo(scanner *bufio.Scanner) {
	top3 := [3]int{}
	sum := 0

	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			// fmt.Println("sum", sum)
			// fmt.Print("top3", top3, "\n\n")

			if sum > top3[0] {
				// > 1st el
				if sum > top3[1] {
					// > 2nd el
					if sum > top3[2] {
						// > 3rd el
						top3[0], top3[1], top3[2] = top3[1], top3[2], sum
					} else {
						top3[0], top3[1] = top3[1], sum
					}
				} else {
					top3[0] = sum
				}
			}

			sum = 0
		} else {
			num, err := strconv.Atoi(line)
			if err != nil {
				panic(err)
			}

			sum += num
		}
	}

	fmt.Println("Top3 (3,2,1):", top3[0], top3[1], top3[2])
	fmt.Println("Sum of top3:", top3[0]+top3[1]+top3[2])
}
