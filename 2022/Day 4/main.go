// Advent Of Code
// 13th January 2023
// https://adventofcode.com/2022/day/4

package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	fileName := "input.txt"
	// fileName := "input copy.txt"

	if file, err := os.Open(fileName); err != nil {
		fmt.Println(err)
	} else {
		// Split file into lines
		scanner := bufio.NewScanner(file)
		scanner.Split(bufio.ScanLines)
		partOne(scanner)
		file.Close()
	}

	if file, err := os.Open(fileName); err != nil {
		fmt.Println(err)
	} else {
		// Split file into lines
		scanner := bufio.NewScanner(file)
		scanner.Split(bufio.ScanLines)
		partTwo(scanner)
		file.Close()
	}
}

func partOne(scanner *bufio.Scanner) {
	count := 0 // Number of assignment overlaps

	// Reads from file line by line
	for scanner.Scan() {
		line := scanner.Text()

		// Seperate assignments
		ids := strings.Split(line, ",")
		elf1 := strToIntSlice(ids[0])
		elf2 := strToIntSlice(ids[1])

		// first el of the elf1 is less than the first of elf2
		// last el of elf1 is greater than the last of elf2
		// Eg: 2-8,3-7; 2 <= 3, 8 >= 7
		// 6-6,4-6 as well
		if elf1[0] <= elf2[0] && elf1[1] >= elf2[1] ||
			elf2[0] <= elf1[0] && elf2[1] >= elf1[1] {
			count++
		}
	}

	fmt.Printf("There are %d assignment overlaps\n", count)
}

func partTwo(scanner *bufio.Scanner) {
	overlaps := 0

	for scanner.Scan() {
		line := scanner.Text()

		// Seperate assignments
		ids := strings.Split(line, ",")
		elf1 := strToIntSlice(ids[0])
		elf2 := strToIntSlice(ids[1])

		// 5-7, 7-9; 7 >= 5 and 7 <= 7
		// 2-8, 3-7; 3 >= 2 and 3 <= 8
		if elf2[0] >= elf1[0] && elf2[0] <= elf1[1] ||
			elf1[0] >= elf2[0] && elf1[0] <= elf2[1] {
			overlaps++
		}
	}

	fmt.Printf("Part Two: There are %d assignment overlaps\n", overlaps)
}

func strToIntSlice(x string) []int {
	out := make([]int, 2)

	// Seperate start and stop
	strs := strings.Split(x, "-")
	for i, v := range strs {
		var err error
		out[i], err = strconv.Atoi(v)
		if err != nil {
			log.Println(err)
		}
	}
	return out
}
