// Advent Of Code
// January 12th, 2023
// https://adventofcode.com/2022/day/2

package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

/*
Opponent, You
A, X - Rock
B, Y - Paper
C, Z - Scissors
1 - Rock, 2 - Paper, and 3 - Scissors

Rock - Paper - Win
Rock - Scissors - Loss
Paper - Rock - Loss
Paper - Scissors - Win
Scissors - Rock - Win
Scissors - Paper - Loss
Same - Draw
0 - Loss, 3 - Draw, and 6 - Win
*/

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

// Uses switch case to calc score for what was played (rock, paper, scissors)
func partOne(scanner *bufio.Scanner) {
	// Convert X to A, Y to B, Z to C for simplicity
	conv := map[string]string{"X": "A", "Y": "B", "Z": "C"}
	scores := map[string]int{"A": 1, "B": 2, "C": 3}
	score := 0
	for scanner.Scan() {
		data := strings.Split(scanner.Text(), " ")
		foe := data[0]
		you := data[1]
		// fmt.Println(foe, you)

		// score += (Win, Lose, Draw) + (score for rock, paper, scissors)
		switch {
		case foe == conv[you]:
			score += 3 + scores[foe]
		case foe == "A" && you == "Y":
			// Rock - Paper - Win
			score += 6 + 2
		case foe == "A" && you == "Z":
			// Rock - Scissors - Loss
			score += 0 + 3
		case foe == "B" && you == "X":
			// Paper - Rock - Loss
			score += 0 + 1
		case foe == "B" && you == "Z":
			// Paper - Scissors - Win
			score += 6 + 3
		case foe == "C" && you == "X":
			// Scissors - Rock - Win
			score += 6 + 1
		case foe == "C" && you == "Y":
			// Scissors - Paper - Loss
			score += 0 + 2
		}
	}

	fmt.Println("You had a score of", score)
}

// Uses a hashmap to store what should be played to win, lose
func partTwo(scanner *bufio.Scanner) {
	scores := map[string]int{"A": 1, "B": 2, "C": 3}
	win := map[string]string{"A": "B", "B": "C", "C": "A"}
	lose := map[string]string{"A": "C", "B": "A", "C": "B"}
	score := 0

	for scanner.Scan() {
		data := strings.Split(scanner.Text(), " ")
		foe := data[0]
		end := data[1]

		switch {
		case end == "X":
			// Lose
			score += 0 + scores[lose[foe]]
		case end == "Y":
			// Draw
			score += 3 + scores[foe]
		case end == "Z":
			// Win
			score += 6 + scores[win[foe]]
		}

		// fmt.Println("foe:", foe)
		// fmt.Println("end:", end, "lose:", scores[lose[foe]], "win:", scores[win[foe]])
		// fmt.Println("score", score)
		// fmt.Println()
	}

	fmt.Println("Part 2: You had a score of", score)
}
