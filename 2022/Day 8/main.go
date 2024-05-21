package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

// Input.txt 99by99
const rows = 99
const cols = 99

func main() {
	fileName := "input.txt"
	file, err := os.Open(fileName)
	if err != nil {
		fmt.Println(err)
	}

	// Split file into lines
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)
	matrix := readMatrix(scanner)
	file.Close()

	partOne(&matrix)
	partTwo(&matrix)
	// fmt.Println(matrix)
}

// TODO: proper comments
func partOne(m *[rows][cols]int) {
	mat := *m
	visibleTrees := 0
	var visible bool

	for i := 1; i < len(mat)-1; i++ {
		// Relative to current tree
		tallestFromLeft := mat[i][0]
		for j := 1; j < len(mat[i])-1; j++ {
			currentTree := mat[i][j]

			// Check if current tree is tallest from the left
			if currentTree > tallestFromLeft {
				visibleTrees++
				tallestFromLeft = currentTree
				continue
			}

			// Check if there are any taller trees on top
			visible = true
			for y := i - 1; y >= 0; y-- {
				if mat[y][j] >= currentTree {
					visible = false
					break
				}
			}
			if visible {
				visibleTrees++
				continue
			}

			// Check if there are any taller trees to the right
			visible = true
			for x := j + 1; x < len(mat[i]); x++ {
				if mat[i][x] >= currentTree {
					visible = false
					break
				}
			}
			if visible {
				visibleTrees++
				continue
			}

			// Check if there are any taller trees below
			visible = true
			for z := i + 1; z < len(mat); z++ {
				if mat[z][j] >= currentTree {
					visible = false
					break
				}
			}
			if visible {
				visibleTrees++
				continue
			}
		}
	}

	// Add number of trees on the edge
	visibleTrees += (len(mat) + (len(mat[0]) - 2)) * 2
	fmt.Println(visibleTrees, "Visible Trees (part 1)")
}

func partTwo(m *[rows][cols]int) {
	mat := *m
	var left, right, top, bottom, highestScore, score int

	for i := 1; i < len(mat)-1; i++ {
		for j := 1; j < len(mat[i])-1; j++ {
			currentTree := mat[i][j]

			left = 0
			for k := j - 1; k >= 0; k-- {
				left++
				if mat[i][k] >= currentTree {
					break
				}
			}

			// Check if there are any taller trees on top
			top = 0
			for y := i - 1; y >= 0; y-- {
				top++
				if mat[y][j] >= currentTree {
					break
				}
			}

			// Check if there are any taller trees to the right
			right = 0
			for x := j + 1; x < len(mat[i]); x++ {
				right++
				if mat[i][x] >= currentTree {
					break
				}
			}

			// Check if there are any taller trees below
			bottom = 0
			for z := i + 1; z < len(mat); z++ {
				bottom++
				if mat[z][j] >= currentTree {
					break
				}
			}

			score = left * top * right * bottom
			if score > highestScore {
				highestScore = score
			}
		}
	}

	// Add number of trees on the edge
	fmt.Println("Highest scenic score:", highestScore)
}

func readMatrix(scanner *bufio.Scanner) [rows][cols]int {
	mat := [rows][cols]int{}
	// Reads from file line by line
	var i = 0
	for scanner.Scan() {
		line := scanner.Text()
		row := [cols]int{}
		for j, v := range line {
			val, err := strconv.Atoi(string(v))
			if err != nil {
				panic(err)
			}
			row[j] = val
		}
		mat[i] = row
		i++
	}

	return mat
}
