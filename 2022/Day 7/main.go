// Advent of Code
// Day 7
// https://adventofcode.com/2022/day/7

package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type dir struct {
	name      string
	space     int
	subDirs   []dir
	parentDir *dir
}

const maxSum = 100000
const maxSpace = 70000000
const freeSpaceNeeded = 30000000

func main() {
	fileName := "test.txt"
	var root dir

	file, err := os.Open(fileName)
	if err != nil {
		fmt.Println(err)
	}

	// Split file into lines
	scanner := bufio.NewScanner(file)
	scanner.Split(bufio.ScanLines)

	root = createDirsFromFile(scanner)

	file.Close()
	partOne(&root)

	fmt.Print("Part Two: ")
	// Max the hard drive can have used to run the program
	spaceCap := maxSpace - freeSpaceNeeded
	if root.space > spaceCap {
		spaceToLose := root.space - spaceCap
		space := root.space
		partTwo(&root, &space, spaceToLose)
		fmt.Println(space)
	} else {
		fmt.Println("Enough Space")
	}
}

func partOne(root *dir) {
	sum := sumSpace(root, 0)
	fmt.Printf("Sum of space of dirs with less than %d space: %d\n", maxSum, sum)
	printDir(root, 0)
}

func partTwo(root *dir, space *int, spaceToLose int) int {
	for _, v := range root.subDirs {
		if v.space < *space && v.space > spaceToLose {
			*space = v.space
		}

		if len(v.subDirs) != 0 {
			partTwo(&v, space, spaceToLose)
		}
	}

	return *space
}

// Creates directories from file and return topmost level
func createDirsFromFile(scanner *bufio.Scanner) dir {
	root := dir{name: "root"}
	currentDir := &root

	// Reads from file line by line
	for scanner.Scan() {
		line := scanner.Text()
		text := strings.Split(line, " ")

		if text[0] == "$" {
			if text[1] == "cd" {
				// Do nothing if root
				if text[2] == "/" {
					continue
				} else if text[2] == ".." {
					// Go up one level
					currentDir = currentDir.parentDir
				} else {
					currentDir = createDir(currentDir, text[2])
				}
			}
			// If "ls" do nothing
			// No need to do anything
		} else if text[0] == "dir" {
			createDir(currentDir, text[1])
		} else {
			// For the sake of simplicity
			// assume it will be a file
			// Look at input.txt if you dont understand
			size, err := strconv.Atoi(text[0])
			if err != nil {
				panic(err)
			}

			currentDir.space += size

			// Increment space of parent dirs
			parent := currentDir.parentDir
			for parent != nil {
				parent.space += size
				parent = parent.parentDir
			}
		}
	}

	return root
}

func sumSpace(currentDir *dir, sum int) int {
	// Assume root dir space will exceed maxSum
	for _, v := range currentDir.subDirs {
		if v.space <= maxSum {
			sum += v.space
		}

		if len(v.subDirs) != 0 {
			sum = sumSpace(&v, sum)
		}
	}

	return sum
}

// Creates dir if not exists
// And returns
func createDir(currentDir *dir, name string) *dir {
	if i := dirExists(currentDir, name); i != -1 {
		return &currentDir.subDirs[i]
	}

	newDir := dir{name: name, parentDir: currentDir}
	currentDir.subDirs = append(currentDir.subDirs, newDir)

	return &newDir
}

// Returns index of dir with name "name"
// -1 if not exists
func dirExists(currentDir *dir, name string) int {
	for i, v := range currentDir.subDirs {
		if v.name == name {
			return i
		}
	}

	return -1
}

func printDir(currentDir *dir, depth int) {
	fmt.Printf("%s- %s (%d)\n", strings.Repeat("\t", depth), currentDir.name, currentDir.space)
	if len(currentDir.subDirs) != 0 {
		for _, v := range currentDir.subDirs {
			printDir(&v, depth+1)
		}
	}
}
