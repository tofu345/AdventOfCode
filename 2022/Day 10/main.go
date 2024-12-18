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
	filePath := "input.txt"
	readFile, err := os.Open(filePath)

	if err != nil {
		fmt.Println(err)
	}
	fileScanner := bufio.NewScanner(readFile)
	fileScanner.Split(bufio.ScanLines)
	var fileLines []string

	for fileScanner.Scan() {
		fileLines = append(fileLines, fileScanner.Text())
	}

	fmt.Println(len(fileLines))

	partOne(fileLines)
}

type Instruction struct {
	Operation string
	Cycles    int
	Value     int
}

func parseInstructions(lines []string) []Instruction {
	objs := []Instruction{}
	for _, v := range lines {
		words := strings.Split(v, " ")
		if words[0] == "addx" {
			value, err := strconv.Atoi(words[1])
			if err != nil {
				log.Fatal(err)
			}

			objs = append(objs, Instruction{Operation: words[0], Value: value, Cycles: 2})
		} else {
			objs = append(objs, Instruction{Operation: words[0], Value: 0, Cycles: 1})
		}
	}

	return objs
}

func partOne(fileLines []string) {
	instructions := parseInstructions(fileLines)

	X := 1
	signalStrength := 0
	i := 0
	for cycle := 1; cycle <= 220; cycle++ {
		if (cycle-20)%40 == 0 {
			signalStrength += X * cycle
			fmt.Println(signalStrength)
		}

		switch instructions[i].Operation {
		case "addx":
			instructions[i].Cycles--
			if instructions[i].Cycles == 0 {
				X += instructions[i].Value
				i++
			}
		case "noop":
			i++
		}
	}

	fmt.Println("Signal strength:", signalStrength)
}
