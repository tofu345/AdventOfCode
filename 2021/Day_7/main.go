package main

import (
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func main() {
	bytes, err := os.ReadFile("input.txt")
	if err != nil {
		fmt.Print(err)
	}
	fileContents := strings.TrimSpace(string(bytes))
	data := parse(fileContents)

	partOne(&data)
	partTwo(&data)
}

func partOne(data *[]int) {
	max_pos := max_position(data)
	var min_fuel_cost int

	for v := range max_pos {
		var fuel_cost int
		for _, num := range *data {
			cost := num - v
			if cost < 0 {
				cost *= -1
			}
			fuel_cost += cost
		}

		if min_fuel_cost == 0 || fuel_cost < min_fuel_cost {
			min_fuel_cost = fuel_cost
		}
	}

	fmt.Printf("Part One: %v\n", min_fuel_cost)
}

func partTwo(data *[]int) {
	max_pos := max_position(data)
	var min_fuel_cost int

	for v := range max_pos {
		var fuel_cost int
		for _, num := range *data {
			distance := num - v
			if distance < 0 {
				distance *= -1
			}

			for v := range distance {
				fuel_cost += v + 1
			}
		}

		if min_fuel_cost == 0 || fuel_cost < min_fuel_cost {
			min_fuel_cost = fuel_cost
		}
	}

	fmt.Printf("Part Two: %v\n", min_fuel_cost)

}

func max_position(data *[]int) int {
	var max_pos int
	for _, v := range *data {
		max_pos = max(v, max_pos)
	}

	return max_pos
}

func parse(fileContents string) []int {
	values := make([]int, 0, len(fileContents))
	splits := strings.Split(fileContents, ",")
	for _, str := range splits {
		v, err := strconv.Atoi(str)
		if err != nil {
			log.Fatal(err)
		}

		values = append(values, v)
	}

	return values
}
