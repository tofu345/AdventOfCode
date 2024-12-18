package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("input.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	sc := bufio.NewScanner(file)
	floor := 0
	for sc.Scan() {
		line := sc.Text()
		for i, v := range line {
			if v == '(' {
				floor++
			} else {
				floor--
			}

			if floor == -1 {
				fmt.Printf("Santa enters the basement on floor %v\n", i+1)
				return
			}
		}
	}

	fmt.Printf("Floor: %v\n", floor)

	if err := sc.Err(); err != nil {
		log.Fatal(err)
	}
}
