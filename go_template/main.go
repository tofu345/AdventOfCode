package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	file, err := os.Open("test.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()

	sc := bufio.NewScanner(file)
	if err := sc.Err(); err != nil {
		log.Fatal(err)
	}

	partOne(sc)
}

func partOne(sc *bufio.Scanner) {
	for sc.Scan() {
		line := sc.Text()
		fmt.Println(line)
	}
}

func partTwo(sc *bufio.Scanner) {
}
