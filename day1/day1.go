package main

import (
	"fmt"
	"io/ioutil"
	"sort"
	"strconv"
	"strings"
)

func main() {
	data, err := ioutil.ReadFile("day1_input.txt")
	if err != nil {
		panic(err)
	}
	text := string(data)
	lines := strings.Split(text, "\n")

	// Iterate over the lines in the file.
	// If the line is a number, add it to the current calorie count.
	// If the line is blank, stash the current calorie count and start a new
	// count at 0.
	var arr []int
	calories := 0
	for _, line := range lines {
		if line == "" {
			arr = append(arr, calories)
			calories = 0
		} else {
			i, err := strconv.Atoi(line)
			if err != nil {
				panic(err)
			}
			calories += i
		}
	}

	// Part 1: Find the largest calorie count
	sort.Ints(arr)
	maxCals := arr[len(arr)-1]
	fmt.Printf("Part 1: %d\n", maxCals)

	// Part 2: Find the sum of the 3 largest calorie counts
	topThreeTotal := arr[len(arr)-1] + arr[len(arr)-2] + arr[len(arr)-3]
	fmt.Printf("Part 2: %d\n", topThreeTotal)
}
