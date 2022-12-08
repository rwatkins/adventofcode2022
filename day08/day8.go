package main

import (
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"
)

type Tree struct{ row, col, height int }

func readFile(file string) string {
	data, err := ioutil.ReadFile(file)
	if err != nil {
		panic(err)
	}
	text := string(data)
	return text
}

func buildGrid(input string) [][]int {
	grid := [][]int{}
	lines := strings.Split(input, "\n")
	for i, line := range lines {
		if line == "" {
			continue
		}
		grid = append(grid, []int{})
		for _, c := range strings.Split(line, "") {
			height, err := strconv.Atoi(c)
			if err != nil {
				panic(err)
			}
			grid[i] = append(grid[i], height)
		}
	}
	return grid
}

func part1(input string) int {
	visibleTrees := map[Tree]int{}
	grid := buildGrid(input)

	// Rows, top to bottom
	for i, row := range grid {
		var lineOfSight []int
		// Left to right
		for j, height := range row {
			if len(lineOfSight) == 0 || height > lineOfSight[len(lineOfSight)-1] {
				// Record visible height
				lineOfSight = append(lineOfSight, height)
				// Record visible tree
				visibleTrees[Tree{i, j, height}] = 1
			}
		}
		lineOfSight = []int{}

		// Right to left
		for j := len(row) - 1; j >= 0; j-- {
			height := row[j]
			if len(lineOfSight) == 0 || height > lineOfSight[len(lineOfSight)-1] {
				// Record visible height
				lineOfSight = append(lineOfSight, height)
				// Record visible tree
				visibleTrees[Tree{i, j, height}] = 1
			}
		}
	}

	// Columns, left to right
	for i := 0; i < len(grid[0]); i++ {
		lineOfSight := []int{}
		// Rows, top to bottom
		for j := 0; j < len(grid); j++ {
			height := grid[j][i]
			if len(lineOfSight) == 0 || height > lineOfSight[len(lineOfSight)-1] {
				// Record visible height
				lineOfSight = append(lineOfSight, height)
				// Record visible tree
				visibleTrees[Tree{j, i, height}] = 1
			}
		}
		lineOfSight = []int{}

		// Rows, bottom to top
		for j := len(grid) - 1; j >= 0; j-- {
			height := grid[j][i]
			if len(lineOfSight) == 0 || height > lineOfSight[len(lineOfSight)-1] {
				// Record visible height
				lineOfSight = append(lineOfSight, height)
				// Record visible tree
				visibleTrees[Tree{j, i, height}] = 1
			}
		}
	}

	return len(visibleTrees)
}

func part2(input string) int {
	grid := buildGrid(input)
	maxScore := 0
	// Rows
	for row := 0; row < len(grid); row++ {
		// Columns
		for col := 0; col < len(grid[row]); col++ {
			height := grid[row][col]
			// Up
			upScore := 0
			for i := row - 1; i >= 0; i-- {
				upScore += 1
				if grid[i][col] >= height {
					break
				}
			}
			// Down
			downScore := 0
			for i := row + 1; i < len(grid); i++ {
				downScore += 1
				if grid[i][col] >= height {
					break
				}
			}
			// Left
			leftScore := 0
			for i := col - 1; i >= 0; i-- {
				leftScore += 1
				if grid[row][i] >= height {
					break
				}
			}
			// Right
			rightScore := 0
			for i := col + 1; i < len(grid[row]); i++ {
				rightScore += 1
				if grid[row][i] >= height {
					break
				}
			}
			score := upScore * downScore * leftScore * rightScore
			if score > maxScore {
				maxScore = score
			}
		}
	}
	return maxScore
}

func main() {
	input := readFile("day8_input.txt")
	fmt.Println("Part 1:", part1(input))
	fmt.Println("Part 2:", part2(input))
}
