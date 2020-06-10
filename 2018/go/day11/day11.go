package main

// TODO profile to see mem consumption
// TODO try removing intermediate cell grid windows and profile again

import ()

const (
	GRID_SIZE = 300
	GRID_SN   = 7400
)

type Pos struct {
	X int
	Y int
}

type Cell struct {
	Pos  Pos
	Size int
}

type Grid [GRID_SIZE][GRID_SIZE]map[int]int

func SolveA(grid *Grid) Pos {
	var sizeLimit int = 3
	pos, _ := findMaxPower(grid, sizeLimit)
	return translateIndexes(pos)
}

func SolveB(grid *Grid) Cell {
	var sizeLimit int = GRID_SIZE
	pos, size := findMaxPower(grid, sizeLimit)
	return Cell{translateIndexes(pos), size}
}

func CreateGrid(serialNum int) *Grid {
	var grid Grid
	for row := 0; row < GRID_SIZE; row++ {
		for col := 0; col < GRID_SIZE; col++ {
			pow := CalculateCellPower(serialNum, col, row)
			grid[col][row] = map[int]int{1: pow}
		}
	}
	return &grid
}

func CalculateCellPower(serialNum int, x int, y int) (power int) {
	rackId := x + 1 + 10 // internally indexed from 0
	power = rackId * (y + 1)
	power += serialNum
	power *= rackId
	power = power / 100 % 10
	power -= 5
	return
}

func findMaxPower(grid *Grid, sizeLimit int) (Pos, int) {
	var (
		// Lowest possible is 9 * -5
		maxCellWinPow  int = -46
		maxCellWinPos  Pos
		maxCellWinSize int
		windowSize     = checkLatestMaxCellWindowSize(grid)
	)
	for size := windowSize + 1; size <= sizeLimit; size++ {
		for row := 0; row <= GRID_SIZE-size; row++ {
			for col := 0; col <= GRID_SIZE-size; col++ {
				// One size smaller win in the same pos
				var cellWinPow int = grid[col][row][size-1]
				// Size one cells on the right edge
				for winRow := 0; winRow < size; winRow++ {
					cellWinPow += grid[col+size-1][row+winRow][1]
				}
				// Size one cells on the bottom edge
				// -1 because right edge already covers the cell
				for winCol := 0; winCol < size-1; winCol++ {
					cellWinPow += grid[col+winCol][row+size-1][1]
				}
				grid[col][row][size] = cellWinPow
				if cellWinPow > maxCellWinPow {
					maxCellWinPow = cellWinPow
					maxCellWinPos = Pos{col, row}
					maxCellWinSize = size
				}
			}
		}
	}
	return maxCellWinPos, maxCellWinSize
}

func checkLatestMaxCellWindowSize(grid *Grid) int {
	firstCell := grid[0][0]
	maxSize := 0
	for size, _ := range firstCell {
		if size > maxSize {
			maxSize = size
		}
	}
	return maxSize
}

func translateIndexes(pos Pos) Pos {
	// +1 because internally indexes start from 0
	// Puzzle operates on indexes numbered from 1
	pos.X += 1
	pos.Y += 1
	return pos
}
