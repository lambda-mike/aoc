package main

import (
	"testing"
)

func TestCalculateCellPower(t *testing.T) {
	t.Log("CalculateCellPower")
	{
		for _, cell := range []struct {
			sn  int
			pos Pos
			pow int
		}{
			{8, Pos{2, 4}, 4},
			{57, Pos{121, 78}, -5},
			{39, Pos{216, 195}, 0},
			{71, Pos{100, 152}, 4},
		} {
			t.Log("\tGiven cell", cell)
			{
				pow := CalculateCellPower(cell.sn, cell.pos.X, cell.pos.Y)
				if cell.pow != pow {
					t.Errorf("Should return: %v, got: %v", cell.pow, pow)
				}
			}
		}
	}
}

func TestCreateGrid(t *testing.T) {
	t.Log("CreateGrid")
	{
		for _, tc := range []struct {
			sn  int
			pos Pos
			pow [9]int
		}{
			{18, Pos{32, 44}, [9]int{4, 4, 4, 3, 3, 4, 1, 2, 4}},
			{42, Pos{20, 60}, [9]int{4, 3, 3, 3, 3, 4, 3, 3, 4}},
		} {
			t.Log("\tGiven sn", tc.sn, "and pos", tc.pos)
			{
				grid := CreateGrid(tc.sn)
				x := tc.pos.X
				y := tc.pos.Y
				pow := [9]int{
					grid[x][y][1], grid[x+1][y][1], grid[x+2][y][1],
					grid[x][y+1][1], grid[x+1][y+1][1], grid[x+2][y+1][1],
					grid[x][y+2][1], grid[x+1][y+2][1], grid[x+2][y+2][1],
				}
				if tc.pow != pow {
					t.Errorf("Should return: %v, got: %v", tc.pow, pow)
				}
			}
		}
	}
}

func TestSolveA(t *testing.T) {
	t.Log("SolveA")
	{
		for _, tc := range []struct {
			sn  int
			pos Pos
		}{
			{18, Pos{33, 45}},
			{42, Pos{21, 61}},
		} {
			t.Log("\tGiven sn", tc.sn)
			{
				grid := CreateGrid(tc.sn)
				pos := SolveA(grid)
				if tc.pos != pos {
					t.Errorf("Should return: %v, got: %v", tc.pos, pos)
				}
			}
		}
	}
}

func TestCheckLatestMaxCellWindowSize(t *testing.T) {
	t.Log("CheckLatestMaxCellWindowSize")
	{
		var (
			g1 Grid
			g2 Grid
			g3 Grid
		)
		g1[0][0] = map[int]int{1: 3}
		g2[0][0] = map[int]int{1: 3, 3: 4}
		g3[0][0] = map[int]int{1: 3, 22: 2}
		for _, tc := range []struct {
			g    Grid
			size int
		}{
			{g1, 1},
			{g2, 3},
			{g3, 22},
		} {
			t.Log("\tGiven grid", tc.g[0][0])
			{
				size := checkLatestMaxCellWindowSize(&tc.g)
				if tc.size != size {
					t.Errorf("Should return: %v, got: %v", tc.size, size)
				}
			}
		}
	}
}
