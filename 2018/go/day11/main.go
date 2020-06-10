package main

import "fmt"

func main() {
	// {34 72}
	fmt.Println("Solving Day11A...")
	grid := CreateGrid(GRID_SN)
	resultA := SolveA(grid)
	fmt.Println(resultA)
	// // 90,269,16
	// fmt.Println("Solving sample B1...")
	// gridB1 := CreateGrid(18)
	// resultB1 := SolveB(gridB1)
	// fmt.Println(resultB1)
	// // 232,251,12
	// fmt.Println("Solving sample B2...")
	// gridB2 := CreateGrid(42)
	// resultB2 := SolveB(gridB2)
	// fmt.Println(resultB2)
	// {{233 187} 13}
	fmt.Println("Solving Day11B...")
	resultB := SolveB(grid)
	fmt.Println(resultB)
}
