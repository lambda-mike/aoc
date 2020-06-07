package main

import "fmt"

func main() {
	input := readInput("input.txt")
	// 510
	fmt.Println("Solving Day01A...")
	resultA := SolveA(input)
	fmt.Println(resultA)
	// 69074
	fmt.Println("Solving Day01B...")
	resultB := SolveB(input)
	fmt.Println(resultB)
}
