package main

import "fmt"

func main() {
	input := readInput("input.txt")
	claims := ParseInput(input)
	// 101781
	fmt.Println("Solving Day03A...")
	resultA := SolveA(claims)
	fmt.Println(resultA)
	// 909
	fmt.Println("Solving Day03B...")
	resultB := SolveB(claims)
	fmt.Println(resultB)
}
