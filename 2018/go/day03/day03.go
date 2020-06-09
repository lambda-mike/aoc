package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

const FABRIC_SIZE = 1000

type Pos struct {
	x int
	y int
}

type Claim struct {
	id  int
	pos Pos
	w   int
	h   int
}

func SolveA(claims []Claim) int {
	var (
		claimsHeatmap [FABRIC_SIZE][FABRIC_SIZE]int
		counter       int
	)
	for _, cl := range claims {
		x := cl.pos.x
		y := cl.pos.y
		for row := y; row < y+cl.h; row++ {
			for col := x; col < x+cl.w; col++ {
				if claimsHeatmap[col][row] == 1 {
					counter += 1
				}
				claimsHeatmap[col][row] += 1
			}
		}
	}
	return counter
}

func SolveB(claims []Claim) int {
	var claimsVolume [FABRIC_SIZE][FABRIC_SIZE]map[int]int
	notOverlappingClaims := make(map[int]int)
	// Init map - all claimes are not overlapping
	for _, cl := range claims {
		notOverlappingClaims[cl.id] = 0
	}
	for _, cl := range claims {
		x := cl.pos.x
		y := cl.pos.y
		for row := y; row < y+cl.h; row++ {
			for col := x; col < x+cl.w; col++ {
				if nil == claimsVolume[col][row] {
					claimsVolume[col][row] = make(map[int]int)
				}
				claimsVolume[col][row][cl.id] = 0
				if len(claimsVolume[col][row]) > 1 {
					delete(notOverlappingClaims, cl.id)
					for otherId, _ := range claimsVolume[col][row] {
						delete(notOverlappingClaims, otherId)
					}
				}
			}
		}
	}
	for id, _ := range notOverlappingClaims {
		return id
	}
	panic("Could not find not overlapping id")
	return 0
}

func readInput(fileName string) []byte {
	input, err := ioutil.ReadFile(fileName)
	if err != nil {
		log.Fatal(err)
	}
	return input
}

func ParseInput(input []byte) []Claim {
	lines := strings.Split(fmt.Sprintf("%s", input), "\n")
	var claims []Claim = make([]Claim, 0, len(input))
	for _, line := range lines {
		if line == "" {
			break
		}
		chunks := strings.Split(line, " ")
		id, err := strconv.Atoi(chunks[0][1:])
		if err != nil {
			log.Fatal(err)
		}
		coords := strings.Split(chunks[2], ",")
		x, err := strconv.Atoi(coords[0])
		if err != nil {
			log.Fatal(err)
		}
		y, err := strconv.Atoi(coords[1][:len(coords[1])-1])
		if err != nil {
			log.Fatal(err)
		}
		size := strings.Split(chunks[3], "x")
		w, err := strconv.Atoi(size[0])
		if err != nil {
			log.Fatal(err)
		}
		h, err := strconv.Atoi(size[1])
		if err != nil {
			log.Fatal(err)
		}
		cl := Claim{id, Pos{x, y}, w, h}
		claims = append(claims, cl)
	}
	return claims
}
