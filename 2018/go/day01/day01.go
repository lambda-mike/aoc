package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

const FREQ0 = 0

func readInput(fileName string) []byte {
	input, err := ioutil.ReadFile(fileName)
	if err != nil {
		log.Fatal(err)
	}
	return input
}

func SolveA(input []byte) int64 {
	freqChanges := readFreqChanges(input)
	return sumFreqChanges(freqChanges)
}

func SolveB(input []byte) int64 {
	freqChanges := readFreqChanges(input)
	return findDuplicate(freqChanges)
}

func findDuplicate(freqChanges []int64) int64 {
	var sum int64 = FREQ0
	freqsMap := map[int64]bool{FREQ0: true}
	freqsLen := len(freqChanges)
	i := 0
	for {
		sum += freqChanges[i%freqsLen]
		_, exists := freqsMap[sum]
		if exists {
			return sum
		} else {
			freqsMap[sum] = true
			i = (i + 1) % freqsLen
		}
	}
}

func sumFreqChanges(freqChanges []int64) int64 {
	var sum int64 = FREQ0
	for _, f := range freqChanges {
		sum += f
	}
	return sum
}

func readFreqChanges(input []byte) []int64 {
	lines := strings.Split(fmt.Sprintf("%s", input), "\n")
	var freqChanges []int64 = make([]int64, 0, len(input))
	for _, f := range lines {
		if f != "" {
			freqChage, err := strconv.ParseInt(f, 10, 64)
			if err != nil {
				log.Fatal(err)
			}
			freqChanges = append(freqChanges, freqChage)
		}
	}
	return freqChanges
}
