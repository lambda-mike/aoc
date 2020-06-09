package main

import (
	"io/ioutil"
	"strings"
	"testing"
)

const sample string = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"

func TestParseInput(t *testing.T) {
	t.Log("ParseInput")
	{
		input, _ := ioutil.ReadAll(strings.NewReader(sample))
		t.Log("\tGiven file content: ", sample)
		{
			result := ParseInput(input)
			const expectedLen int = 3
			if expectedLen != len(result) {
				t.Errorf("\t\tShould return %v elements, got: %v", expectedLen, len(result))
			}

			claim1 := Claim{1, Pos{1, 3}, 4, 4}
			actual := result[0]
			if claim1 != actual {
				t.Errorf("\t\t%v is not %v", actual, claim1)
			}

			claim2 := Claim{2, Pos{3, 1}, 4, 4}
			actual = result[1]
			if claim2 != actual {
				t.Errorf("\t\t%v is not %v", actual, claim2)
			}

			claim3 := Claim{3, Pos{5, 5}, 2, 2}
			actual = result[2]
			if claim3 != actual {
				t.Errorf("\t\t%v is not %v", actual, claim3)
			}

		}
	}
}

func TestSolveA(t *testing.T) {
	t.Log("SolveA")
	{
		input, _ := ioutil.ReadAll(strings.NewReader(sample))
		claims := ParseInput(input)
		t.Log("\tGiven sample claims: ", claims)
		{
			const expected int = 4
			result := SolveA(claims)
			if expected != result {
				t.Errorf("\t\tShould return %v, got: %v", expected, result)
			}
		}
	}
}

func TestSolveB(t *testing.T) {
	t.Log("SolveB")
	{
		input, _ := ioutil.ReadAll(strings.NewReader(sample))
		claims := ParseInput(input)
		t.Log("\tGiven sample claims: ", claims)
		{
			const expected ClaimId = 3
			result := SolveB(claims)
			if expected != result {
				t.Errorf("\t\tShould return id %v, got: %v", expected, result)
			}
		}
	}
}
