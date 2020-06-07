package main

import (
	"io/ioutil"
	"strings"
	"testing"
)

func TestSolveA(t *testing.T) {
	t.Log("SolveA")
	{
		file := "+1\n-2\n+3\n+1"
		t.Log("\tGiven file content: ", file)
		{
			input, _ := ioutil.ReadAll(strings.NewReader(file))
			result := SolveA(input)
			if 3 != result {
				t.Error("\t\tShould return 3, got: ", result)
			}
		}
	}
}

func TestSolveB(t *testing.T) {
	t.Log("SolveB")
	{
		cases := []struct {
			in  string
			out int64
		}{
			{
				"+1\n-1",
				0,
			},
			{
				"+3\n+3\n+4\n-2\n-4",
				10,
			},
			{
				"-6\n+3\n+8\n+5\n-6",
				5,
			},
			{
				"+7\n+7\n-2\n-7\n-4",
				14,
			},
		}
		for _, c := range cases {
			t.Log("\tGiven file content: ", c.in)
			{
				input, _ := ioutil.ReadAll(strings.NewReader(c.in))
				result := SolveB(input)
				if c.out != result {
					t.Errorf("\t\tShould return %v, got: %v", c.out, result)
				}
			}
		}
	}
}
