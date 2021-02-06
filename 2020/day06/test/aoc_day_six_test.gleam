import aoc_day_six
import gleam/list
import gleam/should

pub fn parse_input_test() {
  "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
  |> aoc_day_six.parse_input
  |> should.equal(
    list.reverse([
      ["abc"],
      ["c", "b", "a"],
      ["ac", "ab"],
      ["a", "a", "a", "a"],
      ["b"]])
    )
}

pub fn count_answers_test() {
  ["abc"]
  |> aoc_day_six.count_answers
  |> should.equal(3)
  ["c", "b", "a"]
  |> aoc_day_six.count_answers
  |> should.equal(3)
  ["ac", "ab"]
  |> aoc_day_six.count_answers
  |> should.equal(3)
  ["a", "a", "a", "a"]
  |> aoc_day_six.count_answers
  |> should.equal(1)
  ["b"]
  |> aoc_day_six.count_answers
  |> should.equal(1)
}

pub fn count_unanimous_answers_test() {
  ["abc"]
  |> aoc_day_six.count_unanimous_answers
  |> should.equal(3)
  ["c", "b", "a"]
  |> aoc_day_six.count_unanimous_answers
  |> should.equal(0)
  ["ac", "ab"]
  |> aoc_day_six.count_unanimous_answers
  |> should.equal(1)
  ["a", "a", "a", "a"]
  |> aoc_day_six.count_unanimous_answers
  |> should.equal(1)
  ["b"]
  |> aoc_day_six.count_unanimous_answers
  |> should.equal(1)
}
