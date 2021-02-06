import gleam/bit_string
import gleam/function
import gleam/int
import gleam/io.{println}
import gleam/list
import gleam/result
import gleam/set
import gleam/string

// Ignore errors and use Nil
external fn do_read_to_bitstring(
  filename: String,
) -> Result(BitString, Nil) =
  "file" "read_file"

pub fn read_input(file: String) -> Result(String, Nil) {
  file
  |> do_read_to_bitstring
  |> result.then(bit_string.to_string)
}

pub fn parse_input(input: String) -> List(List(String)) {
  input
  |> string.split("\n")
  |> list.fold(from: [], with: fn(line, answers) {
    case string.is_empty(line) {
      // Ignore the fact that it is possible one of the lines in answers
      // might be empty if input ends with two \n chars
      True -> [[], ..answers]
      False -> case answers {
        [] -> [[line]]
        [current, ..rest] -> {
          let next = [line, ..current]
          [next, ..rest]
        }
      }
    }
  })
}

pub fn count_answers(answers: List(String)) -> Int {
  answers
  |> list.map(string.to_graphemes)
  |> list.flatten
  |> list.unique
  |> list.length
}

pub fn count_unanimous_answers(group_answers: List(String)) -> Int {
  let all_answers_set =
    group_answers
    |> list.map(string.to_graphemes)
    |> list.flatten
    |> set.from_list
  group_answers
  |> list.map(function.compose(string.to_graphemes, set.from_list))
  |> list.fold(from: all_answers_set, with: set.intersection)
  |> set.size
}

pub fn solve_a(input: String) -> Int {
  input
  |> parse_input
  |> list.map(count_answers)
  |> int.sum
}

pub fn solve_b(input: String) -> Int {
  input
  |> parse_input
  |> list.map(count_unanimous_answers)
  |> int.sum
}

pub fn main(args) {
  let file = "input.txt"
  case read_input(file) {
    Ok(input) -> {
      // 6735
      io.println("Solving Day06A...")
      let result_a = solve_a(input)
      io.println(int.to_string(result_a))

      // 3221
      io.println("Solving Day06B...")
      let result_b = solve_b(input)
      io.println(int.to_string(result_b))
    }
    Error(_) ->
      io.println("Reading input file failed")
  }
}
