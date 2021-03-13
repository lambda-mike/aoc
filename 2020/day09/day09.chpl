// TODO config param to run sample or not
// TODO read file to list
// TODO change list to array?
// TODO solveA
// TODO solveB


module Day09 {
  config const sample: bool = false;
  const fileName: string = if sample then "sample.txt" else "input.txt";

  proc main() {
    writeln("Solving Day09A...");
    const resultA = solveA();
    writeln(resultA);
  }

  proc solveA(): int {
    return 0;
  }
}
