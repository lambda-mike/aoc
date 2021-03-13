// TODO read file to list
// TODO change list to array?
// TODO solveA
// TODO solveB


module Day09 {
  private use IO;
  private use List;

  config const sample: bool = false;
  const fileName: string = if sample then "sample.txt" else "input.txt";

  proc main() throws {
    const input: list(int) = readInput(fileName);
    writeln("Solving Day09A...");
    const resultA = solveA(input);
    writeln(resultA);
  }

  proc solveA(input: list(int)): int {
    writeln(input);
    return 0;
  }

  proc readInput(fname: string): list(int) throws {
    var input: list(int);
    var f = open(fname, iomode.r);
    for line in f.lines() {
      input.append(line:int);
    }
    f.close();
    return input;
  }
}
