module Day09 {
  private use IO;
  private use List;

  config const sample: bool = false;
  const fileName: string = if sample then "sample.txt" else "input.txt";
  const preambleSize: int = if sample then 5 else 25;

  proc main() {
    const input: list(int) = try! readInput(fileName);

    // 15353384
    writeln("Solving Day09A...");
    const resultA = solveA(input);
    writeln(resultA);

    // 2466556
    writeln("Solving Day09B...");
    const resultB = solveB(input, resultA);
    writeln(resultB);
  }

  proc solveA(input: list(int)): int {
    for i in preambleSize .. input.size-1 {
      const current = input[i];
      const preamble: domain(int) = input[i-preambleSize .. #preambleSize];
      //writeln(preamble);
      //writeln("i ", i, " ", input[i]);
      var valid: bool = false;
      for p in preamble {
        //writeln("p ", p);
        if preamble.contains(current - p) && 2*p != current {
          valid = true;
          break;
        }
      }
      if !valid then return current;
    }
    return -1;
  }

  proc solveB(input: list(int), n: int): int {
    for i in 0 .. #(input.size - 1) {
      //writeln(i, " ", input[i]);
      var currNum: int = input[i];
      var contSum: int = currNum;
      var contSumList: list(int);
      contSumList.append(currNum);
      for sumIndex in i+1 .. input.size-1 {
        if contSum >= n then break;
        currNum = input[sumIndex];
        contSum += currNum;
        contSumList.append(currNum);
      }
      //writeln(contSumList);
      if contSum == n && contSumList.size >= 2 {
        contSumList.sort();
        return contSumList.first() + contSumList.last();
      }
    }
    return -1;
  }

  proc readInput(fname: string): list(int) throws {
    var input: list(int);
    var f = open(fname, iomode.r);
    defer {
      try! f.close();
    }
    for line in f.lines() {
      input.append(line:int);
    }
    return input;
  }
}
