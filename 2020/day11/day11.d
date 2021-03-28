import std.algorithm.comparison : equal;
import std.algorithm.iteration;
import std.algorithm.searching;
import std.array;
import std.stdio;

enum Cell { Ground, Free, Taken }

struct Pos {
  ulong x;
  ulong y;
}

void main() {
  //test();

  //auto fileName = "sample.txt";
  //auto fileName = "sample2.txt";
  auto fileName = "input.txt";
  Cell[][] seats =
    File(fileName)
    .byLineCopy()
    .map!(a => a.map!(b => parseCell(b)).array())
    .array();

  // 2354
  writeln("Solving Day11A...");
  auto resultA = solveA(seats);
  writeln(resultA);

  // 2072
  writeln("Solving Day11B...");
  auto resultB = solveB(seats);
  writeln(resultB);
}

ulong solveA(Cell[][] seats) {
  auto crowdRatio = 4;
  auto shortRange = true;
  return solve(seats, crowdRatio, shortRange);
}

ulong solveB(Cell[][] seats) {
  auto crowdRatio = 5;
  auto shortRange = false;
  return solve(seats, crowdRatio, shortRange);
}

ulong solve(Cell[][] input, int crowdRatio, bool shortRange) {
  auto seats = cloneCells(input);
  auto oldSeats = cloneCells(input);
  calcNextRound(seats, oldSeats, crowdRatio, shortRange);
  while (seatsDiffer(seats, oldSeats)) {
    copyCells(seats, oldSeats);
    calcNextRound(seats, oldSeats, crowdRatio, shortRange);
  }
  return countSeatsTaken(seats);
}

Cell[][] cloneCells(const Cell[][] from) {
  auto cells = new Cell[][](from.length, from[0].length);
  foreach (i, _; from) {
    cells[i] = from[i].dup;
  }
  return cells;
}

void copyCells(const Cell[][] from, Cell[][] to) {
  foreach (y, row; from) {
    foreach (x, _; row) {
      to[y][x] = from[y][x];
    }
  }
}

// crowdRatio = the least amount of neighbours causing cell to switch to Free
// shortRange = look only one cell in certain direction or as many as possible
void calcNextRound(Cell[][] seats, const Cell[][] oldSeats, int crowdRatio, bool shortRange) {
  auto maxY = oldSeats.length;
  auto maxX = oldSeats[0].length;
  auto posLimit = Pos(maxX, maxY);
  foreach (y, row; seats) {
    foreach (x, cell; row) {
      final switch (cell) {
        case Cell.Ground:
          continue;
        case Cell.Free:
          if (noNeighbours(Pos(x, y), oldSeats, posLimit, shortRange))
            seats[y][x] = Cell.Taken;
          break;
        case Cell.Taken:
          if (tooManyNeighbours(Pos(x, y), oldSeats, posLimit, shortRange, crowdRatio))
            seats[y][x] = Cell.Free;
          break;
      }
    }
  }
}

bool isPosValid(const Pos pos, const Pos limit) {
  return pos.x >=0 && pos.y >= 0 && pos.x < limit.x && pos.y < limit.y;
}

auto getDirections() {
  return [
    Pos(-1, -1), Pos(0, -1), Pos(1, -1),
    Pos(-1,  0),             Pos(1,  0),
    Pos(-1, +1), Pos(0, +1), Pos(1, +1),
  ];
}

Pos move(const Pos pos, const Pos dir) {
  return Pos(pos.x + dir.x, pos.y + dir.y);
}

// Ground represents no seat was found
Cell findFirstSeatInDirection(
  const Cell[][] seats,
  const Pos start,
  const Pos dir,
  const Pos limit,
  const bool shortRange) {
  Pos pos = move(start, dir);
  while (isPosValid(pos, limit)) {
    auto cell = seats[pos.y][pos.x];
    if (cell != Cell.Ground)
      return cell;
    if (shortRange)
      break;
    pos = move(pos, dir);
  }
  return Cell.Ground;
}

bool noNeighbours(
  const Pos start,
  const Cell[][] oldSeats,
  const Pos limit,
  const bool shortRange) {
  return !getDirections()
    .map!(dir => findFirstSeatInDirection(oldSeats, start, dir, limit, shortRange))
    .any!(s => s == Cell.Taken);
}

bool tooManyNeighbours(
  const Pos start,
  const Cell[][] oldSeats,
  const Pos limit,
  const bool shortRange,
  const int crowdRatio) {
  auto seats = getDirections()
    .map!(dir => findFirstSeatInDirection(oldSeats, start, dir, limit, shortRange));
  return count(seats, Cell.Taken) >= crowdRatio;
}

bool seatsDiffer(const Cell[][] seats, const Cell[][] oldSeats) {
  foreach (y, row; seats) {
    foreach (x, cell; row) {
      if (cell != oldSeats[y][x]) return true;
    }
  }
  return false;
}

auto countSeatsTaken(const Cell[][] seats) {
  return seats
    .map!(row => count(row, Cell.Taken))
    .sum();
  //return seats.fold!((s, row) => count(row, Cell.Taken) + s)(0);
}

Cell parseCell(dchar c) {
  // Assumption: input is always correct, no strange chars present
  final switch (c) {
    case 'L':
      return Cell.Free;
    case '.':
      return Cell.Ground;
    case '#':
      return Cell.Taken;
  }
}

void test() {
  auto testParseCell = ".#L.".map!(a => parseCell(a)).array();
  auto testParseCellExpected = [ Cell.Ground, Cell.Taken, Cell.Free, Cell.Taken ];
  assert(equal(testParseCell, testParseCellExpected), "parseCell does not work!");
}
