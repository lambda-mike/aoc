class Cup {
  final var n: Int
  var prev: Int = 0
  var next: Int = 0

  init(n: Int, prev: Int, next: Int) {
    self.n = n
    self.prev = prev
    self.next = next
  }

  func show() {
    print("cup \(self.prev) [\(self.n)] \(self.next)")
  }
}

struct Game {

  var cups: [Cup] = []
  var currentCup: Int = 0

  init(input: [Int], upperLimit: Int) {
    // Cup at index 0 is a dummy cup to align remainig caps with their
    // indices: 1...n
    for _ in 0...upperLimit {
      let cup = Cup(n: 0, prev: 0, next: 0)
      self.cups.append(cup)
    }
    // First cup needs to connect with the end of the ring of cups
    let firstCup = input[0]
    self.currentCup = firstCup
    self.cups[firstCup].prev = upperLimit
    self.cups[firstCup].n = firstCup
    for i in 1..<input.count {
      let prevCup = input[i - 1]
      let cup = input[i]
      self.cups[prevCup].next = cup
      self.cups[cup].prev = prevCup
      self.cups[cup].n = cup
    }
    if upperLimit == input.count {
      // for part A there are no extra cups - we loop immedatiely
      self.cups[input.last!].next = input.first!
      self.cups[input.first!].prev = input.last!
    } else {
      // Connect last cup from the input with the next cup from monotonic range
      // of remaining cups
      let switchPoint = input.count + 1
      self.cups[input.last!].next = switchPoint
      self.cups[switchPoint].prev = input.last!
      self.cups[switchPoint].n = switchPoint
      for cup in (1 + switchPoint)...upperLimit {
        let prevCup = cup - 1
        self.cups[prevCup].next = cup
        self.cups[cup].n = cup
        self.cups[cup].prev = prevCup
      }
      // Close the ring by connecting the last cup with the first from the input
      self.cups[upperLimit].next = input[0]
    }
  }

  // 3. first try moving 3 cups one by one and placing one by one;
  // 4. if moving one by one is too slow - optimize to use the least possible amount of operations
  // 5.
  mutating func move() {
    // We do not count dummy Cup 0
    let lastCup = self.cups.count - 1
    var destination = self.currentCup - 1
    if destination == 0 {
      destination = lastCup
    }
    // 1. 3 cups clockwise from current
    let c1 = self.cups[self.cups[self.currentCup].next]
    let c2 = self.cups[c1.next]
    let c3 = self.cups[c2.next]
    // Remove cups c1 to c3
    self.cups[self.currentCup].next = c3.next
    self.cups[c3.next].prev = self.currentCup
    // 2. destination cup
    while destination == c1.n || destination == c2.n || destination == c3.n {
      destination -= 1
      if destination == 0 {
        destination = lastCup
      }
    }
    // 3. put 3 removed cups right after destination cup
    let destinationCup = self.cups[destination]
    c3.next = destinationCup.next
    self.cups[c3.next].prev = c3.n
    destinationCup.next = c1.n
    c1.prev = destination
    // Remaining connections between c1, c2, and c3 should remain intact
    // 4. new current - next cup to the current one
    self.currentCup = self.cups[self.currentCup].next
  }

  mutating func play(moves: Int) {
    for i in 1...moves {
      if moves > 100 && i % (moves / 100) == 0 {
        print("[\(i / (moves / 100))%]")
      }
      self.move()
    }
  }

  func calculateScoreB() -> Int {
    let cup1 = self.cups[self.cups[1].next]
    let cup2 = self.cups[cup1.next]
    return cup1.n * cup2.n
  }

  func printCups(cups: [Int]) -> String {
    return cups.reduce("", { acc, c in acc + "\(c)" })
  }

  // List all remaining cups after cup 1
  func listCups() -> [Int] {
    var result = [Int]()
    var cup = self.cups[1]
    while cup.next != 1 {
      result.append(cup.next)
      cup = self.cups[cup.next]
    }
    return result
  }
}

func parseInput() -> [Int] {
  // FIXME importing foundation does not work on linux, so can't read input
  // from file.
  // sample
  // let file = "389125467"
  let file = "476138259"
  var input: [Int] = []
  for c in file {
    input.append(Int("\(c)")!)
  }
  return input
}

func solveA(_ game: inout Game) -> String {
  // game.play(moves: 10)
  game.play(moves: 100)
  return game.printCups(cups: game.listCups())
}

func solveB(_ game: inout Game) -> Int {
  game.play(moves: 10_000_000)
  return game.calculateScoreB()
}

var input = parseInput()

var gameA = Game(input: input, upperLimit: input.count)
// 97245386
print("Solving Day23A...")
print(solveA(&gameA))

// 156180332979
var gameB = Game(input: input, upperLimit: 1_000_000)
print("Solving Day23B...")
print(solveB(&gameB))
