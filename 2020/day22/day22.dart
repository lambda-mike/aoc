import 'dart:collection';
import 'dart:io';

enum Player {
  one,
  two,
}

class Game {
  final Queue<int> player1;
  final Queue<int> player2;
  const Game(this.player1, this.player2);

  static Game copy(Game other) {
    return Game(Queue.from(other.player1), Queue.from(other.player2));
  }

  int calculateScore(Player winner) {
    var winningDeck = winner == Player.one ? player1 : player2;
    return winningDeck
        .toList()
        .reversed
        .indexed
        .map((x) => (x.$1 + 1) * x.$2)
        .fold(0, (int x, int y) => x + y);
  }

  Player play() {
    while (player1.isNotEmpty && player2.isNotEmpty) {
      var p1 = player1.removeFirst();
      var p2 = player2.removeFirst();
      if (p1 > p2) {
        player1.addLast(p1);
        player1.addLast(p2);
      } else {
        player2.addLast(p2);
        player2.addLast(p1);
      }
    }
    return player1.isNotEmpty ? Player.one : Player.two;
  }
}

Game parseInput(String fName) {
  final lines = File(fName).readAsLinesSync();
  final player1 = Queue<int>();
  final player2 = Queue<int>();
  var player = player1;
  for (final line in lines) {
    // Ignore lines with Player card deck header
    if (line.startsWith("P")) {
      continue;
    }
    if (line.trim().isEmpty) {
      player = player2;
      continue;
    }
    final card = int.parse(line);
    player.addLast(card);
  }
  return Game(player1, player2);
}

int solveA(Game game) {
  final winner = game.play();
  return game.calculateScore(winner);
}

class GameRec extends Game {
  final Set<String> _historicalDecks = {};

  GameRec(super.player1, super.player2);

  static GameRec subgame(
      int card1, Queue<int> player1, int card2, Queue<int> player2) {
    final subP1 = player1.take(card1);
    final subP2 = player2.take(card2);
    return GameRec(Queue.from(subP1), Queue.from(subP2));
  }

  String hashDecks() {
    final p1 = player1.join(",");
    final p2 = player2.join(",");
    return "P1:${p1}P2:${p2}";
  }

  Player play() {
    while (player1.isNotEmpty && player2.isNotEmpty) {
      final hash = hashDecks();
      if (_historicalDecks.contains(hash)) {
        return Player.one;
      }
      _historicalDecks.add(hash);
      final p1 = player1.removeFirst();
      final p2 = player2.removeFirst();
      // We need to play subgame to decide who wins
      if (player1.length >= p1 && player2.length >= p2) {
        final winner = GameRec.subgame(p1, player1, p2, player2).play();
        if (winner == Player.one) {
          player1.addLast(p1);
          player1.addLast(p2);
        } else {
          player2.addLast(p2);
          player2.addLast(p1);
        }
      } else {
        if (p1 > p2) {
          player1.addLast(p1);
          player1.addLast(p2);
        } else {
          player2.addLast(p2);
          player2.addLast(p1);
        }
      }
    }
    return player1.isNotEmpty ? Player.one : Player.two;
  }
}

int solveB(GameRec game) {
  final winner = game.play();
  return game.calculateScore(winner);
}

void main() {
  // const fName = "sample.txt";
  // const fName = "sample_rec.txt";
  const fName = "input.txt";
  final game = parseInput(fName);

  // 32677
  print("Solving Day22A...");
  final gameA = Game.copy(game);
  print(solveA(gameA));

  // 33661
  print("Solving Day22B...");
  final gameB = Game.copy(game);
  print(solveB(GameRec(gameB.player1, gameB.player2)));
}
