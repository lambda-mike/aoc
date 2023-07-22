import re
import sugar
import std/sequtils
import std/strformat
import std/tables
import strutils

type
  ParseException = object of ValueError
  RuleKind = enum
    rList,
    rChoice,
    rLetter
  Rule = ref object
    case kind: RuleKind
    of rList: list: seq[int]
    of rChoice: left, right: seq[int]
    of rLetter: letter: char
  RuleIndexed = tuple[i: int, rule: Rule]
  Input = tuple[messages: seq[string], rules: Table[int, Rule]]
  Validation = tuple[valid: bool, remaining: string]

proc dumpRule(rule: Rule) =
  case rule.kind
    of rLetter: echo fmt"{rule.kind = } {rule.letter = }"
    of rList:
      let xs = rule.list.map(x => $x).join(", ")
      echo fmt"{rule.kind = } {xs = }"
    of rChoice:
      let
        left = rule.left.map(x => $x).join(", ")
        right = rule.right.map(x => $x).join(", ")
      echo fmt"{rule.kind = } {left = } {right = }"

proc parseLine(line: string): RuleIndexed =
  if line =~ re"(\d+): (.+)":
    let
      i = matches[0].parseInt()
      text = matches[1]
    var rule: Rule
    if text.contains("\""): rule = Rule(kind: rLetter, letter: text[1])
    elif text =~ re"(\d+) (\d+) \| (\d+) (\d+)":
      rule = Rule(
        kind: rChoice,
        left: @[matches[0].parseInt, matches[1].parseInt],
        right: @[matches[2].parseInt, matches[3].parseInt]
      )
    elif text =~ re"(\d+) \| (\d+)":
      rule = Rule(
        kind: rChoice,
        left: @[matches[0].parseInt],
        right: @[matches[1].parseInt]
      )
    else: rule = Rule(kind: rList, list: text.split(" ").map(parseInt))
    result = (i: i, rule: rule)
  else:
    raise newException(ParseException, "Could not parse rule! '" & line & "'")

proc parseInput(input: string): Input =
  var
    messages = newSeq[string](0)
    # set to true once all rules are parsed and we keep parsing
    rulesExhausted = false
    rules = initTable[int, Rule]()
  for line in input.splitLines():
    if line.strip() == "":
      rulesExhausted = true
    elif rulesExhausted:
      messages.add(line)
    else:
      let (i, rule) = parseLine(line)
      rules[i] = rule
  return (messages: messages, rules: rules)

proc traverseListValidateRules(rules: Table[int, Rule], list: seq[int], msgRemaining: string, traverseValidateRules: (Table[int, Rule], int, string) -> Validation): Validation =
  var
    valid = true
    remaining = msgRemaining
  for r in list:
    if not valid: break
    else:
      (valid, remaining) = traverseValidateRules(rules, r, remaining)
  result = (valid: valid, remaining: remaining)

proc traverseValidateRules(rules: Table[int, Rule], i: int, msgRemaining: string): Validation =
  let rule = rules[i]
  # dumpRule(rule)
  case rule.kind
    of rLetter:
      result =
        if msgRemaining.len == 0:
          (valid: false, remaining: msgRemaining)
        elif rule.letter == msgRemaining[0]:
          (valid: true, remaining: msgRemaining[1..^1])
        else:
          (valid: false, remaining: msgRemaining[0..^1])
    of rChoice:
        var
          valid = false
          rest = msgRemaining
        (valid, rest) = traverseListValidateRules(rules, rule.left, msgRemaining, traverseValidateRules)
        if not valid:
          (valid, rest) = traverseListValidateRules(rules, rule.right, msgRemaining, traverseValidateRules)
        result = (valid: valid, remaining: rest)
    of rList:
      result =
        traverseListValidateRules(rules, rule.list, msgRemaining, traverseValidateRules)

proc validateMsg(rules: Table[int, Rule], msg: string): bool =
  let (valid, rest) = traverseValidateRules(rules, 0, msg)
  result = valid and rest == ""

proc solveA(input: Input): int =
  let
    (messages, rules) = input
  for msg in messages:
    if validateMsg(rules, msg):
      result += 1

# Rule 0 has a definition of: 0: 8 11;
# Rule 8 has a definition of: u: 42 | 42 8
proc getNumberOfValidRepetitionsForRule42(msg: string, rules: Table[int, Rule]): int =
  var
    valid = true
    rest = msg
    n = 0
  while valid:
    (valid, rest) = traverseValidateRules(rules, 42, rest)
    if valid: n += 1
  result = n

# iterate over rule 8 variants from 1 to N as long as the prefix matches the
# beginning of the msg; 
# calculateN = keep calling validate on original msg starting with rule 42 (definition of rule 8)
# once we have N keep checking the original msg starting from rule 0, but with rule 8 modified to be at most N repetitions of rule 42 (using rList kind) - once you validate whole msg at any point, msg is considered to be valid, if not, message is invalid
proc solveB(input: Input): int =
  let (messages, rules) = input
  var newRules = rules
  # 8: 42 | 42 8
  # 11: 42 31 | 42 11 31
  newRules[8] = Rule(kind: rChoice, left: @[42], right: @[42, 8])
  newRules[11] = Rule(kind: rChoice, left: @[42, 31], right: @[42, 11, 31])
  for msg in messages:
    let n = getNumberOfValidRepetitionsForRule42(msg, newRules)
    # Because rule 0 = 8 11, 8 = 42 | 42 8, 11 = 42 31 | 42 11 31,
    # rule 42 must be valid at least two times in a row for the message to be valid
    if n <= 1: continue
    # Try with rule 8 repeated from 1 to N excluded (because we need at least one 42 for rule 11)
    for i in 1..<n:
      newRules[8] = Rule(kind: rList, list: repeat(42, i))
      if validateMsg(newRules, msg):
        result += 1

proc main() =
  let
    fname = "input.txt"
    input = readFile(fname).strip()
    data = parseInput(input)
  echo "Sovling Day19A..."
  # 151
  echo solveA(data)
  # 386
  echo "Sovling Day19B..."
  echo solveB(data)

main()
