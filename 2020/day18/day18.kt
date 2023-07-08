package aoc2020.day18

import java.io.File
import kotlin.text.split

enum class Operator {
    ADD, MUL
}

sealed class Token()
data class Num(val n: Long) : Token()
data class Op(val op: Operator) : Token()
class ParenLeft() : Token()
class ParenRight() : Token()

open class Calculation() {
    // NOTE: tokens are inserted at the beginning (like top of the stack)
    val nestingLevels: MutableList<MutableList<Token>> = mutableListOf(mutableListOf())

    init {}

    // Levels from 0 to n
    fun nestingLevel(): Int = nestingLevels.size - 1

    fun getTokensFromCurrentNestingLevel(): MutableList<Token> = nestingLevels[nestingLevel()]

    fun reduceExpr(op: Operator, x: Long, y: Long): Long = when (op) {
        Operator.ADD -> x + y
        Operator.MUL-> x * y
    }

    fun reduceLastOperation(tokens: MutableList<Token>) {
        if (tokens.size < 3) {
            throw Error("Cannot reduce operation when tokens size is ${tokens.size}")
        }
        val numX = tokens.removeAt(0)
        val op = tokens.removeAt(0)
        val numY = tokens.removeAt(0)
        if (op is Op && numX is Num && numY is Num) {
            val (x) = numX
            val (y) = numY
            tokens.add(0, Num(reduceExpr(op.op, x, y)))
        } else {
            throw Error("Next three tokens should be num op and num! Got: ${numX.toString()}, ${op.toString()}, ${numY.toString()}")
        }
    }

    open fun addToken(token: Token) {
        when (token) {
            is Num -> {
                val tokens = getTokensFromCurrentNestingLevel()
                tokens.add(0, token)
                if (tokens.size >= 3) {
                    reduceLastOperation(tokens)
                }
            }
            is Op -> {
                val tokens = getTokensFromCurrentNestingLevel()
                // add to the current level
                tokens.add(0, token)
            }
            is ParenLeft -> {
                // start new nested expression - next nest level
                nestingLevels.add(mutableListOf());
            }
            is ParenRight -> {
                // once we close current expression we should already
                // have a result - apply it to unfinished operator in previous
                // nesting level OR simply add if level is empty.
                // if current level is 0, do nothing
                if (nestingLevel() == 0) {
                    throw Error("We must be at least on level 1 when we hit closing paren! Level: ${nestingLevel()}")
                }
                val tokens = nestingLevels.removeAt(nestingLevel());
                // First token should be a number and the only token in the level
                when (val num = tokens.get(0)) {
                    is Num -> {
                        // Current is level below, because we removed level above
                        val tokensLevelBelow = getTokensFromCurrentNestingLevel()
                        tokensLevelBelow.add(0, num);
                        if (tokensLevelBelow.size >= 3) {
                            reduceLastOperation(tokensLevelBelow)
                        }
                    }
                    else   -> throw Error("Last token in level must be a Num! Got: ${num.toString()}")
                }
            }
       }
    }

    open fun getResult(): Long {
        // there should be only one level left and only one number inside the level 0
        if (nestingLevels.size != 1 || nestingLevels[0].size != 1) {
            throw Error("Calculation has not ended yet! ${nestingLevels.size} level(s); nestingLevels[0].size: ${nestingLevels[0].size}")
        }
        return when (val result = nestingLevels[0][0]) {
            is Num -> result.n
            else   -> throw Error("result is not Num! ${result}")
        }
    }
}

class CalculationB() : Calculation() {
    init {}

    // NOTE: it reduces all operation one by one ignoring precedence!
    fun reduceLevel(tokens: MutableList<Token>): Num {
        if (tokens.isEmpty()) return Num(0)
        while (tokens.size >= 3) {
            super.reduceLastOperation(tokens)
        }
        val result = tokens.get(0)
        if (result is Num && tokens.size == 1) {
            return result
        } else {
            throw Error("After reducing the level only Num should stay! Got: ${tokens[0].toString()}")
        }
    }

    override fun addToken(token: Token) {
        when (token) {
            is Num -> {
                val tokens = getTokensFromCurrentNestingLevel()
                when (val lastToken = tokens.getOrNull(0)) {
                    null -> tokens.add(0, token)
                    is Op -> when (lastToken.op) {
                        Operator.MUL -> tokens.add(0, token)
                        Operator.ADD -> {
                            tokens.add(0, token)
                            reduceLastOperation(tokens)
                        }
                    }
                    else -> throw Error("New Number token must be the first token or follow an operator; We got: ${lastToken.toString()}")
                }
            }
            is Op -> {
                val tokens = getTokensFromCurrentNestingLevel()
                // Just add to the current level
                tokens.add(0, token)
            }
            is ParenLeft -> {
                // simply start new nested expression - next nest level
                nestingLevels.add(mutableListOf());
            }
            is ParenRight -> {
                // once we close current expression we should evaluate all
                // remaining operations. Then, once we have a result,
                // add it to the previous level.
                // Last level will be reduced in getResult fn
                if (nestingLevel() == 0) {
                    throw Error("We must be at least on level 1 when we hit closing paren! Level: ${nestingLevel()}")
                }
                val tokens = nestingLevels.removeAt(nestingLevel())
                val num = reduceLevel(tokens)
                val tokensLevelLower =  getTokensFromCurrentNestingLevel()
                val lastToken = tokensLevelLower.getOrNull(0)
                tokensLevelLower.add(0, num)
                when (lastToken) {
                    null  -> Unit
                    is Op -> {
                        if (lastToken.op == Operator.ADD) {
                            // We always want to eagerly reduce + which
                            // is higher precedence than *
                            reduceLastOperation(tokensLevelLower)
                        }
                    }
                    else -> throw Error("Previous level should always end with an operator! Got: ${lastToken.toString()}")
                }
            }
       }
    }

    override fun getResult(): Long {
        if (nestingLevels.size != 1) {
            throw Error("Calculation has not ended yet! ${nestingLevels.size} level(s)")
        }
        val tokens = getTokensFromCurrentNestingLevel()
        if (tokens.contains(Op(Operator.ADD))) {
            throw Error("Final tokens must not contain any ADD operator!")
        }
        return reduceLevel(tokens).n
    }
}

fun readInput(fileName: String) = File(fileName)
  .readLines()

fun parseToken(token: String) =
    when (token) {
        "+" -> Op(Operator.ADD)
        "*" -> Op(Operator.MUL)
        "(" -> ParenLeft()
        ")" -> ParenRight()
        else -> Num(token.toLong())
    }

fun parseEquation(input: String) = input
 // There are no spaces between parens, so we need to add them
 .replace("(", "( ")
 .replace(")", " )")
 .split(Regex(" |\n"))
 .map { parseToken(it) }

fun solveEquation(equation: List<Token>): Long {
    val calculation = Calculation()
    equation.forEach({ calculation.addToken(it) })
    return calculation.getResult()
}

fun solveEquationB(equation: List<Token>): Long {
    val calculation = CalculationB()
    equation.forEach({ calculation.addToken(it) })
    return calculation.getResult()
}

fun repl(input: List<String>) = input
    .forEach {
        println("[repl] line: $it")
        val result = solveEquationB(parseEquation(it))
        println("$result")
    }

fun solveA(input: List<String>) = input
  .map { solveEquation(parseEquation(it)) }
  .sumOf { it }

fun solveB(input: List<String>) = input
  .map { solveEquationB(parseEquation(it)) }
  .sumOf { it }

fun main(args: Array<String>) {
    val fileName =
        if (args.find { it == "sample" } != null) "sample.txt" else "input.txt" 
    val input = readInput(fileName)
    // println(repl(input))

    // 75592527415659
    println("Solving Day18A...")
    println(solveA(input))

    // 360029542265462
    println("Solving Day18B...")
    println(solveB(input))
}
