package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

class Board(val numbers: List[String]) {
  private var touches: Array[Array[String]] = Array.ofDim[String](5, 5)
  for {
    i <- touches.indices
    j <- touches.head.indices
  } touches(i)(j) = ""

  def touch(num: String): Unit = {
    val index = numbers.indexOf(num)
    if (index >= 0) {
      touches(index / 5)(index % 5) = num
    }

  }

  def wins: Boolean = {
    val n2 = numbers.sliding(5, 5).zip(touches).toList
    val thisWins = n2.exists(pair => pair._1 sameElements pair._2)
    thisWins
  }

  def score: Int = {
    val marked = touches.flatten.filter(n => n != "-1")
    val unmarked = numbers.filter(n => !marked.contains(n))
    println(s"Unmarked: ${unmarked.mkString(" ")}")
    unmarked.map(_.toInt).sum
  }

  override def toString: String = {
    numbers.mkString(" ")
  }
}

object Board {
  def apply(board: List[String]): Board = {
    new Board(board)
  }
}

class Bingo(pseudoRnd: List[String], cards: List[Board]) {

  private def drawNumber(number: String): Boolean = {
    println(s"num? $number")
    cards.foreach(card => card.touch(number))
    !cards.exists(_.wins)
  }

  def winner: (Board, Int) = {
    val drawnNums = pseudoRnd.takeWhile(n => drawNumber(n))
    (cards.filter(_.wins).head, pseudoRnd.splitAt(drawnNums.size)._2.head.toInt)
  }
}

object Bingo {
  def apply(lines: Seq[String]): Bingo = {
    val pseudornd = lines.head.split(",").toList
    val boardsInput = lines.tail.filter(_.nonEmpty).sliding(5, 5).toList
    val boards = boardsInput.map(board => {
      val b = board.flatMap(l => l.split(" ").filter(_.nonEmpty))
      Board(b.toList)
    })
    new Bingo(pseudornd, boards)
  }
}

object Day04 {

  def part1(bingo: Bingo): Int = {
    val (board, num) = bingo.winner
    println(num)
    board.score * num
  }

  def main(args: Array[String]): Unit = {
    val input = read("day04.txt")
    println(s"Final Score: ${part1(Bingo(input))}")
  }
}
