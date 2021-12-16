package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day10 {

  val chunkOpens = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')
  val points: Map[Char, Int] = Map[Char, Int](')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val completeScore = Map[Char, Int](')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  case class NavLine(input: String) {
    val (queue, error) = illegal(input)
  }


  def illegal(navigation: String): (List[Char], Option[Char]) = {
    def noError(navline: List[Char]): (List[Char], Option[Char]) = (navline, None)

    navigation.tail.foldLeft(noError(List(navigation.head)))((acc, c) => {
      if (chunkOpens.values.toList.contains(c)) (c :: acc._1, acc._2) else {
        if (chunkOpens(c) != acc._1.head) {
          (acc._1.tail, Some(c))
        } else (acc._1.tail, acc._2)
      }
    })
  }

  def complete(nv: NavLine): List[Char] = {
    val chunkCloses = chunkOpens.map(_.swap)
    nv.queue.map(chunkCloses(_))
  }

  def score(completion: List[Char]): BigInt = completion.foldLeft(BigInt(0))((score, ch) => {
    (score * 5) + completeScore(ch)
  })

  def part1(navSystem: List[NavLine]): Int = navSystem.map(c => if (points.contains(c.error.getOrElse(' '))) points(c
    .error.get) else 0).sum

  def part2(navSystem: List[NavLine]): BigInt = {
    val scores = navSystem.filter(_.error.isEmpty).map(l => score(complete(l))).sorted
    scores(scores.length / 2)
  }

  def main(args: Array[String]): Unit = {
    val navSystem = read("day10.txt").map(NavLine).toList

    println(s"Points: ${part1(navSystem)}")
    println(s"Middle Score: ${part2(navSystem)}")
  }
}
