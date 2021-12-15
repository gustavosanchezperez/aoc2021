package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day09 {

  def neighbourgs(input: List[Int], pos: Int, rowSz: Int): List[Int] = {
    val up = pos - rowSz
    val down = pos + rowSz
    val left = if (pos % rowSz == 0) -1 else pos - 1
    val right = if ((pos + 1) % rowSz == 0) -1 else pos + 1
    val positions = List(up, down, left, right)
    val nbs = positions.foldLeft(List[Int]())((nb, npos) => {
      if (input.indices.contains(npos)) input(npos) :: nb else nb
    })
    nbs
  }

  def lowPoint(point: Int, neighbourgs: List[Int]): Boolean = neighbourgs.min > point

  def main(args: Array[String]): Unit = {
    val input = read("day09.txt")
    val rowSz = input.head.size
    val i2 = input.map(_.toList).flatMap(_.map(c => Integer.parseInt(c.toString))).toList

    val lp = i2.zip(i2.indices).foldLeft(List[Int]())((lowPoints, point) => {
      if (lowPoint(point._1, neighbourgs(i2, point._2, rowSz))) lowPoints :+ point._1 else lowPoints
    })

    val riskLevel = lp.map(n => n + 1)

    println(s"risk: ${riskLevel.sum}")
  }
}
