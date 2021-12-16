package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

import scala.annotation.tailrec

object Day09 {

  case class Location(pos: Int, height: Int) {}

  def neighbourgs(input: List[Int], pos: Int, rowSz: Int): List[Location] = {
    val up = pos - rowSz
    val down = pos + rowSz
    val left = if (pos % rowSz == 0) -1 else pos - 1
    val right = if ((pos + 1) % rowSz == 0) -1 else pos + 1
    val positions = List(up, down, left, right)
    val nbs = positions.foldLeft(List[Location]())((nb, npos) => {
      if (input.indices.contains(npos)) Location(npos, input(npos)) :: nb else nb
    })
    nbs
  }

  def lowPoint(point: Int, neighbourgs: List[Location]): Boolean = neighbourgs.map(_.height).min > point

  def basin(loc: Location, hmap: List[Int], rs: Int): List[Int] = {
    @tailrec
    def loop(nb: List[Location], bs: Set[Location]): Set[Location] = {
      if (nb.isEmpty) bs
      else {
        val newbasin = bs ++ nb
        val newnb = nb.flatMap(n => {
          neighbourgs(hmap, n.pos, rs).filter(_.height != 9).diff(newbasin.toList)
        })
        loop(newnb, newbasin)
      }
    }

    loop(List[Location](loc), Set()).toList.map(_.height)
  }

  def part1(lp: List[Location]): Int = lp.map(n => n.height + 1).sum

  def part2(lp: List[List[Int]]): Int = lp.map(_.size).sorted.takeRight(3).product

  def main(args: Array[String]): Unit = {
    val input = read("day09.txt")
    val rowSz = input.head.length
    val i2 = input.map(_.toList).flatMap(_.map(c => Integer.parseInt(c.toString))).toList

    val lp = i2.zipWithIndex.foldLeft(List[Location]())((lowPoints, point) => {
      if (lowPoint(point._1, neighbourgs(i2, point._2, rowSz))) lowPoints :+ Location(point._2, point._1) else lowPoints
    })

    println(s"risk: ${part1(lp)}")
    println(s"basin product: ${part2(lp.map(basin(_, i2, rowSz)))}")
  }
}
