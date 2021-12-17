package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day13 {
  case class Pos(col: Int, row: Int) {}

  type Paper = List[Pos]

  def parsePos(p: String): Pos = {
    val cr = p.split(",").map(_.trim)
    Pos(Integer.parseInt(cr.head), Integer.parseInt(cr.last))
  }

  def hfold(paper: Paper, y: Int): Paper = {
    val (up, down) = paper.partition(p => p.row < y)
    (down.map(p => Pos(p.col, y - (p.row - y))) ++ up).distinct
  }

  def vfold(paper: Paper, x: Int): Paper = {
    val (left, right) = paper.partition(p => p.col < x)
    (right.map(p => Pos(x - (p.col - x), p.row)) ++ left).distinct
  }

  def main(args: Array[String]): Unit = {
    val input = read("day13.txt").toList
    val (instructions, dots) = input.partition(_.contains("fold along"))

    val paper = dots.filter(_.nonEmpty).map(parsePos)
    println(vfold(paper, 655).length)
  }
}
