package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day13 {
  case class Pos(col: Int, row: Int) {}

  case class Fold(axe: String, coord: Int) {}

  type Paper = List[Pos]

  def parsePos(p: String): Pos = {
    val cr = p.split(",").map(_.trim)
    Pos(Integer.parseInt(cr.head), Integer.parseInt(cr.last))
  }

  def parseInstruction(i: String): Fold = {
    val ins = i.split(" ")(2).split("=")
    Fold(ins.head, Integer.parseInt(ins.last))
  }

  def hfold(paper: Paper, y: Int): Paper = {
    val (up, down) = paper.partition(p => p.row < y)
    (down.map(p => Pos(p.col, y - (p.row - y))) ++ up).distinct
  }

  def vfold(paper: Paper, x: Int): Paper = {
    val (left, right) = paper.partition(p => p.col < x)
    (right.map(p => Pos(x - (p.col - x), p.row)) ++ left).distinct
  }

  def printPaper(p: Paper): Unit = {
    val pp = p.map(t => t -> '#').toMap
    println(pp)
    val rowSz = p.map(_.col).max
    val rowNum = p.map(_.row).max
    println(s"row.sz: $rowSz num.rows: $rowNum")
    for(r<-0 to rowNum){
      for(c<- 0 to rowSz){
        print(if(pp.isDefinedAt(Pos(c, r))) "#" else ".")
      }
      println
    }
//    val printed = for {
//      r <- 0 to rowNum
//      c <- 0 to rowSz
//    } yield if (pp.isDefinedAt(Pos(c, r))) '#' else '.'
//    println(printed.sliding(rowSz, rowSz).map(_.mkString).mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    val input = read("day13.txt").toList
    val (instructions, dots) = input.partition(_.contains("fold along"))

    val paper = dots.filter(_.nonEmpty).map(parsePos)
    println(vfold(paper, 655).length)


    val paperFolded = instructions.map(parseInstruction).foldLeft(paper)((p, ins) => {
      ins match {
        case Fold("x", x) => vfold(p, x)
        case Fold(_, y) => hfold(p, y)
      }
    })
    println(s"points ${paperFolded.length}")
    printPaper(paperFolded)
  }
}
