package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

import scala.annotation.tailrec

object Day11 {

  case class Pos(col: Int, row: Int) {
    def +(op: Pos): Pos = {
      Pos(this.col + op.col, this.row + op.row)
    }
  }

  case class Octopus(pos: Pos, var egr: Int) {
    def apply(): Int = pos.col + pos.row * 10

    override def equals(oct: Any): Boolean = oct match {
      case o: Octopus => this.pos == o.pos
      case _ => false
    }
  }

  type OctopusCavern = Array[Octopus]

  def printc(cavern: OctopusCavern): Unit = {
    cavern.sliding(10, 10).foreach(r => println(r.map(_.egr).mkString("")))
    println("----------------")
  }

  def increaseEngergy(cavern: OctopusCavern, inc: Int): Unit = {
    cavern.foreach(_.egr += inc)
  }

  def adjacent(oct: Octopus, oc: OctopusCavern): List[Octopus] = {
    val nb = for {
      c <- -1 to 1
      r <- -1 to 1
      if c != 0 || r != 0
    } yield Pos(c, r)
    nb.foldLeft(List[Pos]())((adj, pos) => {
      pos + oct.pos :: adj
    }).filter(pos => pos.row >= 0 && pos.col >= 0 && pos.row < 10 && pos.col < 10).map(pos => oc(pos.col + pos.row
      * 10))
  }

  def flashes(oc: OctopusCavern): List[Octopus] = {
    @tailrec
    def loop(toflash: List[Octopus], alreadyFlashed: List[Octopus]): List[Octopus] = {
      //      println(s"loop| tf: ${toflash} .. af: ${alreadyFlashed}")
      if (toflash.nonEmpty) {
        toflash.foreach(oct => {
          adjacent(oct, oc).foreach(oct => {
            oc(oct()).egr += 1
          })
        })
        val flashed = toflash.flatMap(adjacent(_, oc)).toSet
        val alfl = toflash ++ alreadyFlashed
        //        println(s"FLASHED: ${flashed.filter(_.egr > 9).filter(oc => !alfl.contains(oc))}")
        //        flashed.foreach(oct => oc(oct()).egr += 1)
        loop(flashed.filter(_.egr > 9).filter(oc => !alfl.contains(oc)).toList, alfl)
      }
      else alreadyFlashed
    }

    val toFlash = oc.filter(_.egr > 9)
    //    toFlash.foreach(println(_))
    loop(toFlash.toList, List())
  }

  def tick(oc: OctopusCavern): Int = {
    increaseEngergy(oc, 1)
    val octFlashed = flashes(oc)
    octFlashed.foreach(oct => oc(oct()).egr = 0)
    octFlashed.length
  }

  def part1(oc: OctopusCavern): Int = {
    (1 to 100).foldLeft(0)((flash, _) => {
      flash + tick(oc)
    })
  }

  def part2(oc: OctopusCavern): Int = LazyList.from(1).takeWhile(_ => tick(oc) != oc.length).last + 1

  def main(args: Array[String]): Unit = {
    val energy = read("day11.txt")
    val octopus = energy.map(_.toList).flatMap(_.map(c => Integer.parseInt(c.toString))).zipWithIndex.map(oct => {
      Octopus(Pos(oct._2 % 10, oct._2 / 10), oct._1)
    }).toArray

    val oct2 = octopus.clone()

    println(s"Total flashes:  ${part1(octopus)}")
    println(s"Step all flash:  ${part2(oct2)}")
  }
}
