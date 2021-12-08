package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.{read, readAsInt}

object Day01 {

  def part1(resource: String): Unit = {
    val lines = readAsInt(resource)

    val largerThanPrev = lines.sliding(2).map(pair => (pair, pair.last - pair.head)).count(_._2 > 0)
    println(s"Counter Increased> $largerThanPrev")
  }

  def part2(resource: String): Unit = {
    val lines = readAsInt(resource)

    val largerThanPrev = lines.sliding(3).map(_.sum).sliding(2).map(pair => (pair, pair.last - pair.head)).count(_._2 > 0)
    println(s"Counter Increased> $largerThanPrev")
  }


  def main(args: Array[String]): Unit = {
    part1("day01.part01.txt")
    part2("day01.part01.txt")
  }
}
