package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day01 {

  def part1(resource: String): Unit = {
    val lines = read(resource)

    val largerThanPrev = lines.sliding(2).map(pair => (pair, pair.last.toInt - pair.head.toInt)).count(_._2 > 0)
    println(s"Counter Increased> $largerThanPrev")
  }

  def main(args: Array[String]): Unit = {
    part1("day01.part01.txt")
  }
}
