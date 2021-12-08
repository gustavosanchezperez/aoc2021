package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.readAsInt

object Day01 {

  private def depthMeasurement(lines: Seq[Int]): Int = {
    lines.sliding(2).map(pair => (pair, pair.last - pair.head)).count(_._2 > 0)
  }

  def part1(lines: Seq[Int]): Int = depthMeasurement(lines)

  def part2(lines: Seq[Int]): Int = {
    depthMeasurement(lines.sliding(3).map(_.sum).toSeq)
  }


  def main(args: Array[String]): Unit = {
    val input = readAsInt("day01.part01.txt")
    println(s"Counter Increased> ${part1(input)}")
    println(s"Counter Increased> ${part2(input)}")
  }
}
