package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.readAsPair

object Day02 {
  class Position(val hor: Int, val depth: Int, val aim: Int) {
    def this(hor: Int, depth: Int) {
      this(hor, depth, 0)
    }

    def value: Int = hor * depth
  }

  def part1(course: Seq[(String, Int)], initial: Position): Position = {
    course.foldLeft(initial)((p: Position, m: (String, Int)) => {
      val position = m match {
        case ("forward", f) => new Position(p.hor + f, p.depth)
        case ("down", d) => new Position(p.hor, p.depth + d)
        case ("up", up) => new Position(p.hor, p.depth - up)
      }
      position
    })
  }

  def part2(course: Seq[(String, Int)], initial: Position): Position = {
    course.foldLeft(initial)((p: Position, m: (String, Int)) => {
      val position = m match {
        case ("forward", forward) => new Position(p.hor + forward, p.depth + (p.aim * forward), p.aim)
        case ("down", down) => new Position(p.hor, p.depth, p.aim + down)
        case ("up", up) => new Position(p.hor, p.depth, p.aim - up)
      }
      position
    })
  }

  def main(args: Array[String]): Unit = {
    val course = readAsPair("day02.txt")
    val finalPos = part1(course, new Position(0, 0))
    println(s"Final Position: ${finalPos} Result: ${finalPos.value}")
    val finalPos2 = part2(course, new Position(0, 0, 0))
    println(s"Final Position2: ${finalPos2} Result: ${finalPos2.value}")
  }
}
