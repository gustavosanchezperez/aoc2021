package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.{read, readAsInt}

object Day07 {

  def fuel(positions: List[Int], aligned: Int): Int = {
    positions.foldLeft(0)((fuel, pos) => fuel + Math.abs(aligned - pos))
  }

  def part1(hpos: List[Int]): (Int, Int) = {
    var deltax = (hpos.last - hpos.head) / 4
    var x_start = hpos.sum / hpos.size
    var dir = if (hpos.partition(_ == x_start)._1.size > hpos.size / 2) 1 else -1
    var fuel_start = fuel(hpos, x_start)
    var fuel_min = fuel_start
    var x_min = x_start

    println(s"xs: ${x_start} -> fuels?${fuel_start}")
    while (deltax > 0) {
      x_start = x_start + dir * deltax
      val fuel_next = fuel(hpos, x_start)
      println(s"x: ${x_start} -> fuel?${fuel_next} delta(${dir * deltax})")
      if (fuel_next < fuel_min) {
        fuel_min = fuel_next
        x_min = x_start
      }
      else {
        deltax = Math.abs(x_start - x_min) / 2
        dir = if (((x_min - x_start.toFloat) / (fuel_min - fuel_next.toFloat)) > 0) -1 else 1
      }
    }
    (x_min, fuel_min)
  }

  def main(args: Array[String]): Unit = {
    val input = read("day07.txt").head
//        val input = "16,1,2,0,4,2,7,1,2,14"
    val horPos = input.split(",").map(_.toInt).sorted.toList
    println(horPos)
    val hpos = part1(horPos)
    println(s"Align position ${hpos}")
    println(s"i:${fuel(horPos, hpos._1-1)} s:${fuel(horPos, hpos._1)} d:${fuel(horPos, hpos._1+1)}")
  }
}