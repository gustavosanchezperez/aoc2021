package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.{read, readAsInt}

object Day07 {

  def fuelCheap(positions: List[Int], aligned: Int): Int = {
    positions.foldLeft(0)((fuel, pos) => fuel + Math.abs(aligned - pos))
  }

  def fuelExpensive(positions: List[Int], aligned: Int): Int = {
    positions.foldLeft(0)((fuel, pos) => {
      val x = Math.abs(aligned - pos)
      val fuelAtThisPos = (x * (x + 1)) / 2
      fuel + fuelAtThisPos
    })
  }

  def part1(hpos: List[Int], fuel: (List[Int], Int) => Int): (Int, Int) = {
    var deltax = (hpos.last - hpos.head) / 4
    var x_current = hpos.sum / hpos.size
    var dir = if (hpos.partition(_ == x_current)._1.size > hpos.size / 2) 1 else -1
    var fuel_current = fuel(hpos, x_current)
    var fuel_min = fuel_current
    var x_min = x_current

    println(s"xs: ${x_current} -> fuels?${fuel_current}")
    while (deltax > 0) {
      x_current = x_current + dir * deltax
      val fuel_next = fuel(hpos, x_current)
      println(s"x: ${x_current} -> fuel?${fuel_next} delta(${dir * deltax})")
      if (fuel_next < fuel_min) {
        x_min = x_current
        fuel_min = fuel_next
      }
      if (fuel_next > fuel_current) {
        deltax = Math.abs(x_current - x_min) / 2
        dir = if (((x_min - x_current.toFloat) / (fuel_min - fuel_next.toFloat)) > 0) -1 else 1
      }
      fuel_current = fuel_next
    }
    (x_min, fuel_min)
  }

  def main(args: Array[String]): Unit = {
        val input = read("day07.txt").head
//    val input = "16,1,2,0,4,2,7,1,2,14"
    val horPos = input.split(",").map(_.toInt).sorted.toList
    println(horPos)
    val hpos = part1(horPos, fuelCheap)
    println(s"Align position ${hpos}")
    println(s"i:${fuelCheap(horPos, hpos._1 - 1)} s:${fuelCheap(horPos, hpos._1)} d:${
      fuelCheap(horPos, hpos._1
        + 1)
    }")

    println(s"i:${fuelExpensive(horPos, 2)}")
    val hpos2 = part1(horPos, fuelExpensive)
    println(s"Align position ${hpos2}")
    println(s"i:${fuelExpensive(horPos, hpos2._1 - 1)} s:${fuelExpensive(horPos, hpos2._1)} d:${fuelExpensive(horPos, hpos2._1 + 1)}")
  }
}