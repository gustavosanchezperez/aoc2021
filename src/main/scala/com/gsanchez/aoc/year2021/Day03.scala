package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day03 {

  class PowerConsumption(gamma: Int) {
    val epsilon = 0x00000FFF ^ gamma

    def power: BigInt = gamma * epsilon
  }

  class LifeSupportRating(oxygen: Int, co2: Int) {
    def rating = oxygen * co2
  }

  private def mostFreqBit(report: Seq[String], bitPos: Int): Char = {
    val numOf1 = report.map(line => line.charAt(bitPos))
    if (numOf1.count(_ == '0') > report.size / 2) '0' else '1'
  }

  private def leastFreqBit(report: Seq[String], bitPos: Int): Char = {
    val numOf1 = report.map(line => line.charAt(bitPos))
    if (numOf1.count(_ == '0') > report.size / 2) '1' else '0'
  }

  def part1(report: Seq[String]): PowerConsumption = {
    val freq = report.foldLeft(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))((f, line) => {
      line.zip(f).map(e => if (e._1 == '1') e._2 + 1 else e._2).toList
    }).map(b => if (b > report.size / 2) 1 else 0)
    val num = freq.foldLeft("")((binString, v) => if (v == 1) binString ++ "1" else binString ++ "0")
    new PowerConsumption(Integer.parseInt(num, 2))
  }

  def part2(report: Seq[String]): LifeSupportRating = {
    val numBitSize = report.head.size
    val oxygen = (0 until numBitSize).foldLeft(report)((elems, bitindex) => {
      val bit = mostFreqBit(elems, bitindex)
      if (elems.size > 1) elems.filter(line => line.charAt(bitindex) == bit) else elems
    })
    val co2 = (0 until numBitSize).foldLeft(report)((elems, bitindex) => {
      val bit = leastFreqBit(elems, bitindex)
      if (elems.size > 1) elems.filter(line => line.charAt(bitindex) == bit) else elems
    })

    new LifeSupportRating(Integer.parseInt(oxygen.head, 2), Integer.parseInt(co2.head, 2))
  }

  def main(args: Array[String]): Unit = {
    val report = read("day03.txt")
    println(s"Power Consumption:  ${part1(report).power}")
    println(s"Life Supporting Rating:  ${part2(report).rating}")
  }
}
