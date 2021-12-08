package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day03 {

  class PowerConsumption(gamma: Int) {
    val epsilon = 0x00000FFF^gamma

    def power: BigInt = gamma * epsilon
  }

  def part1(report: Seq[String]): PowerConsumption = {
    val freq = report.foldLeft(List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))((f, line) => {
      line.zip(f).map(e => if (e._1 == '1') e._2 + 1 else e._2).toList
    }).map(b => if (b > report.size / 2) 1 else 0)
    val num = freq.foldLeft("")((binString, v) => if (v == 1) binString ++ "1" else binString ++ "0")
    new PowerConsumption(Integer.parseInt(num, 2))
  }

  def main(args: Array[String]): Unit = {
    val report = read("day03.txt")
    println(s"Power Consumption:  ${part1(report).power}")
  }
}
