package com.gsanchez.aoc.year2021

import com.gsanchez.util.Files.read

object Day06 {

  type State = List[Int]

  def tick(state: State): State = {
    val toadd = state.count(_ == 0)
    val updated = state.map(n => if (n - 1 < 0) 6 else n - 1) ++ List.fill(toadd)(8)
    updated
  }

  def lanternfish(state: State, days: Int): State = {
    (1 to days).foldLeft(state)((s, _) => tick(s))
  }

  def initialState(state: String): State = {
    state.split(",").map(_.toInt).toList
  }

  def main(args: Array[String]): Unit = {
    val input = read("day06.txt").head
    //    val inputex = "3,4,3,1,2"
    val initState = initialState(input)
    val lanternpop = lanternfish(initState, 80).size
    println(s"LanternPop? ${lanternpop}")
  }
}
