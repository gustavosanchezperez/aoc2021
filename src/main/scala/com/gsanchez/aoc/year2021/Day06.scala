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

  // ==================== part 2
  type State2 = Map[Int, BigInt]

  def initialState2(state: String): State2 = {
    state.split(",").map(_.toInt).sorted.groupBy(n => n % 6).map(e => (e._1, e._2.size))
  }

  def tick2(state: State2): State2 = {
    val toadd: BigInt = state.getOrElse(0, 0)
    val updated = state.map(e => {
      val key = if (e._1 - 1 < 0) 6 else e._1 - 1
      val upValue: BigInt = if (e._1 == 7) e._2 + state.getOrElse(0, 0) else e._2
      (key, upValue)
    }) + (8 -> toadd)
    updated
  }

  def lanternfish2(state: State2, days: Int): State2 = {
    (1 to days).foldLeft(state)((s, _) => tick2(s))
  }

  def main(args: Array[String]): Unit = {
    val input = read("day06.txt").head
    val initState = initialState(input)
    val lanternpop = lanternfish(initState, 80).size
    println(s"LanternPop? ${lanternpop}")

    val initState2 = initialState2(input)
    val endState2 = lanternfish2(initState2, 256)
    val lanternpop2 = endState2.foldLeft(BigInt.apply(0))(_ + _._2)
    println(s"LanternPop2? ${lanternpop2}")
  }
}
