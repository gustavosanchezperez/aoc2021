package com.gsanchez.util

import scala.io.Source

object Files {

  def read(resource: String) = {
    Source.fromResource(resource).getLines().toSeq
  }

  def readAsInt(resource: String): Seq[Int] = {
    val input = read(resource)
    input.map(_.toInt)
  }

}
