package com.gsanchez.util

import scala.io.Source

object Files {

  def read(resource: String) = {
    Source.fromResource(resource).getLines().toSeq
  }

}
