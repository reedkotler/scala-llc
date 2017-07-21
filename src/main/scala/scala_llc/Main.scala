package scala_llc

import scala.io.Source


object Main {
  def main(args: Array[String]) = {
    if (args.length  == 1) {
      val filename = args(0)
      for (line <- Source.fromFile(filename).getLines) {
        println(line)
      }
    }
    else {
      println("scala-llc <file-name>")
    }
  }
}