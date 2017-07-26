package scala_llc

import scala.io.Source

class Token


object Main {

  // Grammar based on https://github.com/llir/grammar

  // tbd: needs more scala clean up to make it look nicer

  def isAsciiLetter(c: Char) = ('A' <= c && c <= 'Z') || ('a' <= c && c <= 'z')

  def isLetter(c: Char) =
    c match {
      case x if isAsciiLetter(c) => true
      case '$' | '-' | '.' | '_' => true
      case _ => false
    }


  def isEscapeLetter(c: Char) =
    c match {
      case x if isLetter(c) => true
      case '\\' => true
      case _ => false
    }


  def isDecimalDigit(c: Char) = ('0' <= c && c <= '9')


  def isHexDigit(c: Char) = c match {
    case x if isDecimalDigit(x) => true
    case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' |
         'a' | 'b' | 'c' | 'd' | 'e' | 'f' => true
  }

  def isWhiteSpace(c: Char) =
    c match {
      case ' ' | '\t' => true
      case default => false
    }

  // scan routines assume they are correct syntax, especially regarding to start character
  // routines are passed a string with their token at the start. they are supposed to split
  // the line into their (token, rest-of-line)

  def scanComment(s: String) = (s, "")

  def scanWhiteSpace(s: String) = s.span(x => isWhiteSpace(x))

  def scanName(s: String) = s.span(x => isLetter(x) || isDecimalDigit(x))

  def scanEscapeName(s: String) = s.span(x => isEscapeLetter(x) || isDecimalDigit(x))

  def scanQuotedName(s: String) = scanStringLiteral(s)

  def scanId(s: String) = scanDecimals(s)

  def scanMetadataName(s: String) = scanEscapeName(s.substring(1))

  def scanMetaDataId(s: String) = scanId(s.substring(1))

  def scanStringLiteral(s: String) = scanQuotedStringLiteral(s)

  def scanDecimals(s: String) = s.span(x => isDecimalDigit(x))

  def scanNegDecimalLit(s: String): (String, String) = {
    val result = s.substring(1).span(x => isDecimalDigit(x))
    ('-' + result._1, result._2)
  }

  def scanQuotedStringLiteral(s: String): (String, String) = {
    val result = s.substring(1).span(x => x != '"')
    (result._1, result._2.substring(1))
  }

  val reservedWords = List(
    "add", "and", "any", "asm", "ashr", "c", "comdat", "constant", "declare",
    "define", "double", "exactmatch", "externally_initialized",
    "extractelement", "extractvalue", "fadd", "false", "fdiv", "float", "fmul",
    "fp128", "fpext", "fptrunc", "fptoui", "frem" , "fsub", "half", "global",
    "label", "getelementptr", "insertelement", "insertvalue", "largest", "lshr",
    "metadata", "module", "mul", "noduplicates", "null", "opaque", "or",
    "ppc_fp128", "samesize", "sdiv", "sext", "shl", "shufflevector", "srem",
    "sub", "true" , "trunc", "type", "udiv", "undef", "urem", "void",
    "x86_fp80", "xor", "zeroinitializer", "zext")

  val singleCharOpSymbols = List("=", ",", "(", ")", "{", "}", "!", "<", ">", "[", "]")
  val multipleCharOpSymbols = List("...")


  def scanLine(s: String): List[Token] = {
    var s_ = s.dropWhile(isWhiteSpace)
    println(s_)
    var result = Nil
    result
  }


  def unitTest(): Unit = {
    var s = scanQuotedStringLiteral("\"hello\"")
    println(s)
    s = scanQuotedStringLiteral("\"hello\" some more")
    println(s)
    s = scanName("today")
    println(s)
    s = scanName("today=tomorrow")
    println(s)
    s = scanNegDecimalLit("-5")
    println(s)
    s = scanNegDecimalLit("-5+10")
    println(s)
    s = scanMetadataName("!mymeta")
    println(s)
  }


  def main(args: Array[String]) = {

    unitTest()

    if (args.length == 1) {
      val filename = args(0)
      for (line <- Source.fromFile(filename).getLines) {
        println(line)
        scanLine(line)
      }
    }
    else {
      println("scala-llc <file-name>")
    }
  }
}