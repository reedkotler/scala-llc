package scala_llc

import scala.io.Source



object Main {

  class IrToken

  case class Identifier(s: String) extends IrToken
  case class StringLiteral(s: String) extends IrToken
  case class Unparsed(s: String) extends IrToken
  case object And extends IrToken
  case object Assign extends IrToken
  case object EndOfString extends IrToken
  case object Comment extends IrToken
  case object Target extends IrToken

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

  def scanQuotedStringLiteral(s: String): (IrToken, String) = {
    val result = s.substring(1).span(x => x != '"')
    (StringLiteral(result._1), result._2.substring(1))
  }

  val reservedWordsSet = Set(
    "add", "and", "any", "asm", "ashr", "c", "comdat", "constant", "declare",
    "define", "double", "exactmatch", "externally_initialized",
    "extractelement", "extractvalue", "fadd", "false", "fdiv", "float", "fmul",
    "fp128", "fpext", "fptrunc", "fptoui", "frem" , "fsub", "half", "global",
    "label", "getelementptr", "insertelement", "insertvalue", "largest", "lshr",
    "metadata", "module", "mul", "noduplicates", "null", "opaque", "or",
    "ppc_fp128", "samesize", "sdiv", "sext", "shl", "shufflevector", "srem",
    "sub", "true" , "trunc", "type", "udiv", "undef", "urem", "void",
    "x86_fp80", "xor", "zeroinitializer", "zext")

  val reservedWordsMap = Map[String, IrToken]("and" -> And, "target" -> Target)

  val singleCharOpSymbols = List("=", ",", "(", ")", "{", "}", "!", "<", ">", "[", "]")
  val multipleCharOpSymbols = List("...")

  def scanIdOrReserved(s: String) : (IrToken, String) = {
    val (s1, s2) = scanName(s)
    if (reservedWordsMap.contains(s1))
      (reservedWordsMap(s1), s2)
    else
      (Identifier(s1), s2)
  }

  def nextToken(s: String): (IrToken, String) = {
    val s_ = s.dropWhile(isWhiteSpace)
    if (s_.length() == 0)
      return (EndOfString, "")
    s_(0) match {
      case x if isLetter(x)=> scanIdOrReserved(s_)
      case '=' => (Assign, s_.substring(1))
      case ';' => (EndOfString, "")
      case '"' => scanQuotedStringLiteral(s_)
      case '!' => (EndOfString, "")  // for now treat metadata as a comment
      case _ =>     (Unparsed(s_), "")

    }
  }


  def scanLine(s: String): List[IrToken] = {
    val s_ = s.dropWhile(isWhiteSpace)
    if (s_.length() == 0)
      List()
    else {
      val (t, s__) = nextToken(s)
      t match {
        case EndOfString => List()
        case _ => t :: scanLine(s__)
      }
    }
  }


  def unitTest(): Unit = {
    var s = scanQuotedStringLiteral("\"hello\"")
    println(s)
    var t = scanQuotedStringLiteral("\"hello\" some more")
    println(t)
    println(scanName("today"))
    println(scanName("today=tomorrow"))
    println(scanNegDecimalLit("-5"))
    println(scanNegDecimalLit("-5+10"))
    println(scanMetadataName("!mymeta"))
    var x = scanLine("a=b")
    println(x)
    x = scanLine("a = b; comment")
    println(x)
    x = scanLine("and")
    println(x)
    x = scanLine("; ModuleID = 'null.c'")
    println(x)
    x = scanLine("source_filename = \"null.c\"")
    println(x)
  }


  def main(args: Array[String]) = {

    unitTest()

    if (args.length == 1) {
      val filename = args(0)
      for (line <- Source.fromFile(filename).getLines) {
        println(scanLine(line))
      }
    }
    else {
      println("scala-llc <file-name>")
    }
  }
}