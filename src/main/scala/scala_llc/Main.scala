package scala_llc

import scala.io.Source

object Main {

  class IrToken

  case class Identifier(s: String) extends IrToken
  case class StringLiteral(s: String) extends IrToken
  case class Unparsed(s: String) extends IrToken
  case object And extends IrToken
  case object Assign extends IrToken
  case object DataLayout extends IrToken
  case object EndOfString extends IrToken
  case object Comment extends IrToken
  case object SourceFileName extends IrToken
  case object Target extends IrToken
  case object Triple extends IrToken

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

  val reservedWordsMap = Map[String, IrToken]("and" -> And,
    "datalayout" -> DataLayout, "source_filename" -> SourceFileName, "target" -> Target,
    "triple" -> Triple)

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

  // TBD: need to choose how we will concatenate these lists efficiently
  // this way here is very slow because it will need to read the whole
  // first list to find the end. There are lots of options here in scala for
  // how to handle this issue.
  //
  def getTokens(filename: String): List[IrToken] = {
    var tokens = List[IrToken]()
    for (line <- Source.fromFile(filename).getLines) {
      val lineTokens = scanLine(line)
      if (lineTokens.length != 0)
        tokens = tokens ::: lineTokens
    }
    tokens
  }

  def processSourceFile(name: String) =
     println("process source file: ", name)

  def setTargetLayout(form: String) =
    println("target layout: ", form)

  def setTargetTriple(t: String) =
    println("target triple: ", t)

  def parseModule(p: List[IrToken]): Unit = {
    p match {
      case List(SourceFileName, Assign, StringLiteral(x), _*)  =>
        processSourceFile(x); parseModule(p.drop(3))
      case List(Target, DataLayout, Assign, StringLiteral(x), _*  )=>
        setTargetLayout(x); parseModule(p.drop(4))
      case List(Target, Triple, Assign, StringLiteral(x), _*) =>
        setTargetTriple(x); parseModule(p.drop(4))
      case _ =>
    }
  }

  def main(args: Array[String]) = {
    if (args.length == 1) {
      val filename = args(0)
      val tokens = getTokens(filename)
      // print(tokens)
      parseModule(tokens)
    }
    else {
      println("scala-llc <file-name>")
    }
  }
}