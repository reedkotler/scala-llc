package scala_llc

import scala.io.Source

object Main {

  class IrToken

  def doDebug = false

  case class Identifier(s: String) extends IrToken
  case class StringLiteral(s: String) extends IrToken
  case class Unparsed(s: String) extends IrToken
  case object And extends IrToken
  case object Appending extends IrToken
  case object Attributes extends IrToken
  case object AvailableExternally extends IrToken
  case object Assign extends IrToken
  case class  AttrGroupId(s: String) extends  IrToken
  case object Comment extends IrToken
  case object Common extends  IrToken
  case object DataLayout extends IrToken
  case object Define extends IrToken
  case object EndOfString extends IrToken
  case class  GlobalIdentifier(s: String) extends IrToken
  case object Internal extends  IrToken
  case object LeftCurlyBracket extends IrToken
  case object LeftSquareBracket extends IrToken
  case object LeftParen extends  IrToken
  case object Linkonce extends  IrToken
  case object LinkonceOdr extends IrToken
  case object Nounwind extends IrToken
  case object Ssp extends  IrToken
  case object Private extends IrToken
  case object Ret extends IrToken
  case object RightCurlyBracket extends IrToken
  case object RightParen extends  IrToken
  case object RightSquareBracket extends IrToken
  case object SourceFileName extends IrToken
  case object Target extends IrToken
  case object Triple extends IrToken
  case object Void extends  IrToken
  case object Uwtable extends  IrToken
  case object Weak extends IrToken
  case object WeakOdr extends IrToken
  case object None extends IrToken

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

  def scanQuotedStringLiteralRaw(s: String): (String, String) = {
    val result = s.substring(1).span(x => x != '"')
    (result._1, result._2.substring(1))
  }

  def scanQuotedStringLiteral(s: String): (IrToken, String) = {
    val result = s.substring(1).span(x => x != '"')
    (StringLiteral(result._1), result._2.substring(1))
  }

  val reservedWordsMap = Map[String, IrToken]("and" -> And,"attributes" ->Attributes,
    "datalayout" -> DataLayout, "define" -> Define,
    "nounwind" -> Nounwind,
    "ret" -> Ret, "ssp" -> Ssp,
    "source_filename" -> SourceFileName, "target" -> Target,
    "triple" -> Triple, "uwtable" -> Uwtable, "void" -> Void)

  val singleCharOp = Map[Char, IrToken] (
    '=' -> Assign, '{' -> LeftCurlyBracket, '}' -> RightCurlyBracket,
     '(' -> LeftParen, ')' -> RightParen, '[' -> LeftSquareBracket,
    ']' -> RightSquareBracket)

  def scanIdOrReserved(s: String) : (IrToken, String) = {
    val (s1, s2) = scanName(s)
    if (reservedWordsMap.contains(s1))
      (reservedWordsMap(s1), s2)
    else
      (Identifier(s1), s2)
  }

  def scanGlobalIdent(s_ : String) : (IrToken, String) = {
    val s = s_.substring(1)
    s(1) match {
      case x if isLetter(x) =>
        val (a, b) = scanName(s)
        (GlobalIdentifier(a), b)
      case x if isDecimalDigit(x) =>
        val (a,b) = scanId(s)
        (GlobalIdentifier(a), b)
      case '"' =>
        val (a,b) = scanQuotedStringLiteralRaw(s)
        (GlobalIdentifier(a),b)
    }
  }

  def scanAttrGroupId(s: String): (IrToken, String) = {
    val (a,b ) = scanDecimals(s.substring(1))
    (AttrGroupId(a), b)
  }

    def nextToken(s: String): (IrToken, String) = {
    val s_ = s.dropWhile(isWhiteSpace)
      //if (doDebug ) println(s_)
    if (s_.length() == 0)
      return (EndOfString, "")
    s_(0) match {
      case x  if isLetter(x)=> scanIdOrReserved(s_)
      case '@' => scanGlobalIdent(s_)
      case '#' => scanAttrGroupId(s_)
      case c@ ('=' | '{' | '}' | '[' | ']' | '(' | ')') =>
        (singleCharOp(c), s_.substring(1))
      case ';' => (EndOfString, "")
      case '"' => scanQuotedStringLiteral(s_)
      case '!' => (EndOfString, "")  // for now treat metadata as a comment
      case _ =>   (Unparsed(s_), "")

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

  def emitHeader = {
    println("\t.section\t__TEXT,__text,regular,pure_instructions")
    println("\t.macosx_version_min 10, 12")
  }

  def emitTail = {
    println(".subsections_via_symbols")
  }

  def processSourceFile(name: String) =
     if (doDebug) Console.println("process source file: ", name)

  def setTargetLayout(form: String) =
    if (doDebug) Console.println("target layout: ", form)

  def setTargetTriple(t: String) =
    if (doDebug) Console.println("target triple: ", t)

  def parseRet(p_ : List[IrToken]): List[IrToken] = {
    var p = p_
    p match  {
      case Void :: x =>
        p = x
    }
    p
  }


  def parseBasicBlock(p_ : List[IrToken]): List[IrToken] = {
    var p = p_
    p match {
      case Ret :: x => parseRet(x)
        p = x
      case _ =>
    }
    p
  }

  def parseBasicBlockList(p_ : List[IrToken]): List[IrToken] = {
    var p = p_
    p match {
      case Ret :: x =>
        p = parseBasicBlockList(parseBasicBlock(p))
      case _ =>
    }
    p
  }

  def parseFunctionBody(p_ : List[IrToken]): List[IrToken] = {
    var p = p_
    p match {
      case LeftCurlyBracket :: x => p = x
    }
    p = parseBasicBlockList(p)
    p
  }

  def parseDefine(p_ : List[IrToken]): List[IrToken] = {
    var p = p_
    p match {
      case Void :: x =>
        p = x
    }
    p match {
      case GlobalIdentifier(g) :: x =>
        p = x
    }
    p match {
      case LeftParen :: x =>
        p = x
    }
    p match {
      case RightParen :: x =>
        p = x
    }
    p match {
      case AttrGroupId(g) :: x =>
        p = x
    }
    p = parseFunctionBody(p)
    println("finishing parseDefine")
    p
  }

  def parseModule(p: List[IrToken]): Unit = {
    p match {
      case SourceFileName :: Assign ::  StringLiteral(x) :: y =>
        processSourceFile(x)
        parseModule(y)
      case Target :: DataLayout ::  Assign ::  StringLiteral(x) :: y =>
        setTargetLayout(x)
        parseModule(y)
      case Target :: Triple :: Assign ::  StringLiteral(x) :: y =>
        setTargetTriple(x)
        parseModule(y)
      case Define :: defineBody =>
       parseModule(parseDefine(defineBody))
      case _ => List()
    }
  }

  def main(args: Array[String]) = {
    if (args.length == 1) {
      val filename = args(0)
      val tokens = getTokens(filename)
      if (doDebug) println(tokens)
      emitHeader
      parseModule(tokens)
      emitTail
    }
    else {
      println("scala-llc <file-name>")
    }
  }
}