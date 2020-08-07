package org.thp.elastictool

import java.io.{InputStream, InputStreamReader}

import scala.util.parsing.combinator._
import java.util.UUID

import org.thp.elastictool

class Index(
    val health: String,
    val status: String,
    val name: String,
    val uuid: String,
    val rep: Long,
    val docsCount: Long,
    val storeSize: String
) {
  override def toString: String = s"Index $name health=$health status=$status rep=$rep docsCount=$docsCount storeSize=$storeSize"
}

object Index extends RegexParsers {
  override def skipWhitespace: Boolean = true

//  def word: Parser[String] = """\p{Alnum}+""".r ^^ { _.toString }
//  def number: Parser[Long] = """\p{Digit}+""".r ^^ { _.toLong }
  def word: Parser[String] = """\S+""".r ^^ { _.toString }
  def number: Parser[Long] = """\d+""".r ^^ { _.toLong }

  val parser: Parser[Index] = word ~ word ~ word ~ word ~ number ~ number ~ word ^^ {
    case health ~ status ~ index ~ uuid ~ rep ~ docsCount ~ storeSize ⇒ new Index(health, status, index, uuid, rep, docsCount, storeSize)
  }

  def parseLine(line: String): Option[Index] = parseAll(parser, line) match {
    case Success(index, _) ⇒ Some(index)
    case failure ⇒
      println(s"WARNING, Index information cannot be parsed: $failure")
      None
  }

  def parseStream(inputStream: InputStream): List[Index] =
    (parseAll(parser.+, new InputStreamReader(inputStream)) match {
      case Success(indexes, _) ⇒
        indexes.foreach(i ⇒ println(s"Found index:$i"))
        Some(indexes)
      case failure ⇒
        println(s"WARNING, Index information cannot be parsed: $failure")
        None
    }).getOrElse(Nil)
}
