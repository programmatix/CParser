package parsing

import java.io.InputStream
import java.net.URL

import fastparse.core.{Parsed, Parser}
import pprint.PPrinter

import scala.io.Source
import scala.reflect.runtime.{universe => ru}

object TestUtils {
  def loop(lookingFor: String, in: Product): Boolean = {
    val cls = in.getClass.toString.stripPrefix("class ")
    //println(s"Looping: '${cls}' '${lookingFor}' ${in.toString}")
    if (cls == lookingFor) true
    else {
      val it = in.productIterator
      var matched = false
      while (it.hasNext) {
        val i = it.next()
        i match {
          case v: Product =>
            if (loop(lookingFor, v)) matched = true
          case _          => //println("Terminal: " + i.toString)
        }
      }
      matched
    }
  }

  def contains[LookingFor: ru.TypeTag](in: TranslationUnit): Boolean = {
    val tt = ru.typeTag[LookingFor]
    //    val symbol = tt.tpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].toString().stripPrefix("class ")
    val symbol = tt.toString.stripPrefix("TypeTag[").stripSuffix("]")

    loop(symbol, in)
  }

  private def getRec[Want](wantCls: String, in: Product): Option[Want] = {
    //    val cls = in.getClass.toString.stripPrefix("class ")
    //println(s"Looping: '${cls}' '${lookingFor}' ${in.toString}")
    val it = in.productIterator
    var out: Option[Want] = None
    while (it.hasNext) {
      val i = it.next()
      i match {
          // Not sure why this doesn't work, but it matches everything
        //          case v: Want =>
        //            out = Some(v)

          // Note ArrayBuffer won't match this, but Seq will - so it's important for Parser to toList everything
        case v: Product =>
          if (v.getClass.toString.stripPrefix("class ") == wantCls) {
            out = Some(v.asInstanceOf[Want])
          }
          else {
            val x = getRec[Want](wantCls, v)
            if (out.isEmpty) out = x
          }
        case _          => //println("Terminal: " + i.toString)
      }
    }
    out
  }

  def get[Want: ru.TypeTag](in: Product): Option[Want] = {
    val tt = ru.typeTag[Want]
//    val symbol = tt.tpe.typeSymbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].toString().stripPrefix("class ")
    val symbol = tt.toString.stripPrefix("TypeTag[").stripSuffix("]")

    getRec[Want](symbol, in)
  }

  def getAndMatchSnippet[Want: ru.TypeTag, T](raw: String, compareTo: Want, print: Boolean = false): Unit = {
    val pp = createParser()
    getAndMatch(pp.blockItemList, raw, compareTo, print)
  }

    def getAndMatch[Want: ru.TypeTag, T](parser: Parser[T, Char, String], raw: String, compareTo: Want, print: Boolean = false): Unit = {
    val parsedRaw = parser.parse(raw)
    val parsed = CParseResult.wrap(parsedRaw)
    parsed match {
      case CParseSuccess(x) =>
        if (print) {
          PPrinter.Color.log(parsed, width = 50, height = 1000)
        }

        val fetched: Option[Want] = get[Want](x.asInstanceOf[Product])
        fetched match {
          case Some(v) =>
            assert (v == compareTo, s"${v} != ${compareTo}")
          case _ => assert(false, s"Could not find instance in ${x}")
        }

        x
      //        assert (x.v.nonEmpty)
      case CParseFail(x) =>
        println(parsed)
        assert (false)
        null
    }
  }

  //  def get[Want, T <: Product](in: T): Option[Want] = {
  //    get[Want](in)
  //  }


  def createParser() = new CParser

  def check(raw: String, print: Boolean = false): TranslationUnit = {
    val p = createParser()
    val parsed = p.parse(raw)
    parsed match {
      case CParseSuccess(x) =>
        if (print) {
          PPrinter.Color.log(parsed, width = 50, height = 1000)
        }

        x
      //        assert (x.v.nonEmpty)
      case CParseFail(x) =>
        println(parsed)
        assert (false)
        null
    }
  }

  def checkSnippet(raw: String, print: Boolean = false): Seq[BlockItem] = {
    val p = createParser()
    val parsed = p.parseSnippet(raw)
    parsed match {
      case CParseSuccess(x) =>
        if (print) {
          PPrinter.Color.log(parsed, width = 50, height = 1000)
        }

        x
      //        assert (x.v.nonEmpty)
      case CParseFail(x) =>
        println(parsed)
        assert (false)
        null
    }
  }

  def good[T, Elem, Repr](p: Parsed[T, Elem, Repr], expected: T): Unit = {
    p match {
      case Parsed.Success(x, y) => assert (x == expected)
      case Parsed.Failure(x, y, z) =>
        println(p)
        println(x)
        println(y)
        println(z)
        assert (false)
    }
  }

  def bad[T, Elem, Repr](p: Parsed[T, Elem, Repr]): Unit = {
    p match {
      case Parsed.Success(x, y) => assert (false)
      case Parsed.Failure(x, y, z) =>
    }
  }

  def checkFile(url: URL, print: Boolean = false): Unit = {
    val contents = Source.fromURL(url, "UTF-8").getLines().mkString("\n")
    check(contents, print)
  }

  def checkStream(url: InputStream, print: Boolean = false): Unit = {
    val contents = Source.fromInputStream(url, "UTF-8").getLines().mkString("\n")
    check(contents, print)
  }

  def loadTestResource(fileName: String): String = {
    val url = getClass.getResource(fileName)
    val contents = Source.fromURL(url, "UTF-8").getLines().mkString("\n")
    contents
  }

}
