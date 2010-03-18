package sjson.json

import java.text.SimpleDateFormat
import java.util.TimeZone

object Util {
  def quote(s: String): String = s match {
    case null => "null"
    case _ => {
      new StringBuilder(s.length + 2)
        .append('"')
        .append(s.foldLeft(new StringBuilder(""))((a, b) => a.append(escape(b, '"'))).toString)
        .append('"')
        .toString
    }
  }
  
  private def escape(c: Char, quoteChar: Char): String = c match {
    case '"' if (c == quoteChar) => "\\" + c
    case '"' => "" + c
    case '\'' if (c == quoteChar) => "\\" + c
    case '\'' => "" + c
    case '/' => "\\/"
    case '\\' => "\\\\"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case x if (x < 32 || x > 126) => {
      val hex = Integer.toHexString(x)
      val len = hex.length
      "\\u" + (if (len < 4) "0000".substring(len) + hex else hex)
    }
    case _ => "" + c
  }
  
  import java.io._
  def readTillNl(in: InputStreamReader): String = {
    var c = -1
    var str = new StringBuffer
    do {
      c = in.read
      if (c != '\n' && c != -1) {
        str.append(c.toChar)
      }
    } 
    while (c != '\n' && c != -1)
    str.toString
  }

  def mkNum(v: BigDecimal, c: Class[_]): Any = {
    if (c.isAssignableFrom(classOf[Int])) v.asInstanceOf[BigDecimal].intValue
    else if (c.isAssignableFrom(classOf[Long])) v.asInstanceOf[BigDecimal].longValue
    else if (c.isAssignableFrom(classOf[Float])) v.asInstanceOf[BigDecimal].floatValue
    else if (c.isAssignableFrom(classOf[Double])) v.asInstanceOf[BigDecimal].doubleValue
    else if (c.isAssignableFrom(classOf[Short])) v.asInstanceOf[BigDecimal].shortValue
    else v
  }

  /**
   * Tries to read date as millis since epoch, and if this fails
   * will attempt to read as an ISO8660 date.
   */
  import java.util.Date
  def mkDate(v: String): Date = {
    try {
      new Date(v.toLong.longValue)
    } catch {
      case e: NumberFormatException =>
        val corrected = v.substring(0, v.length() - 3) + v.substring(v.length() - 2)
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").parse(corrected)
    }
  }

  /**
   * Formats the date as an ISO8660 string, which supports
   * lexicographic sorting.
   */
  def outputDate(v: Date): String = {
    val result = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSZ").format(v)
    result.substring(0, result.length() - 2) + ":" + result.substring(result.length() - 2)
  }
}

