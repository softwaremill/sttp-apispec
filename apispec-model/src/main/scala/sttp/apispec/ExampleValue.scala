package sttp.apispec

/** Used to represent arbitrary enum, constant and example values in schemas. */
case class ExampleValue private (json: String)
object ExampleValue {
  private def apply(json: String): ExampleValue = new ExampleValue(json)
  private def copy(json: String): ExampleValue = new ExampleValue(json)

  final val Null = rawJson("null")

  /** @param json must be a valid JSON string */
  def rawJson(json: String): ExampleValue = ExampleValue(json)

  def `null`: ExampleValue = Null
  def boolean(b: Boolean): ExampleValue = rawJson(b.toString)
  def string(s: String): ExampleValue = rawJson(encodeJsonString(s))
  def number(n: BigDecimal): ExampleValue = rawJson(n.toString)
  def number(n: Double): ExampleValue = rawJson(n.toString)
  def number(n: Float): ExampleValue = rawJson(n.toString)
  def number(n: BigInt): ExampleValue = rawJson(n.toString)
  def number(n: Long): ExampleValue = rawJson(n.toString)
  def number(n: Int): ExampleValue = rawJson(n.toString)
  def number(n: Short): ExampleValue = rawJson(n.toString)
  def number(n: Byte): ExampleValue = rawJson(n.toString)

  def array(elements: Iterable[ExampleValue]): ExampleValue =
    rawJson(elements.iterator.map(_.json).mkString("[", ",", "]"))

  def array(elements: ExampleValue*): ExampleValue = array(elements)

  def `object`(fields: Iterable[(String, ExampleValue)]): ExampleValue =
    rawJson(fields.iterator.map { case (k, v) => encodeJsonString(k) + ":" + v.json }.mkString("{", ",", "}"))

  def `object`(fields: (String, ExampleValue)*): ExampleValue =
    `object`(fields)

  // implemented manually to avoid introducing external dependencies into the core module
  private def encodeJsonString(str: String): String = {
    val builder = new java.lang.StringBuilder
    builder.append('"')
    var i = 0
    var s = 0
    while (i < str.length) {
      val ch = str.charAt(i)
      val esc = ch match {
        case '"'                              => '"'
        case '\\'                             => '\\'
        case '\b'                             => 'b'
        case '\f'                             => 'f'
        case '\n'                             => 'n'
        case '\r'                             => 'r'
        case '\t'                             => 't'
        case ch if Character.isISOControl(ch) => 1.toChar
        case _                                => 0.toChar
      }
      if (esc != 0) {
        builder.append(str, s, i).append('\\')
        s = i + 1
        if (esc != 1) {
          builder.append(esc)
        } else {
          builder
            .append('u')
            .append(toHex((ch >> 12) & 0xf))
            .append(toHex((ch >> 8) & 0xf))
            .append(toHex((ch >> 4) & 0xf))
            .append(toHex(ch & 0xf))
        }
      }
      i += 1
    }
    builder.append(str, s, str.length).append('"').toString
  }

  private def toHex(nibble: Int): Char =
    (nibble + (if (nibble >= 10) 'a' - 10 else '0')).toChar
}
