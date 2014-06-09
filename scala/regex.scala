sealed trait Regex {
  def matches(str: String): Boolean = {
    val all = matchesWithRest(str)
    all.nonEmpty && all.find(_ == "").isDefined
  }

  def matchesWithRest(str: String): Seq[String]
}
case object AnyChar extends Regex {
  def matchesWithRest(str: String): Seq[String] = 
    if (str.length >= 1) Seq(str.substring(1)) else Seq()
}
case class Letter(c: Char) extends Regex {
  def matchesWithRest(str: String): Seq[String] =
    if (str.length >= 1 && str.charAt(0) == c)
      Seq(str.substring(1))
    else
      Seq()
}
case class Repeat(r: Regex) extends Regex {
  // Needs to get the transitive closure
  def matchesWithRest(str: String): Seq[String] = {
    def loop(str: String, seen: Set[String]): Set[String] = {
      val current = r.matchesWithRest(str)
      val newSeen = seen ++ current
      (newSeen -- seen).foldLeft(newSeen)((res, cur) =>
        res ++ loop(cur, res))
    }
    loop(str, Set()).toSeq
  }
}
    
case class RegexAnd(r1: Regex, r2: Regex) extends Regex {
  def matchesWithRest(str: String): Seq[String] =
    for {
      rest <- r1.matchesWithRest(str)
      result <- r2.matchesWithRest(rest)
    } yield result
}

object RegexTest {
  // a
  val r1 = Letter('a')

  // .
  val r2 = AnyChar

  // ab
  val r3 = RegexAnd(Letter('a'), Letter('b'))

  // a*b
  val r4 = RegexAnd(Repeat(Letter('a')), Letter('b'))
}
