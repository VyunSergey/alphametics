object Alphametics {
  def valid(part: String): Boolean = {
    part.split(" \\+ ").forall(_.forall(c => c.isLetter && c.isUpper))
  }

  def startChars(part: String): Set[Char] = {
    part.split(" \\+ ").map(_.head).toSet
  }

  def solve(eq: String): Option[Map[Char, Int]] = eq.split(" == ").toList match {
    case left :: right :: Nil if valid(left) && valid(right) => solve(left, right)
    case _ => None
  }

  def solve(left: String, right: String): Option[Map[Char, Int]] = {
    val letters: Set[Char] = Set('A' to 'Z': _*) intersect (left.toSet union right.toSet)
    val startLetters: Set[Char] = startChars(left) union startChars(right)
    println(startLetters, letters)

    val candidates: Map[Char, List[Int]] =
      Map.from(letters.map(c => c -> List.from(0 to 9))) ++
        Map.from(startLetters.map(c => c -> List.from(1 to 9)))
    println(candidates)

    None
  }
}
