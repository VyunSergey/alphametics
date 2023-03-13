import scala.annotation.tailrec

object Alphametics {
  val alphabet: Set[Char] = ('A' to 'Z').toSet
  val digits: Set[Int] = (0 to 9).toSet

  // MAIN solve method
  def solve(eq: String): Option[Map[Char, Int]] =
    eq.split(" == ").toList match {
      case left :: right :: Nil if valid(left) && valid(right) => solveEq(left.split(" \\+ "), right)
      case _ => None
    }

  // prepare method for creating list of equations to solve
  def solveEq(left: Array[String], right: String): Option[Map[Char, Int]] = {
    val leftEqs: List[List[(Char, Int)]] = equations(left)
    val rightEqs: List[Option[Char]] = right.toList.reverse.map(Some(_))
    val unionEqs: List[(List[(Char, Int)], Option[Char])] = leftEqs.zipAll(rightEqs, Nil, None)
    val lts: Set[Char] = letters(left) ++ letters(Array(right))
    val sts: Set[Char] = startLetters(left) ++ startLetters(Array(right))
    val conditions: Map[Char, Set[Int]] = lts.map(c => c -> digits).toMap ++ sts.map(c => c -> (digits - 0)).toMap
    solveEqs(unionEqs, 0, conditions, Map.empty[Char, Int])
  }

  // recursive method for solving list of equations from last digit to first
  def solveEqs(eqs: List[(List[(Char, Int)], Option[Char])],
               accum: Int,
               conditions: Map[Char, Set[Int]],
               known: Map[Char, Int]): Option[Map[Char, Int]] = {
    eqs match {
      case Nil =>
        // known is final solution
        Some(known)
      case ((c, n) :: rest, Some(cr)) :: tail if known.contains(c) =>
        // replace known char with digit
        solveEqs((rest, Some(cr)) :: tail, accum + n * known(c), conditions, known)
      case ((c, n) :: rest, Some(cr)) :: tail if !known.contains(c) =>
        // iterate over all available digits for unknown char
        (conditions(c) diff known.values.toSet)
          .foldLeft(Option.empty[Map[Char, Int]]) { case (answer, digit) =>
            answer orElse solveEqs((rest, Some(cr)) :: tail, accum + n * digit, conditions, known ++ Map(c -> digit))
          }
      case (Nil, Some(cr)) :: tail if known.contains(cr) =>
        // check accum equals known digit for right side char
        Some(cr).filter(accum % 10 == known(_)).flatMap { _ =>
          solveEqs(tail, accum / 10, conditions, known)
        }
      case (Nil, Some(cr)) :: tail if !known.contains(cr) &&
        (conditions(cr) diff known.values.toSet).contains(accum % 10) =>
        // found digit for right side char
        solveEqs(tail, accum / 10, conditions, known ++ Map(cr -> accum % 10))
      case _ => None
    }
  }

  // TEXT -> (A ... Z)[_+_(A ... Z)]*...
  def valid(part: String): Boolean = {
    part.split(" \\+ ").forall(_.forall(c => alphabet.contains(c)))
  }

  // [[A, B], [C]] UNION [[C], [D, E], [F]] -> [[A, B, C], [C, D, E], [F]]
  @tailrec
  def union[A](left: List[List[A]], right: List[List[A]], res: List[List[A]] = Nil): List[List[A]] =
    (left, right) match {
      case (l :: ls, r :: rs) => union(ls, rs, (l ++ r) :: res)
      case (lh :: ls, Nil)    => union(ls, Nil, lh :: res)
      case (Nil, rh :: rs)    => union(Nil, rs, rh :: res)
      case (Nil, Nil)         => res.reverse
    }

  // A + BB + AA + CDE -> [[(A,2), (B,1), (E,1)], [(B,1), (A,1), (D,1)], [(C,1)]]
  def equations(parts: Array[String]): List[List[(Char, Int)]] = {
    parts
      .map(_.toList.map(c => List(c)).reverse)
      .reduce(union(_, _))
      .map(_.groupBy(x => x).view.mapValues(_.length).toList)
  }

  // A + BB + CDE -> [A, B, C, D, E]
  def letters(parts: Array[String]): Set[Char] = {
    parts.flatMap(_.toCharArray).toSet
  }

  // A + BB + CDE -> [A, B, C]
  def startLetters(parts: Array[String]): Set[Char] = {
    parts.flatMap(_.headOption).toSet
  }

}
