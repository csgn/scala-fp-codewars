import scala.annotation.tailrec

/*
 * Break camelCase
 * https://www.codewars.com/kata/5208f99aee097e6552000148/train/scala
 */
def breakCamelCase(s: String): String =
  @tailrec
  def go(s: String, acc: String): String =
    if s.isEmpty then acc
    else
      val h = s.head
      val t = s.tail
      if h.isLower then go(t, acc + h)
      else go(t, acc + " " + h)

  go(s, "")

/*
 * Are they the "same"?
 * https://www.codewars.com/kata/550498447451fbbd7600041c/train/scala
 */
def comp(seq1: Seq[Int], seq2: Seq[Int]): Boolean =
  if seq1 == null || seq2 == null then false
  else
    @tailrec
    def go(s1: Seq[Int], s2: Seq[Int], acc: Boolean): Boolean =
      if s1.isEmpty && s2.isEmpty then acc
      else if s1.isEmpty || s2.isEmpty then false
      else if s2.head == math.pow(s1.head, 2) then go(s1.tail, s2.tail, true)
      else false

    go(seq1.sorted, seq2.sorted, true)

/*
 * Build a pile of Cubes
 * https://www.codewars.com/kata/5592e3bd57b64d00f3000047/train/scala
 */
def findNb(m: Long): Int =
  @tailrec
  def go(n: Int, acc: Long): Int =
    if acc == m then n - 1
    else if acc > m then -1
    else go(n + 1, (acc + math.pow(n, 3).toLong).toLong)

  go(1, 0)

/*
 * Highest Scoring Word
 * https://www.codewars.com/kata/57eb8fcdf670e99d9b000272/scala
 */
def high(s: String): String =
  @tailrec
  def go(ss: String, count: Int, acc: Int, buff: String, res: String): String =
    if ss.isEmpty then
      if count > acc then buff
      else res
    else
      val h = ss.head
      val t = ss.tail

      if h == ' ' then
        if count > acc then go(t, 0, count, "", buff)
        else go(t, 0, acc, "", res)
      else go(t, count + h.toInt - 96, acc, buff + h, res)

  go(s, 0, 0, "", "")

/*
 * Split Strings
 * https://www.codewars.com/kata/515de9ae9dcfc28eb6000001/train/scala
 */
def splitStrings(s: String): List[String] =
  @tailrec
  def go(
      ss: String,
      count: Int,
      buff: String,
      acc: List[String]
  ): List[String] =
    if ss.isEmpty then
      if !buff.isEmpty then acc ::: List(buff + "_")
      else acc
    else
      val h = ss.head
      val t = ss.tail
      val nextBuff = buff + h

      if count % 2 == 0 then go(t, 1, "", acc ::: List(nextBuff))
      else go(t, count + 1, nextBuff, acc)

  go(s, 1, "", List())

/*
 * Find the missing term in an Arithmetic Progression
 * https://www.codewars.com/kata/52de553ebb55d1fca3000371/train/scala
 */
def findMissing(sequence: Seq[Int]): Int = 0

/*
 * Persistent Bugger
 * https://www.codewars.com/kata/55bf01e5a717a0d57e0000ec/train/scala
 */
def persistence(n: Int): Int =
  @tailrec
  def go(x: Int, buff: Int, acc: Int): Int =
    if x == 0 then
      if buff / 10 == 0 then
        if acc == 0 && buff != 0 then acc
        else acc + 1
      else go(buff, 1, acc + 1)
    else go(x / 10, buff * (x % 10), acc)

  go(n, 1, 0)

/*
 * Write Number in Expanded Form
 * https://www.codewars.com/kata/5842df8ccbd22792a4000245/train/scala
 */
def expandedForm(n: Long): String =
  @tailrec
  def go(nn: Long, place: Long, acc: String): String =
    if nn == 0 then acc
    else
      val c: Long = nn % 10
      val nextNN: Long = nn / 10
      val nextPlace: Long = place * 10
      val nextAcc = if c == 0 then "" else (c * place).toString

      if nextAcc == "" then go(nextNN, nextPlace, acc)
      else if place == 1 || place != 1 && acc == "" then
        go(nextNN, nextPlace, nextAcc + acc)
      else go(nextNN, nextPlace, nextAcc + " + " + acc)

  go(n, 1, "")
