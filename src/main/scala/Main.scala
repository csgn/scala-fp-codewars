import scala.annotation.tailrec
import math.Numeric.Implicits.infixNumericOps

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

/*
 * Multiples of 3 or 5
 * https://www.codewars.com/kata/514b92a657cdc65150000006/train/scala
 */
def multiplesOf3Or5(number: Int): Long =
  @tailrec
  def go(c: Int, acc: Long): Long =
    if c == number then acc
    else go(c + 1, if c % 3 == 0 || c % 5 == 0 then c + acc else acc)

  go(0, 0)

/*
 * Find The Parity Outlier
 * https://www.codewars.com/kata/5526fc09a1bbd946250002dc/train/scala
 */
def findOutlier(integers: List[Int]): Int =
  @tailrec
  def go(as: List[Int], acc: Option[Int], acc2: Option[Int]): Option[Int] =
    if as.isEmpty then acc
    else
      val h = as.head
      val t = as.tail

      acc match
        case None => go(t, Some(h), None)
        case Some(value1) =>
          acc2 match
            case None =>
              if math.abs(h) % 2 == math.abs(value1) % 2 then go(t, acc, None)
              else go(t, Some(h), acc)
            case Some(value2) =>
              if math.abs(h) % 2 == math.abs(value2) % 2 then acc else acc2

  go(integers, None, None) match
    case None        => -1
    case Some(value) => value

/*
 * RGB To Hex Conversion
 * https://www.codewars.com/kata/513e08acc600c94f01000001/train/scala
 */

def rgb(r: Int, g: Int, b: Int): String =
  List(r, g, b)
    .map(el => {
      if el < 0 then 0 else if el > 255 then 255 else el
    })
    .map(el => {
      val hx = el.toHexString.toUpperCase
      if hx.length < 2 then "0" + hx
      else hx
    })
    .fold("")((acc, el) => acc + "" + el)

/*
 * Pyramid Array
 * https://www.codewars.com/kata/515f51d438015969f7000013/train/scala
 */
def pyramid(n: Int): List[List[Int]] =
  def go(c: Int, acc: List[List[Int]]): List[List[Int]] =
    if c > n then acc
    else
      val ll = if c == 0 then Nil else List(List.fill(c)(1))
      go(c + 1, acc ::: ll)

  go(0, Nil)

/*
 * Primes in numbers
 * https://www.codewars.com/kata/54d512e62a5e54c96200019e/train/scala
 */
def factors(m: Int): String =
  def go(n: Int, d: Int, c: Int, acc: String): String =
    if n <= 1 then
      if c > 0 then acc + s"($d)"
      else acc
    else if n % d == 0 then go(n / d, d, c + 1, acc)
    else
      val nextAcc =
        if c > 0 then acc + "(" + d + (if c > 1 then s"**$c" else "") + ")"
        else acc
      go(n, d + 1, 0, nextAcc)

  go(m, 2, 0, "")

/*
 * Tribonacci Sequence
 * https://www.codewars.com/kata/556deca17c58da83c00002db/train/scala
 */
def tribonacci[T: Numeric](signature: List[T], n: Int): List[T] =
  if n <= 3 then signature take n
  else
    @tailrec
    def go(f: T, s: T, t: T, acc: List[T]): List[T] =
      if acc.length >= n then acc
      else
        val r = f + s + t
        go(s, t, r, acc ::: List(r))

    go(signature(0), signature(1), signature(2), signature)

/*
 * Snail
 * https://www.codewars.com/kata/521c2db8ddc89b9b7a0000c1/train/scala
 */
def snail(xs: List[List[Int]]): List[Int] = ???
