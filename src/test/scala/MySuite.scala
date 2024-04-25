import scala.util.Random

class MySuite extends munit.FunSuite {
  test("breakCamelCase") {
    assertEquals(
      breakCamelCase("helloWorld"),
      "hello World",
      "for breakCamelCase(\"helloWorld\")"
    )
    assertEquals(
      breakCamelCase("camelCase"),
      "camel Case",
      "for breakCamelCase(\"camelCase\")"
    )
    assertEquals(
      breakCamelCase("breakCamelCase"),
      "break Camel Case",
      "for breakCamelCase(\"breakCamelCase\")"
    )
  }

  test("Are they the same?") {
    val seq1 = Seq(121, 144, 19, 161, 19, 144, 19, 11)
    val seq2 = Seq(
      11 * 11,
      121 * 121,
      144 * 144,
      19 * 19,
      161 * 161,
      19 * 19,
      144 * 144,
      19 * 19
    )
    assertEquals(
      comp(seq1, seq2),
      true,
      s"\ncomp(${seq1}, ${seq2}) should be true"
    )

    val seq3 = List(121, 144, 19, 161, 19, 144, 19, 11)
    val seq4 = List(231, 14641, 20736, 361, 25921, 361, 20736, 361)
    assertEquals(
      comp(seq3, seq4),
      false,
      s"\ncomp(${seq3}, ${seq4}) should be false"
    )

    val seq5 = List()
    val seq6 = List()
    assertEquals(
      comp(seq5, seq6),
      true,
      s"\ncomp(${seq5}, ${seq6}) should be true"
    )

    val seq7 = null
    val seq8 = List()
    assertEquals(
      comp(seq7, seq8),
      false,
      s"\ncomp(${seq7}, ${seq8}) should be false"
    )

    val seq9 = List(2, 2, 3)
    val seq10 = List(4, 9, 9)
    assertEquals(
      comp(seq9, seq10),
      false,
      s"\ncomp(${seq9}, ${seq10}) should be false"
    )

    val seq11 = List.fill(8)(Random().nextInt(100))
    val seq12 = seq11.map(x => x * x)
    assertEquals(
      comp(seq11, seq12),
      true,
      s"\ncomp(${seq11}, ${seq12}) should be true"
    )
  }

  test("Build a pile of Cubes") {
    val testCases = List(
      (4183059834009L, 2022),
      (24723578342962L, -1),
      (135440716410000L, 4824),
      (40539911473216L, 3568),
      (26825883955641L, 3218),
      (640032000400000000L, 40000)
    )

    testCases.foreach { case (m, expected) =>
      assertEquals({ findNb(m) }, expected, s"\nInput:\n m = $m")
    }
  }

  test("Highest Scoring Word") {
    val testCases = List(
      ("man i need a taxi up to ubud", "taxi"),
      ("what time are we climbing up to the volcano", "volcano"),
      ("take me to semynak", "semynak"),
      ("for equals return earliestg", "return"),
      ("find or finda", "finda"),
      ("aa b", "aa"),
      ("b aa", "b"),
      ("bb d", "bb"),
      ("d bb", "d"),
      ("aaa b", "aaa")
    )

    testCases.foreach { case (m, expected) =>
      assertEquals({ high(m) }, expected, s"\n$m should be $expected\n")
    }
  }

  test("Split Strings") {
    val testCases = List(
      ("asdfadsf", List("as", "df", "ad", "sf")),
      ("asdfads", List("as", "df", "ad", "s_")),
      ("", List()),
      ("x", List("x_"))
    )

    testCases.foreach { case (m, expected) =>
      assertEquals({ splitStrings(m) }, expected, s"\n$m should be $expected\n")
    }
  }

  // test("Find the missing term in an Arithmetic Progression") {
  //   val testCases = List(
  //     (Seq(1, 2, 3, 4, 6, 7, 8, 9), 5),
  //     (Seq(1, 3, 4, 5, 6, 7, 8, 9), 2),
  //     (Seq(1, 3, 5, 9, 11), 7)
  //   )
  //
  //   testCases.foreach { case (xs, expected) =>
  //     assertEquals(findMissing(xs), expected, s"\n$xs should be $expected\n")
  //   }
  // }

  test("Persistent Bugger") {
    val testCases = List(
      (39, 3),
      (999, 4),
      (4, 0),
      (397018, 1)
    )

    testCases.foreach { case (xs, expected) =>
      assertEquals(persistence(xs), expected, s"\n$xs should be $expected\n")
    }
  }

}
