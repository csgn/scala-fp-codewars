import scala.util.Random

class MySuite extends munit.FunSuite {
  def runTestCases[A, B](testCases: List[(A, B)], func: A => B) =
    testCases.foreach { case (xs, expected) =>
      assertEquals(func(xs), expected, s"\n$xs should be $expected\n")
    }

  def runTestCases2[A, B, C](testCases: List[(A, B, C)], func: (A, B) => C) =
    testCases.foreach { case (x1, x2, expected) =>
      assertEquals(
        func(x1, x2),
        expected,
        s"\n($x1, $x2) should be $expected\n"
      )
    }

  def runTestCases3[A, B, C, D](
      testCases: List[(A, B, C, D)],
      func: (A, B, C) => D
  ) =
    testCases.foreach { case (x1, x2, x3, expected) =>
      assertEquals(
        func(x1, x2, x3),
        expected,
        s"\n($x1, $x2, $x3) should be $expected\n"
      )
    }

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

    runTestCases(testCases, findNb)
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

    runTestCases(testCases, high)
  }

  test("Split Strings") {
    val testCases = List(
      ("asdfadsf", List("as", "df", "ad", "sf")),
      ("asdfads", List("as", "df", "ad", "s_")),
      ("", List()),
      ("x", List("x_"))
    )

    runTestCases(testCases, splitStrings)
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

    runTestCases(testCases, persistence)
  }

  test("Write Number in Expanded Form") {
    val testCases = List(
      (2L, "2"),
      (12L, "10 + 2"),
      (42L, "40 + 2"),
      (4982342L, "4000000 + 900000 + 80000 + 2000 + 300 + 40 + 2"),
      (420370022L, "400000000 + 20000000 + 300000 + 70000 + 20 + 2"),
      (70304L, "70000 + 300 + 4"),
      (9000000L, "9000000")
    )

    runTestCases(testCases, expandedForm)
  }

  test("Multiples of 3 or 5") {
    val testCases = List(
      (0, 0),
      (10, 23),
      (15, 45),
      (16, 60),
      (20, 78)
    )

    runTestCases(testCases, multiplesOf3Or5)
  }

  test("Find The Parity Outlier") {
    val testCases = List(
      (List(2, 4, 6, 8, 10, 3), 3),
      (List(2, 4, 0, 100, 4, 11, 2602, 36), 11),
      (List(160, 3, 1719, 19, 11, 13, -21), 160),
      (List(1, 3, -1, 4, 9), 4),
      (List(1, 1, -1, 1, 1, -44, 7, 7, 7, 7, 7, 7, 7, 7), -44)
    )

    runTestCases(testCases, findOutlier)
  }

  test("RGB To Hex Conversion") {
    val testCases = List(
      (0, 0, 0, "000000"),
      (1, 2, 3, "010203"),
      (255, 255, 255, "FFFFFF"),
      (254, 253, 252, "FEFDFC"),
      (-20, 275, 125, "00FF7D")
    )

    runTestCases3(testCases, rgb)
  }

  test("Pyramid Array") {
    val testCases = List(
      (0, List[List[Int]]()),
      (1, List[List[Int]](List(1))),
      (2, List[List[Int]](List(1), List(1, 1))),
      (3, List[List[Int]](List(1), List(1, 1), List(1, 1, 1)))
    )

    runTestCases(testCases, pyramid)
  }

  test("Primes in numbers") {
    val testCases = List(
      (7775460, "(2**2)(3**3)(5)(7)(11**2)(17)"),
      (7919, "(7919)")
    )

    runTestCases(testCases, factors)
  }

  test("Tribonacci Sequence") {
    val testCases = List(
      (List(1, 1, 1), 10, List(1, 1, 1, 3, 5, 9, 17, 31, 57, 105)),
      (List(0, 0, 1), 10, List(0, 0, 1, 1, 2, 4, 7, 13, 24, 44)),
      (List(0, 1, 1), 10, List(0, 1, 1, 2, 4, 7, 13, 24, 44, 81)),
      (List(1, 0, 0), 10, List(1, 0, 0, 1, 1, 2, 4, 7, 13, 24)),
      (List(0, 0, 0), 10, List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)),
      (List(1, 2, 3), 10, List(1, 2, 3, 6, 11, 20, 37, 68, 125, 230)),
      (List(3, 2, 1), 10, List(3, 2, 1, 6, 9, 16, 31, 56, 103, 190)),
      (List(1, 1, 1), 1, List(1)),
      (List(300, 200, 100), 0, List())
    )

    runTestCases2(testCases, tribonacci)
  }

  test("Snail") {
    val testCases = List(
      (
        List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)),
        List(1, 2, 3, 6, 9, 8, 7, 4, 5)
      ),
      (
        List(List(1, 2, 3), List(8, 9, 4), List(7, 6, 5)),
        List(1, 2, 3, 4, 5, 6, 7, 8, 9)
      )
    )

    runTestCases(testCases, snail)
  }

}
