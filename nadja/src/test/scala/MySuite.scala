
class MySuite extends munit.FunSuite {

  List(
    ("coverall_D.jpg", Some(NFilename("coverall", "D", "jpg"))),
    ("coverall_N.jpg", Some(NFilename("coverall", "N", "jpg"))),
    ("coverall_J.jpg", Some(NFilename("coverall", "J", "jpg"))),
    ("coverall_A.jpg", Some(NFilename("coverall", "A", "jpg"))),
    ("x_A.JPG", Some(NFilename("x", "A", "JPG"))),
    ("xA.JPG", None),
    ("x_X.JPG", Some(NFilename("x", "X", "JPG"))),
    ("x_NJPG", None),
    ("", None),
  ).foreach { (fn, nfn) =>
    test(s"convert filename $fn") {
      assertEquals(Util.parseFilename(fn), nfn)
    }
  }

  val patterns = List(
    (
      """...
        |ABC
        |...
        |""".stripMargin,
      3, 3, List("CBL", "CBL", "CBL", "A", "B", "C", "CBL", "CBL", "CBL")
    ),
    (
      """..
        |AB
        |..
        |""".stripMargin,
      3, 2, List("CBL", "CBL", "A", "B", "CBL", "CBL")
    ),
    (
      """.
        |AB
        |..
        |""".stripMargin,
      3, 1, List("CBL", "A", "CBL")
    ),
  )

  patterns.foreach((p, r, c, ids) => {
    val testname = ids.mkString("|")
    test(s"patterns to ids ${testname}") {
      val pattern =
        """...
          |ABC
          |...
          |""".stripMargin

      val expectedRows = 3
      val expectedCols = 3
      val expectedIds = List("CBL", "CBL", "CBL", "A", "B", "C", "CBL", "CBL", "CBL")

      val canvas = MagickUtil.patternToCanvas(pattern)
      assertEquals(canvas.rows, expectedRows)
      assertEquals(canvas.cols, expectedCols)
      assertEquals(canvas.ids.take(expectedRows * expectedCols), expectedIds)
    }
  })

  List(
    (4, 4),
    (5, 4),
    (2044, 2044),
    (2055, 2054)
  ).foreach { (in, expected) => {
    test(s"round to even ${in}") {
      assertEquals(MagickUtil.roundToEven(in), expected)
    }

  }
  }

  List(
    (4.0, 4),
    (5.0, 4),
    (2044.0, 2044),
    (2055.0, 2054),
    (2055.6, 2056),
    (2056.49, 2056),
  ).foreach { (in, expected) => {
    test(s"round to even ${in}") {
      assertEquals(MagickUtil.roundToEven(in), expected)
    }

  }
  }


}
