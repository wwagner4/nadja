class MySuite extends munit.FunSuite {

  List(
    ("N", NChar.N),
    ("A", NChar.A),
    ("D", NChar.D),
    ("J", NChar.J),
  ).foreach{ (s, v) =>
    test(s"create enum from string $s") {
      assertEquals(NChar.valueOf(s), v) 
    }
  }

  List(
    ("coverall_D.jpg", NFilename("coverall", NChar.D, "jpg")),
    ("coverall_N.jpg", NFilename("coverall", NChar.N, "jpg")),
    ("coverall_J.jpg", NFilename("coverall", NChar.J, "jpg")),
    ("coverall_A.jpg", NFilename("coverall", NChar.A, "jpg")),
    ("x_A.JPG", NFilename("x", NChar.A, "JPG")),
  ).foreach{ (fn, nfn) =>
    test(s"convert filename $fn") {
      assertEquals(Main.parseFilename(fn), nfn)
    }
  }
}

