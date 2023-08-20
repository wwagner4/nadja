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
    ("coverall_D.jpg", Some(NFilename("coverall", NChar.D, "jpg"))),
    ("coverall_N.jpg", Some(NFilename("coverall", NChar.N, "jpg"))),
    ("coverall_J.jpg", Some(NFilename("coverall", NChar.J, "jpg"))),
    ("coverall_A.jpg", Some(NFilename("coverall", NChar.A, "jpg"))),
    ("x_A.JPG", Some(NFilename("x", NChar.A, "JPG"))),
    ("xA.JPG", None),
    ("x_X.JPG", None),
    ("x_NJPG", None),
    ("", None),
  ).foreach{ (fn, nfn) =>
    test(s"convert filename $fn") {
      assertEquals(Util.parseFilename(fn), nfn)
    }
  }
}
