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

}
