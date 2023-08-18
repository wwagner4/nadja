import sys.process.Process

object Main {

  @main def hello: Unit = {
    val cmdList = List("ls", "-l")
    exe(cmdList)
    println("Success")
  }

  def exe(cmdList: List[String]) = {
    val p = Process(cmdList)
    val r  = p.run()
    if r.exitValue() != 0 then throw RuntimeException(s"Could not execute $cmdList")
  }
}
