import sys.process._

@main def hello: Unit = {
  val cmd = "ls -l"
  val r  = cmd.!
  if r != 0 then throw RuntimeException(s"Could not execute $cmd")
  println("Success")
}
