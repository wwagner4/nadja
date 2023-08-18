import sys.process.Process

import scala.collection.JavaConverters._
import java.nio.file.{Path, Files}

enum NChar {
  case N, A, D, J,
}

case class NFilename(
  name: String,
  char: NChar,
  ext: String
)

object Main {

  @main def hello: Unit = {
    val root = Path.of("/home/wwagner4/work/nadja/kimi2-t1")
    println(s"root: $root")
    println("Success")
    Files.list(root)
      .iterator
      .asScala
      .map(p => p.getFileName().toString())
      //.map(fn => parseFilename(fn))
      .foreach(nfn => println(nfn))
  }

  def exe(cmdList: List[String]) = {
    val p = Process(cmdList)
    val r  = p.run()
    if r.exitValue() != 0 then throw RuntimeException(s"Could not execute $cmdList")
  }

  def parseFilename(filename: String): NFilename = {
    val a = filename.split("\\.")
    val b = a(0).split("_")
    val c = NChar.valueOf(b(1))
    NFilename(b(0), c, a(1)) 
  }
}
