import sys.process.Process

import scala.collection.JavaConverters._
import java.nio.file.{Path, Files}

enum NChar {
  case N, A, D, J,
}

val nadja = List(NChar.N, NChar.A, NChar.D, NChar.J, NChar.A )

case class NBase (
  path: Path,
  files: List[NFilename],
)

case class NFilename(
  name: String,
  char: NChar,
  ext: String
)

object Main {

  @main def hallo: Unit = {
    val root = Path.of("/home/wwagner4/work/nadja/kimi2-t1/t1200")
    val out = Path.of("/home/wwagner4/work/nadja/kimi2-t1-out")
    if Files.notExists(out) then Files.createDirectory(out)
    println(s"root: $root")
    val base = Util.createBase(root)

    val infiles = fienames(base, nadja)
    val outPath = out.resolve("out001.jpg")
  
    val cmd = List(
      "montage", 
      "-fill", 
      "black", 
      "-bordercolor", 
      "black", 
      "-tile", 
      "5x1", 
      "-geometry", 
      "+5+5",
    ) ++
    infiles ++
    List(
      s"${outPath}",
    )

    println(cmd.mkString("\n"))
    Util.exe(cmd)
  }

  def fienames(base: NBase, chars: List[NChar]): List[String] = {
    chars
      .flatMap {c =>  base.files.filter{f => f.char == c}}
      .map(f => Util.path(f, base.path))
      .map(p => p.toString())
  }

}

object Util {

  def createBase(path: Path): NBase = {

    val files = Files.list(path)
      .iterator
      .asScala
      .toList
      .map(p => p.getFileName().toString())
      .flatMap(fn => Util.parseFilename(fn))
    NBase(
      path, files
    )
  }

  def exe(cmdList: List[String]) = {
    val p = Process(cmdList)
    val r  = p.run()
    if r.exitValue() != 0 then throw RuntimeException(s"Could not execute $cmdList")
  }

  def parseFilename(filename: String): Option[NFilename] = {
    try 
      val a = filename.split("\\.")
      val b = a(0).split("_")
      val c = NChar.valueOf(b(1))
      Some(NFilename(b(0), c, a(1)))  
    catch
      case _: Exception => {
        println(s"No image file ${filename}")
        None
    }
  }

  def path(nfn: NFilename, root: Path): Path = {
    val x = s"${nfn.name}_${nfn.char}.${nfn.ext}" 
    root.resolve(x)
  }
}
