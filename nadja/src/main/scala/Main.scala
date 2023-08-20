import sys.process.Process
import scala.jdk.CollectionConverters._


enum NChar {
  case N, A, D, J,
}

val nadja = List(NChar.N, NChar.A, NChar.D, NChar.J, NChar.A )

case class NBase (
  path: os.Path,
  files: List[NFilename],
)

case class NFilename(
  name: String,
  char: NChar,
  ext: String
)

object Main {

  @main def hallo: Unit = {
    val root = os.home / "work" / "nadja" / "kimi2-t1" / "t1200"
    val out = os.home / "work" / "nadja" / "kimi2-t1" / "out"
    os.makeDir.all(out)
    println(s"root: $root")
    val base = Util.createBase(root)

    val infiles = fienames(base, nadja)
    val t1 = os.temp()
    val outfile = out / "out006.jpg"
  
    montage(infiles, rows=4, cols=2, outfile=t1)
    resize(t1, outfile)


  }

  def resize(infile: os.Path, outfile: os.Path) = {
    val w = 1500
    val h = (w * 3 / 4).toInt
    val geo1 = s"${w}x${h}"

    val cmd1 = List(
      "convert",
      s"${infile}", 
      "-background",
      "black", 
      "-gravity", 
      "center",
      "-resize",
      geo1, 
      "-extent", 
      geo1,  
    ) ++
      List(
        s"${outfile}",
    )
    println(cmd1.mkString(" \\\n"))
    Util.exe(cmd1)
  }

  def montage(infiles: List[os.Path], rows: Int, cols: Int, outfile: os.Path) = {
    val tilesgeo = s"${rows}x${cols}"
    val cmd = List(
      "montage", 
      "-fill", 
      "black", 
      "-background", 
      "black", 
      "-bordercolor", 
      "black", 
      "-resize",
      "1000x",
      "-gravity",
      "east",
      "-borderwidth", 
      "0", 
      "-tile", 
      tilesgeo, 
      "-geometry", 
      "+0+0",
    ) ++
    infiles.map{_.toString()} ++
    List(
      s"${outfile}",
    )

    println(cmd.mkString(" \\\n"))
    Util.exe(cmd)
  }


  def fienames(base: NBase, chars: List[NChar]): List[os.Path] = {
    chars
      .flatMap {c =>  base.files.filter{f => f.char == c}}
      .map(f => Util.path(f, base.path))
  }

}

object Util {

  def createBase(path: os.Path): NBase = {

    val files = os.list(path)
      .toList
      .map(p => p.last)
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

  def path(nfn: NFilename, root: os.Path): os.Path = {
    val x = s"${nfn.name}_${nfn.char}.${nfn.ext}" 
    root / x
  }
}
