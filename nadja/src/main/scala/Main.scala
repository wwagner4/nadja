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
    mainMontage()
  }

  def mainResize() = {
    val root1 = os.home / "work" / "nadja" / "kimi2-t1"
    val t500 = root1 / "t100"
    os.makeDir.all(t500)
    resizeAll(root1, 500, t500)
  }

  def mainMontage() = {
    val root = os.home / "work" / "nadja" / "kimi2-t1"
    val out = os.home / "work" / "nadja" / "kimi2-t1" / "out"
    os.makeDir.all(out)
    (1 to 8).foreach {rows =>
      val size = Util.sizeFromRows(rows)
      val sizedRoot = os.temp.dir()
      resizeAll(root, size, sizedRoot)
      val base = Util.createBase(sizedRoot)
      val infiles = fienames(base, nadja)
      val outfile = out / s"out008_${rows}.jpg"
      val t1 = os.temp()
      montage(infiles, rows=rows, cols=5, size=size, outfile=t1)
      resize(t1, outfile)
    }
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

  val imageExts = Seq("jpg", "jpeg", "png")


  def resizeAll(indir: os.Path, size: Int, outdir: os.Path) = {

    val geo = s"${size}x${size}"

    def rs(f: os.Path) = {
      val cmd = Seq(
        "convert",
        f.toString(),
        "-resize",
        geo,
        (outdir / f.last).toString()
      )
      println(cmd.mkString(" \\\n"))
      Util.exe(cmd)
    }

    os.list(indir)
      .filter(p => os.isFile(p) && imageExts.contains(p.ext.toLowerCase()))
      .foreach(rs(_))
  }

  def montage(infiles: Iterable[os.Path], rows: Int, cols: Int, size: Int, outfile: os.Path) = {
    val tilesgeo = s"${cols}x${rows}"
    val fnams = infiles.take(rows * cols).map(_.toString())
    val cmd = List(
      "montage", 
      "-fill", 
      "black", 
      "-background", 
      "black", 
      "-bordercolor", 
      "black", 
      "-resize",
      s"${size}x",
      "-gravity",
      "east",
      "-borderwidth",
      "0", 
      "-tile", 
      tilesgeo, 
      "-geometry", 
      "+0+0",
    ) ++
    fnams ++
    List(
      s"${outfile}",
    )

    println(cmd.mkString(" \\\n"))
    Util.exe(cmd)
  }


  def fienames(base: NBase, chars: List[NChar]): LazyList[os.Path] = {
    val cs = chars
      .flatMap {c =>  base.files.filter{f => f.char == c}}
      .map(f => Util.path(f, base.path))
    LazyList.continually(cs).flatten
  }

}

object Util {

  def sizeFromRows(rows: Int): Int = {
    if rows < 1 then throw IllegalArgumentException(s"Illegal value for sizeFromRows ${rows}")
    rows match {
      case 1 => 1200
      case 2 => 600
      case 3 => 400
      case _ => 200
    }
  }

  def createBase(path: os.Path): NBase = {

    val files = os.list(path)
      .toList
      .map(p => p.last)
      .flatMap(fn => Util.parseFilename(fn))
    NBase(
      path, files
    )
  }

  def exe(cmdList: Seq[String]) = {
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
