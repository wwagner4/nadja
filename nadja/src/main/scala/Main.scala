import sys.process.Process
import scala.jdk.CollectionConverters._
import scala.util.Random

enum NChar {
  case N, A, D, J, CBL
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

  @main def mainEntryPoint: Unit = {
    // mainMontage()
    mainSwipe()
  }

  def mainSwipe() = {

    def slowFactors(): Iterable[Int] = {
      def f(i: Int): Int = {
        if i < 200 then return 1
        if i < 210 then return 2
        if i < 332 then return 4
        return 1000
      }   

      LazyList.from(0)
        .map(f(_))
    }

    def applySlowDown[T](in: Iterable[T]): Iterable[T] = {
      in
        .zip(slowFactors())
        .flatMap((a, i) => List.fill(i)(a))
    }

    def mySlices[T](in: List[T], slizeLen: Int): Seq[List[T]] = {
      val maxIndex = in.size - slizeLen
      (0 to maxIndex).map(i => in.slice(i, i + slizeLen))
    }

    def doSwipe(base: NBase, len: Int, viewcols: Int, outdir: os.Path) = {

      val visible = List(
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        "N",
        "A",
        "D",
        "J",
        "A",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
        ".",
      ).map(Util.nChar)

      val slices = LazyList.continually(mySlices(visible, viewcols)).flatten
      val descs = applySlowDown(slices)
      descs
        .zipWithIndex
        .take(len)
        .foreach((desc, index) => createImage(base, desc, index, viewcols, outdir))


      val videoFile = outdir / "nadjaSwipe.mp4"
      video(outdir, 30 ,videoFile)
      println("------------------------------------------------------------------")
      println(s"Created video: ${videoFile}")

    }

    def createImage(base: NBase, descr: List[NChar], index: Int, viewcols: Int, outdir: os.Path) = {

      val zi = "%04d".format(index)
      val outfile = outdir / s"nadja_${zi}.jpg"

      def createFile(out: os.Path) = {
        val fns = fienamesContinually(base, descr).take(descr.size)
        val t1 = os.temp()
        montage(fns, rows=1, cols=viewcols, outfile=t1)
        resize(t1, 800, 500, outfile)
      }

      val key = descr.mkString("")
      FileCache.save(key, outfile, createFile)
    }

    val name = "coverall"
    val id = "03"
    val len = 500
    val viewcols = 7

    val rootdir = os.pwd / "src" / "test" / "resources" / name
    val outdir = os.home / "work" / "nadja" / "out" / "swipe" / s"${name}-${id}"
    os.makeDir.all(outdir)
    
    val resizedDir = os.temp.dir()
    resizeAll(rootdir, 1200, resizedDir)
    val base = Util.createBase(resizedDir)
    println(s"base: ${base}")
    doSwipe(base, len, viewcols, outdir)
  }


  def mainMontage() = {

    val name = "coverall"
    val id = "12"
    val maxrows = 6
    val combis = 5
    val framerate = 5
    val width = 800
    val height = 500

    // val rootdir = os.home / "work" / "nadja" / name
    val rootdir = os.pwd / "src" / "test" / "resources" / name
    val outdir = os.home / "work" / "nadja" / "out" / "montage" /s"${name}-${id}"
    os.makeDir.all(outdir)
    
    (0 until maxrows).foreach {j =>
      val rows = maxrows - j
      val cols = Util.colsFromRows(rows)
      val sizedRootDir = os.temp.dir()
      val size = Util.sizeFromRows(rows)
      resizeAll(rootdir, size, sizedRootDir)
      val base = Util.createBase(sizedRootDir)
      val infiles = fienamesContinually(base, nadja)
      (1 to combis).foreach {i =>
        val outfile = outdir / s"nadja_${(j+1) * 100 + i}.jpg"
        val t1 = os.temp()

        val randomize = j != (maxrows - 1) || i != combis
        val fns = infiles
          .take(rows * cols)
          .toList
        val fnams = randomize match {
          case true => Random.shuffle(fns)
          case false => fns
        }

        montage(fnams, rows=rows, cols=cols, outfile=t1)
        resize(t1, width, height, outfile)
      }
    }
    val videoFile = outdir / "nadjaMontage.mp4"
    video(outdir, framerate ,videoFile)
    println("------------------------------------------------------------------")
    println(s"Created video: ${videoFile}")
  }

  def resize(infile: os.Path, width: Int, height: Int, outfile: os.Path) = {
    val geo1 = s"${width}x${height}"

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

  def video(indir: os.Path, frameRate: Int, outfile: os.Path) = {
    val inpattern = indir / "*.jpg"
    val cmd = List(
      "ffmpeg",
      "-y",
      "-framerate",
      frameRate.toString(), 
      "-pattern_type",
      "glob", 
      "-i", 
      inpattern.toString(), 
      "-c:v",
      "libx264", 
      "-pix_fmt", 
      "yuv420p",
    ) ++
      List(
        s"${outfile}",
    )
    println(cmd.mkString(" \\\n"))
    Util.exe(cmd)
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

    val files = os.list(indir)
      .filter(p => os.isFile(p) && imageExts.contains(p.ext.toLowerCase()))
    if files.isEmpty then println(s"WARNING:  Found no files in ${indir}")
    files.foreach(rs(_))
  }

  def montage(infiles: Iterable[os.Path], rows: Int, cols: Int, outfile: os.Path) = {
    val size = Util.sizeFromRows(rows) 
    val tilesgeo = s"${cols}x${rows}"
    val fnams = infiles
          .take(rows * cols)
          .toList
          .map(_.toString())
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


  def fienamesContinually(base: NBase, chars: List[NChar]): LazyList[os.Path] = {
    val cs = chars
      .flatMap {c =>  base.files.filter{f => f.char == c}}
      .map(f => Util.path(f, base.path))
    if cs.isEmpty then throw IllegalArgumentException(s"found no files in ${base.path}")
    LazyList.continually(cs).flatten
  }

}

object Util {

  def nChar(c: String): NChar = {
    c match {
      case "N" => NChar.N
      case "A" => NChar.A
      case "D" => NChar.D
      case "J" => NChar.J
      case "." => NChar.CBL
      case _ => throw IllegalArgumentException(s"Unknown character '${c}' for creating NChar")
    }
  }

  def colsFromRows(rows: Int): Int = {
    rows match {
      case 1 => 5
      case 2 => 6
      case 3 => 7
      case 4 => 8
      case _ => (rows * 1.8).toInt
    }
  }

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

