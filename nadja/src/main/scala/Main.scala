import sys.process.Process
import scala.jdk.CollectionConverters._
import scala.util.Random

enum NChar {
  case N, A, D, J, CBL
}

val nadja = List(NChar.N, NChar.A, NChar.D, NChar.J, NChar.A)

case class NBase(
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
    // mainTryout()
    // mainMontage()
    mainSwipe()
  }

  def mainTryout() = {
    val s = "ABC".map(c => s"${c}")
    println(s)

  }

  def mainSwipe() = {

    def slowFactorsIncreas(): Iterable[Int] = {
      def f(i: Int): Int = {
        if i < 1 then return 100
        if i < 20 then return 10
        if i < 40 then return 5
        if i < 60 then return 2
        return 1
      }

      LazyList.from(0)
        .map(f(_))
    }

    def applySlowDown[T](in: Iterable[T], fSlow: () => Iterable[Int]): Iterable[T] = {
      in
        .zip(fSlow())
        .flatMap((a, i) => List.fill(i)(a))
    }

    def mySlices[T](in: Seq[T], slizeLen: Int): Seq[List[T]] = {
      val maxIndex = in.size - slizeLen
      (0 to maxIndex).map(i => in.toList.slice(i, i + slizeLen))
    }

    def doSwipe(base: NBase, config: SwipeConfig, outdir: os.Path) = {
      val visible = config.pattern.map(c => s"${c}").map(Util.nChar)
      val slices = LazyList.continually(mySlices(visible, config.viewcols)).flatten.drop(config.startDrop)
      val descs = applySlowDown(slices, config.fSlow)
      descs
        .zipWithIndex
        .take(config.videoFrames)
        .foreach((desc, index) => createImage(base, desc, index, config.viewcols, config.width, config.height, outdir))


      val videoFile = outdir / "nadjaSwipe.mp4"
      video(outdir, config.videoFramerate, config.width, config.height, videoFile)
      println("------------------------------------------------------------------")
      println(s"Created video: ${videoFile}")

    }

    def createImage(base: NBase, descr: List[NChar], index: Int, viewcols: Int, width: Int, height: Int, outdir: os.Path) = {

      val zi = "%04d".format(index)
      val outfile = outdir / s"nadja_${zi}.jpg"

      def createFile(out: os.Path) = {
        val fns = fienamesContinually(base, descr).take(descr.size)
        val t1 = os.temp()
        montage(fns, rows = 1, cols = viewcols, outfile = t1)
        resize(t1, width, height, outfile)
      }

      val key = descr.mkString("")
      FileCache.save(key, outfile, createFile)
    }

    case class SwipeConfig
    (
      name: String,
      id: String,
      width: Int,
      height: Int,
      videoFrames: Int,
      videoFramerate: Int,
      pattern: String,
      viewcols: Int,
      startDrop: Int,
      fSlow: () => Iterable[Int],
    )

    val config = SwipeConfig(
      name = "monterey",
      id = "01",
      videoFrames = 500,
      videoFramerate = 40,
      width = 1000,
      height = 800,
      pattern = ".......N.A.D.J.A........",
      viewcols = 12,
      startDrop = 6,
      fSlow = slowFactorsIncreas,
    )

    // val rootdir = os.pwd / "src" / "test" / "resources" / name
    val rootdir = os.home / "work" / "nadja" / "name_chars" / config.name
    val outdir = os.home / "work" / "nadja" / "out" / "swipe" / s"${config.name}-${config.id}"
    os.makeDir.all(outdir)

    val resizedDir = os.temp.dir()
    resizeAll(rootdir, config.width, resizedDir)
    val base = Util.createBase(resizedDir)
    println(s"base: ${base}")
    doSwipe(base, config, outdir)
  }


  def mainMontage() = {

    val name = "monterey"
    val id = "13"
    val maxrows = 6
    val combis = 5
    val framerate = 5
    val width = 2000
    val height = 1500

    // val rootdir = os.home / "work" / "nadja" / name
    val rootdir = os.home / "work" / "nadja" / "name_chars" / name
    // val rootdir = os.pwd / "src" / "test" / "resources" / name
    val outdir = os.home / "work" / "nadja" / "out" / "montage" / s"${name}-${id}"
    os.makeDir.all(outdir)

    (0 until maxrows).foreach { j =>
      val rows = maxrows - j
      val cols = Util.colsFromRows(rows)
      val sizedRootDir = os.temp.dir()
      val size = Util.sizeFromRows(rows)
      resizeAll(rootdir, size, sizedRootDir)
      val base = Util.createBase(sizedRootDir)
      val infiles = fienamesContinually(base, nadja)
      (1 to combis).foreach { i =>
        val outfile = outdir / s"nadja_${(j + 1) * 100 + i}.jpg"
        val t1 = os.temp()

        val randomize = j != (maxrows - 1) || i != combis
        val fns = infiles
          .take(rows * cols)
          .toList
        val fnams = randomize match {
          case true => Random.shuffle(fns)
          case false => fns
        }

        montage(fnams, rows = rows, cols = cols, outfile = t1)
        resize(t1, width, height, outfile)
      }
    }
    val videoFile = outdir / "nadjaMontage.mp4"
    video(outdir, framerate, width, height, videoFile)
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

  def video(indir: os.Path, frameRate: Int, width: Int, height: Int, outfile: os.Path) = {
    val inpattern = indir / "*.jpg"
    val size = s"${width}x${height}"
    val cmd = List(
      "ffmpeg",
      "-y",
      "-s",
      size,
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
      "-crf",
      "25"
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
      .flatMap { c => base.files.filter { f => f.char == c } }
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
    val r = p.run()
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

