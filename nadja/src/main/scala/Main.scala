import sys.process.Process
import scala.jdk.CollectionConverters._
import scala.util.Random

enum NChar {
  case N, A, D, J, CBL
}

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
    val names = List(
      "coverall",
      "lineal",
      "m",
      "mandel",
      "monterey",
      "pampelmuse",
      "pilze",
      "shoes",
    )

    for name <- names do {
      // mainTryout()
      //mainMontage(name)
      mainSwipe(name)
    }

  }

  def mainTryout() = {
    val s = "ABC".map(c => s"${c}")
    println(s)

  }

  def mainSwipe(name: String) = {

    def slowFactorsIncreas(): Iterable[Int] = {
      def f(i: Int): Int = {
        if i < 1 then return 100
        if i < 10 then return 10
        if i < 20 then return 5
        if i < 30 then return 2
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

    def doSwipe(base: NBase, config: SwipeConfig, outfile: os.Path) = {
      val visible = config.pattern.map(c => s"${c}").map(Util.nChar)
      val slices = LazyList.continually(mySlices(visible, config.viewcols)).flatten.drop(config.startDrop)
      val descs = applySlowDown(slices, config.fSlow)
      val t1 = os.temp.dir()
      descs
        .zipWithIndex
        .take(config.videoFrames)
        .foreach((desc, index) => createImage(base, desc, (config.videoFrames - 1) - index, config.viewcols, config.width, config.height, t1))

      video(t1, config.videoFramerate, config.width, config.height, outfile)
      println("------------------------------------------------------------------")
      println(s"Created video: ${outfile}")

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

      val key = name + descr.mkString("")
      FileCache.save(key, outfile, createFile)
    }

    case class SwipeConfig
    (
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

    // ########### SWIPE ###########
    val config = SwipeConfig(
      id = "10",
      videoFrames = 500,
      videoFramerate = 20,
      width = 1000,
      height = 800,
      pattern = ".NADJA.NADJA.",
      viewcols = 7,
      startDrop = 0,
      fSlow = slowFactorsIncreas,
    )
    val config1 = SwipeConfig(
      id = "00",
      videoFrames = 800,
      videoFramerate = 30,
      width = 1000,
      height = 800,
      pattern = ".....NADJA.....",
      viewcols = 5,
      startDrop = 5,
      fSlow = slowFactorsIncreas,
    )

    // val rootdir = os.pwd / "src" / "test" / "resources" / name
    val rootdir = os.home / "work" / "nadja" / "name_chars" / name
    val outfile = os.home / "work" / "nadja" / "out" / s"swipe-${name}-${config.id}.mp4"

    val resizedDir = os.temp.dir()
    resizeAll(rootdir, config.width, resizedDir)
    val base = Util.createBase(resizedDir)
    println(s"base: ${base}")
    doSwipe(base, config, outfile)
  }


  def mainMontage(name: String) = {

    case class MontageConfig(
                              id: String,
                              width: Int,
                              height: Int,
                              pattern: String,
                              maxrows: Int,
                              combis: Int,
                              framerate: Int,
                              lastImageCount: Int
                            )
    // ########### MONTAGE ###########

    val config = MontageConfig(
      id = "01",
      width = 2000,
      height = 1500,
      pattern = "NADJA",
      maxrows = 8,
      combis = 7,
      framerate = 8,
      lastImageCount = 50
    )


    // val rootdir = os.home / "work" / "nadja" / name
    val rootdir = os.home / "work" / "nadja" / "name_chars" / name
    // val rootdir = os.pwd / "src" / "test" / "resources" / name
    val videofile = os.home / "work" / "nadja" / "out" / s"montage-${name}-${config.id}.mp4"

    def idxStr(i: Int): String = "%06d".format(i)

    val nchars = config.pattern.map(c => s"${c}").map(Util.nChar)
    var lastInImages = List.empty[os.Path]
    var lastIndex = 0
    var lastRows = 0
    var lastCols = 0
    val framesdir = os.temp.dir()
    (0 until config.maxrows).foreach { j =>
      val rows = config.maxrows - j
      val cols = Util.colsFromRows(rows)
      val sizedRootDir = os.temp.dir()
      val size = Util.sizeFromRows(rows)
      resizeAll(rootdir, size, sizedRootDir)
      val base = Util.createBase(sizedRootDir)
      val infiles = fienamesContinually(base, nchars)
      (1 to config.combis).foreach { i =>
        val idx = (j + 1) * 100 + i
        val outfile = framesdir / s"nadja_${idxStr(idx)}.jpg"
        val t1 = os.temp()
        val fns = infiles
          .take(rows * cols)
          .toList
        val fnams = j != (config.maxrows - 1) || i != config.combis match {
          case true => Random.shuffle(fns)
          case false => {
            lastInImages = fns
            lastIndex = idx
            lastCols = cols
            lastRows = rows
            fns
          }
        }
        montage(fnams, rows = rows, cols = cols, outfile = t1)
        resize(t1, config.width, config.height, outfile)
      }
    }
    (1 to config.lastImageCount).foreach { i =>
      val outfile = framesdir / s"nadja_${idxStr(lastIndex + i)}.jpg"

      def createFile(out: os.Path) = {
        val t2 = os.temp()
        montage(lastInImages, rows = lastRows, cols = lastCols, outfile = t2)
        resize(t2, config.width, config.height, out)
      }

      FileCache.save(name, outfile, createFile)
    }

    video(framesdir, config.framerate, config.width, config.height, videofile)
    println("------------------------------------------------------------------")
    println(s"Created video: ${videofile}")
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


  def fienamesContinually(base: NBase, chars: Seq[NChar]): LazyList[os.Path] = {
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

