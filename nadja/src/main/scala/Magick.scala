import scala.sys.process.Process
import scala.util.Random
import sys.process.Process

object Magick {


  def mainTryoutResize(name: String) = {
    println(s"Creating pulse for ${name}")

    // val rootdir = os.pwd / "src" / "test" / "resources" / name
    val rootdir = os.home / "work" / "nadja" / "name_chars" / name
    val outfile = os.home / "work" / "nadja" / "out" / s"pulse-${name}-00.mp4"

    val outdir = os.home / "work" / "nadja" / "tmp"
    os.makeDir.all(outdir)

    os.list(rootdir).foreach(f => {
      println(s"resize ${f}")
      val f1 = f.last
      val outfile = outdir / f1
      println(outfile)
      val w = 500
      val h = 1000
      resize(f, 100, 1000, outfile)
    })
  }

  def mainPulse(name: String) = {

    case class PulseConfig
    (
      id: String,
      width: Int,
      height: Int,
      frameCount: Int
    )

    val config = PulseConfig(
      id = "00",
      width = 1200,
      height = 900,
      frameCount = 100
    )

    println(s"Creating pulse for ${name}")

    // val rootdir = os.pwd / "src" / "test" / "resources" / name
    val rootdir = os.home / "work" / "nadja" / "name_chars" / name
    val outfile = os.home / "work" / "nadja" / "out" / s"pulse-${name}-${config.id}.mp4"

    val pattern =
      """.......
        |.NADJA.
        |.......
        |""".stripMargin

    val canvas = Util.patternToCanvas(pattern)

    val bases = List(1.0, 0.7, 0.5, 0.3)
      .map { sf =>
        val resizedDir = os.temp.dir()
        val w = MagickUtil.roundToEven(config.width / canvas.cols)
        val h = MagickUtil.roundToEven(w / sf)
        os.list(rootdir)
          .filter(f => os.isFile(f))
          .foreach(f => {
            val resizedFile = resizedDir / f.last
            resize(f, w, h, resizedFile)
          })
        Util.createBase(resizedDir)
      }
    val frameImagesDir = os.temp.dir()
    for i <- (0 until config.frameCount) do {
      val basesList = LazyList.continually(bases).flatten.take(canvas.rows * canvas.cols).toList
      val rBases = Random.shuffle(basesList)
      val filenames = canvas.ids.zip(rBases).map { (id, base) =>
        MagickUtil.filenameFromBase(base, id)
      }
      val cnt_str = "%05d".format(i)
      val outfile = frameImagesDir / s"frame_${cnt_str}.jpg"
      montage(filenames, canvas.rows, canvas.cols, outfile)
    }
    Util.video(frameImagesDir, 10, config.width, config.height, outfile, ext = "jpg")
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
      val visible = config.pattern.map(c => s"${c}").map(Util.patternToFilenameId)
      val slices = LazyList.continually(mySlices(visible, config.viewcols)).flatten.drop(config.startDrop)
      val descs = applySlowDown(slices, config.fSlow)
      val t1 = os.temp.dir()
      descs
        .zipWithIndex
        .take(config.videoFrames)
        .foreach((desc, index) => createImage(base, desc, (config.videoFrames - 1) - index, config.viewcols, config.width, config.height, t1))

      Util.video(t1, config.videoFramerate, config.width, config.height, outfile)
      println("------------------------------------------------------------------")
      println(s"Created video: ${outfile}")

    }

    def createImage(base: NBase, descr: List[String], index: Int, viewcols: Int, width: Int, height: Int, outdir: os.Path) = {

      val zi = "%04d".format(index)
      val outfile = outdir / s"nadja_${zi}.jpg"

      def createFile(out: os.Path) = {
        val fns = MagickUtil.fienamesContinually(base, descr).take(descr.size)
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
    val config1 = SwipeConfig(
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
    val config = SwipeConfig(
      id = "01",
      videoFrames = 400,
      videoFramerate = 30,
      width = 600,
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
      id = "02",
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

    val filenameIds = config.pattern.map(c => s"${c}").map(Util.patternToFilenameId)
    var lastInImages = List.empty[os.Path]
    var lastIndex = 0
    var lastRows = 0
    var lastCols = 0
    val framesdir = os.temp.dir()
    (0 until config.maxrows).foreach { j =>
      val rows = config.maxrows - j
      val cols = MagickUtil.colsFromRows(rows)
      val sizedRootDir = os.temp.dir()
      val size = MagickUtil.sizeFromRows(rows)
      resizeAll(rootdir, size, sizedRootDir)
      val base = Util.createBase(sizedRootDir)
      val infiles = MagickUtil.fienamesContinually(base, filenameIds)
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

    Util.video(framesdir, config.framerate, config.width, config.height, videofile)
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

  val imageExts = Seq("jpg", "jpeg", "png")


  def resizeAll(indir: os.Path, size: Int, outdir: os.Path) = {

    val s = MagickUtil.roundToEven(size)
    val geo = s"${s}x${s}"

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
    val size = MagickUtil.sizeFromRows(rows)
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

}

object MagickUtil {

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

  def roundToEven(value: Double): Int = {
    val i = (value + 0.5).toInt
    i - i % 2
  }

  def fienamesContinually(base: NBase, ids: Seq[String]): LazyList[os.Path] = {
    val cs = ids
      .flatMap { c => base.files.filter { f => f.id == c } }
      .map(f => Util.path(f, base.path))
    if cs.isEmpty then throw IllegalArgumentException(s"found no files in ${base.path}")
    LazyList.continually(cs).flatten
  }

  def filenameFromBase(base: NBase, id: String): os.Path = {
    val fn = base.files.filter { f => f.id == id }.head
    Util.path(fn, base.path)
  }
}