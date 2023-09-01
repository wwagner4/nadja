import scala.sys.process.Process

object Util {

  def video(indir: os.Path, frameRate: Int, width: Int, height: Int, outfile: os.Path, ext: String = "jpg") = {
    val inpattern = indir / s"*.${ext}"
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
    exe(cmd)
  }

  def createBase(path: os.Path): NBase = {

    val files = os.list(path)
      .toList
      .map(p => p.last)
      .flatMap(fn => parseFilename(fn))
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
      val c = b(1)
      Some(NFilename(b(0), c, a(1)))
    catch
      case _: Exception => {
        println(s"No image file ${filename}")
        None
      }
  }

  def path(nfn: NFilename, root: os.Path): os.Path = {
    val x = s"${nfn.name}_${nfn.id}.${nfn.ext}"
    root / x
  }

  def patternToFilenameId(pattern: String): String = {
    if pattern.size == 0 then throw IllegalStateException("Pattern character must not be empty")
    if pattern.size > 1 then throw IllegalStateException(s"Pattern string must be one character. ${pattern} is illegal")
    pattern match {
      case "." => "CBL"
      case a => a
    }
  }

  def patternToCanvas(pattern: String): NCanvas = {
    val theRows = pattern.split("\\n")
    val cols = theRows.map(_.size).min
    val rows = theRows.size
    val ids = pattern
      .filter(c => c != '\n')
      .map(c => patternToFilenameId(s"${c}"))
    NCanvas(rows, cols, ids)
  }


}
