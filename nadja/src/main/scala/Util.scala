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

  def fienamesContinually(base: NBase, ids: Seq[String]): LazyList[os.Path] = {
    val cs = ids
      .flatMap { c => base.files.filter { f => f.id == c } }
      .map(f => path(f, base.path))
    if cs.isEmpty then throw IllegalArgumentException(s"found no files in ${base.path}")
    LazyList.continually(cs).flatten
  }

  def filenameFromBase
  (base: NBase, id: String): os.Path = {
    val fn = base.files.filter { f => f.id == id }.head
    path(fn, base.path)
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


}
