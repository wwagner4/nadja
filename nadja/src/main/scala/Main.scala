import java.awt.{Graphics2D, RenderingHints}
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import javax.imageio.{IIOException, ImageIO}
import scala.collection.Iterable
import scala.jdk.CollectionConverters.*
import scala.util.Random
import Vec.*

case class NBase(
                  path: os.Path,
                  files: List[NFilename],
                )

case class NFilename(
                      name: String,
                      id: Option[String],
                      ext: String
                    )

case class NCanvas(
                    rows: Int,
                    cols: Int,
                    ids: Iterable[String],
                  )

case class NImage(
                   filename: NFilename,
                   bufferedImage: BufferedImage,
                 )

enum Alignment {
  case START, CENTER, END
}

case class NImagePosition(
                           image: NImage,
                           x: Double,
                           y: Double,
                           width: Double,
                           align: Alignment,
                         )

object Main {

  @main def mainEntryPoint: Unit = {

    //val baseDir = os.pwd / "src" / "test" / "resources"
    val baseDir = os.home / "work" / "nadja" / "name_chars"


    val names = List(
      //"darlingkarte",
      //"coverall",
      //"eisen",
      //"farn",
      //"gold",
      //"lineal",
      //"m",
      //"mandel",
      //"lovetree",
      //"mill",
      //"monterey",
      //"pampelmuse",
      //"paris",
      //"pilze",
      //"schatten",
      // "shoes",
      // "yellow",
      "60",
      // "mix1",
      // "mix2",
    )
    for name <- names do {
      // Magick.mainSwipe(name, baseDir)
      // Magick.mainMontage(name, baseDir)
      // Magick.mainPulse(name, baseDir, true)
      // mainCreep(name, baseDir)
      // mainPuls(name, baseDir)
      mainNova(name, baseDir, true)
      // mainExplode(name, baseDir)
    }

    //mainTryout()

  }

  def mainTryout() = {
    println("main tryout")
    tryoutVec()
  }

  private def mainNova(name: String, baseDir: os.Path, playVideo: Boolean = false) = {

    case class NovaConfig(
                           id: String,
                           width: Int,
                           height: Int,
                           frameCount: Int,
                           frameRate: Int
                         )

    val config = NovaConfig(
      id = "11",
      width = 2000,
      height = 1200,
      frameCount = 300,
      frameRate = 20,
    )

    println(s"nova ${name}")
    val rootdir = baseDir / name
    val outfile = os.home / "work" / "nadja" / "out" / s"nova-${name}-${config.id}.mp4"
    val t1 = os.temp.dir()

    val base = Util.createBase(rootdir)
    val fillFactor = if base.files.size > 10 then 15 else 25
    val images = base.files
      .filter(f => f.id.map(_ != "CBL").getOrElse(true))
      .map { fn => ImageIoUtil.createNImage(base, fn) }
      .flatMap(i => List.fill(fillFactor)(i))

    val centers = Seq(
      Vec(1.5, 0.7),
      Vec(0.2, 1.2),
      Vec(0.5, 0.5),
    )

    val positions = centers.flatMap { center =>
      novaPositions(Random.shuffle(images), (config.frameCount.toDouble / centers.size).toInt, center, false).toList
    }
    for (ip, i) <- positions.zipWithIndex do {
      val canvas = ImageIoUtil.createCanvas(config.width, config.height)
      ImageIoUtil.createImageFile(canvas, ip, i, t1)
    }
    Util.video(t1, config.frameRate, config.width, config.height, outfile)
    println(s"Wrote video to ${outfile}")
    if playVideo then Util.exe(List("mpv", s"${outfile}"))
  }

  private def novaPositions(images: Iterable[NImage], frameCount: Int, center: Vec, endless: Boolean): Iterable[Iterable[NImagePosition]] = {

    def fBase(base: Vec, offset: Vec, speed: Double)(x: Double): Vec = base.add(offset.mul(x * 0.06 * speed))

    def fWidth(x: Double): Double = 0.001 + x * 0.09 / frameCount

    def ranPos(): Double = 0.1 + Random.nextDouble() * 0.8

    def ranSpeed(): Double = 0.01 + (Random.nextDouble() * 1.5)

    val offset = vecToPol(Vec(0.5, 0.5).sub(center))

    println(s"-- positions center:$center frameCount:$frameCount center:$center offset:$offset")
    val vecs = Random.shuffle(skewCircle(offset, 0.6, images.size))

    val fs = vecs.map(v => fBase(center, v, ranSpeed())).toSeq

    for i <- (0 until frameCount) yield {
      for (img, j) <- images.zipWithIndex yield {
        val width = fWidth(i)
        val pos = fs(j)(i)
        val x = if endless then ImageIoUtil.thorus(pos.x) else pos.x
        val y = if endless then ImageIoUtil.thorus(pos.y) else pos.y
        val align = Alignment.CENTER
        NImagePosition(img, x, y, width, align)
      }
    }
  }

  private def mainCreep(name: String, baseDir: os.Path, playVideo: Boolean = false) = {

    case class CreepConfig(
                            id: String,
                            width: Int,
                            height: Int,
                            frameCount: Int,
                            frameRate: Int
                          )

    val config = CreepConfig(
      id = "00",
      width = 2000,
      height = 1200,
      frameCount = 300,
      frameRate = 25,
    )

    println(s"creep ${name}")
    val rootdir = baseDir / name
    val outfile = os.home / "work" / "nadja" / "out" / s"creep-${name}-${config.id}.mp4"
    val t1 = os.temp.dir()

    val base = Util.createBase(rootdir)
    val images = base.files
      .filter(f => f.id.map(_ != "CBL").getOrElse(true))
      .map { fn => ImageIoUtil.createNImage(base, fn) }
      .flatMap(i => List.fill(5)(i))
    if images.isEmpty then {
      println("Found no named images")
    } else {
      val shuffeled = Random.shuffle(images)

      val positions = ImageIoUtil.creepPositions(shuffeled, config.frameCount, true).toList
      for (ip, i) <- positions.zipWithIndex do {
        val canvas = ImageIoUtil.createCanvas(config.width, config.height)
        ImageIoUtil.createImageFile(canvas, ip, i, t1)
      }
      Util.video(t1, config.frameRate, config.width, config.height, outfile)
      println(s"Wrote video to ${outfile}")
      if playVideo then Util.exe(List("mpv", s"${outfile}"))
    }
  }

  private def mainPuls(name: String, baseDir: os.Path, playVideo: Boolean = false) = {

    case class PulsConfig(
                           id: String,
                           width: Int,
                           height: Int,
                           frameCount: Int,
                           frameRate: Int,
                           popFactor: Double,
                           popFrameCount: Int,
                         )

    val config = PulsConfig("01", 2000, 1500,
      frameCount = 200,
      frameRate = 20,
      popFactor = 1.1,
      popFrameCount = 10
    )

    println(s"Creating pulse for ${name}")

    val outfile = os.home / "work" / "nadja" / "out" / s"pulse-${name}-${config.id}.mp4"
    val t1 = os.temp.dir()
    val rootDir = baseDir / name

    println(s"rootdir : ${rootDir}")
    println(s"outfile : ${outfile}")

    val base = Util.createBase(rootDir)
    val images = base.files.map { fn =>
      ImageIoUtil.createNImage(base, fn)
    }

    val pattern = "NADJA"
    val nCanvas = Util.patternToCanvas(pattern)
    val patternImages = nCanvas.ids.flatMap { id => images.filter(i => i.filename.id.map(_ == id).getOrElse(false)) }.toSeq
    if patternImages.isEmpty then {
      println(s"No named images found in ${name} for ${pattern}")
    } else {
      val positions: Iterable[Iterable[NImagePosition]] = ImageIoUtil.pulsePositions(patternImages, config.frameCount, config.popFactor, config.popFrameCount)
      for (ip, i) <- positions.zipWithIndex do {
        val canvas = ImageIoUtil.createCanvas(config.width, config.height)
        ImageIoUtil.createImageFile(canvas, ip, i, t1)
      }

      Util.video(t1, config.frameRate, config.width, config.height, outfile)
      println(s"Wrote video to ${outfile}")
      if playVideo then Util.exe(List("mpv", s"${outfile}"))
    }

  }

}

def mainExplode(name: String, baseDir: os.Path, playVideo: Boolean = false) = {

  case class ExplodeConfig(
                            id: String,
                            width: Int,
                            height: Int,
                            frameCount: Int,
                            framesToLeave: Int,
                            frameRate: Int,
                          )

  val config = ExplodeConfig("02", 2000, 1500,
    frameCount = 200,
    framesToLeave = 20,
    frameRate = 15
  )

  println(s"Creating explode for ${name}")

  val rootdir = baseDir / name
  val outfile = os.home / "work" / "nadja" / "out" / s"explode-${name}-${config.id}.mp4"
  val t1 = os.temp.dir()

  println(s"rootdir : ${rootdir}")
  println(s"outfile : ${outfile}")

  val base = Util.createBase(rootdir)
  val images = base.files.map { fn =>
    ImageIoUtil.createNImage(base, fn)
  }

  val pattern = "NADJA"
  val nCanvas = Util.patternToCanvas(pattern)
  val patternImages = nCanvas.ids.flatMap { id => images.filter(i => i.filename.id.map(_ == id).getOrElse(false)) }.toSeq
  if patternImages.isEmpty then {
    println(s"Found no named images in ${name}")
  } else {
    val f1 = (config.frameCount * 0.2).toInt
    val f2 = (config.frameCount * 0.3).toInt
    val f3 = (config.frameCount * 0.5).toInt
    val p1 = ImageIoUtil.explodePositions(patternImages, f1, config.framesToLeave, false).toList.reverse
    val p2 = ImageIoUtil.pulsePositions(patternImages, f2, 1.1, 10).toList
    val p3 = ImageIoUtil.explodePositions(patternImages, f3, config.framesToLeave, true).toList
    val positions = p1 ++ p2 ++ p3
    for (ip, i) <- positions.zipWithIndex do {
      val canvas = ImageIoUtil.createCanvas(config.width, config.height)
      ImageIoUtil.createImageFile(canvas, ip, i, t1)
    }

    Util.video(t1, config.frameRate, config.width, config.height, outfile)
    println(s"Wrote video to ${outfile}")
    if playVideo then Util.exe(List("mpv", s"${outfile}"))
  }
}

object ImageIoUtil {

  def thorus(x: Double): Double = {
    if x < -0.2 then thorus(x + 1.4)
    else if x > 1.2 then thorus(x - 1.4)
    else x
  }

  def creepPositions(images: Iterable[NImage], frameCount: Int, endless: Boolean): Iterable[Iterable[NImagePosition]] = {

    def fBase(a: Double, k: Double)(x: Double): Double = a + x * k

    def fSizeBase(p: Double)(x: Double): Double = {
      val a = 0.08
      a + 0.01 + a * math.sin((x * 0.1) + p)
    }

    val fxSeq: Seq[Double => Double] = (0 until images.size).map { i => fBase(-0.2, 0.008 + Random.nextDouble * 0.003) }
    val fySeq: Seq[Double => Double] = (0 until images.size).map { i => fBase(Random.nextDouble(), 0.0) }
    val fSizeSeq: Seq[Double => Double] = (0 until images.size).map { i => fSizeBase(Random.nextDouble() * 3.14) }
    for i <- (0 to frameCount) yield {
      for (img, j) <- images.zipWithIndex yield {
        val width = fSizeSeq(j)(i)
        val x = if endless then thorus(fxSeq(j)(i)) else fxSeq(j)(i)
        val y = if endless then thorus(fySeq(j)(i)) else fySeq(j)(i)
        val align = Alignment.START
        NImagePosition(img, x, y, width, align)
      }
    }
  }

  def explodePositions(images: Iterable[NImage], frameCount: Int, framesToLeave: Double, endless: Boolean): Iterable[Iterable[NImagePosition]] = {

    def fBase(a: Double, k: Double)(x: Double): Double = a + x * k

    def randomK(): Double = {
      val k1 = if Random.nextBoolean()
      then framesToLeave - Random.nextDouble() * framesToLeave * 0.5
      else Random.nextDouble() * framesToLeave * 0.5 - framesToLeave
      1.0 / k1
    }

    val fySeq: Seq[Double => Double] = (0 until images.size).map { i => fBase(0.5, randomK()) }
    val fxSeq: Seq[Double => Double] = (0 until images.size).map { i => fBase(i.toDouble / images.size, randomK()) }
    for i <- (0 to frameCount) yield {
      for (img, j) <- images.zipWithIndex yield {
        val width = 1.0 / images.size
        val x = if endless then thorus(fxSeq(j)(i)) else fxSeq(j)(i)
        val y = if endless then thorus(fySeq(j)(i)) else fySeq(j)(i)
        val align = Alignment.START
        NImagePosition(img, x, y, width, align)
      }
    }
  }

  def pulsePositions(images: Iterable[NImage], frameCount: Int, popFactor: Double, popFrameCount: Int): Iterable[Iterable[NImagePosition]] = {
    for i <- (0 to frameCount / popFrameCount) yield {
      val positions = for (img, j) <- images.zipWithIndex yield {
        val width = (if Random.nextDouble() > 0.8 then popFactor else 1.0) / images.size
        //val w0 = (if (i + j) % 10 == 0 then config.popFactor else 1.0) / patternImages.size
        val x = j.toDouble / images.size
        val y = 0.5
        val align = Alignment.START
        NImagePosition(img, x, y, width, align)
      }
      List.fill(popFrameCount)(positions)
    }.flatten
  }

  def createImageFile(canvas: BufferedImage, positions: Iterable[NImagePosition], index: Int, outDir: os.Path) = {
    for pos <- positions do {
      val imgWidth = canvas.getWidth() * pos.width
      val scaled = scaleImage(pos.image, imgWidth)
      drawImage(canvas, scaled, pos.x, pos.y, pos.align)
    }
    val fileName = f"a_${index}%05d.jpg"
    val file = outDir / fileName
    val writeResult = ImageIO.write(canvas, "jpg", file.toIO)
    if !writeResult then throw IllegalStateException(s"Could not write image file ${file}")
    println(s"wrote to ${file}")
  }

  private def scaleImage(image: NImage, width: Double): BufferedImage = {
    val iw = image.bufferedImage.getWidth()
    val _fw = width / iw
    val ih = image.bufferedImage.getHeight()
    val at = AffineTransform()
    at.scale(_fw, _fw)
    val op = AffineTransformOp(at, AffineTransformOp.TYPE_BICUBIC)
    val dest = BufferedImage((iw * _fw).toInt, (ih * _fw).toInt, image.bufferedImage.getType)
    dest.getGraphics.asInstanceOf[Graphics2D]
      .setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
    op.filter(image.bufferedImage, dest)
    dest
  }

  private def drawImage(
                         canvas: BufferedImage,
                         image: BufferedImage,
                         xpos: Double,
                         ypos: Double,
                         xAlign: Alignment = Alignment.CENTER,
                         yAlign: Alignment = Alignment.CENTER,
                       ) = {
    val cw = canvas.getWidth()
    val x1 = cw * xpos
    val ch = canvas.getHeight()
    val y1 = ch * ypos
    val x = xAlign match {
      case Alignment.START => x1
      case Alignment.CENTER => x1 - image.getWidth() / 2.0
      case Alignment.END => x1 - image.getWidth()
    }
    val y = yAlign match {
      case Alignment.START => y1
      case Alignment.CENTER => y1 - image.getHeight() / 2.0
      case Alignment.END => y1 - image.getHeight()
    }
    val graphics = canvas.getGraphics.asInstanceOf[Graphics2D]
    graphics.drawImage(image, x.toInt, y.toInt, null)
    graphics.dispose()
  }


  def createCanvas(width: Int, height: Int): BufferedImage = {
    BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
  }

  def createNImage(base: NBase, filename: NFilename): NImage = {
    val p = Util.path(filename, base.path)
    try {
      val bi = ImageIO.read(p.toIO)
      NImage(filename, bi)
    } catch {
      case e: IIOException => throw IllegalStateException(s"Error reading image file '${p}'. ${e.getMessage}")
    }
  }
}