import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import javax.imageio.ImageIO
import scala.collection.Iterable
import scala.jdk.CollectionConverters.*
import scala.util.Random

case class NBase(
                  path: os.Path,
                  files: List[NFilename],
                )

case class NFilename(
                      name: String,
                      id: String,
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

case class NImagePosition (
  image: NImage,
  x: Double,
  y: Double,
  width: Double,
  align: Alignment,
) 

object Main {

  @main def mainEntryPoint: Unit = {
    val names1 = List(
      //"coverall",
      //"darlingkarte",
      //"eisen",
      //"farn",
      //"gold",
      //"lineal",
      //"m",
      //"mandel",
      "lovetree",
      "mill",
      "monterey",
      "pampelmuse",
      "paris",
      "pilze",
      "schatten",
      "shoes",
      "yellow",
    )
    val names = List(
      "lineal",
    )

    for name <- names do {
      // mainPuls(name)
      mainExplode(name)
      // Magick.mainMontage(name)
      // Magick.mainSwipe(name)
      // Magick.mainPulse(name)
    }

  }

  def mainPuls(name: String) = {

    case class PulsConfig(
                           id: String,
                           width: Int,
                           height: Int,
                           frameCount: Int,
                           frameRate: Int,
                           popFactor: Double,
                         )

    val config = PulsConfig("01", 2000, 1500, 20, 10, 1.2)

    println(s"Creating pulse for ${name}")

    val rootdir = os.pwd / "src" / "test" / "resources" / name
    // val rootdir = os.home / "work" / "nadja" / "name_chars" / name
    val outfile = os.home / "work" / "nadja" / "out" / s"pulse-${name}-${config.id}.mp4"
    val t1 = os.temp.dir()

    println(s"rootdir : ${rootdir}")
    println(s"outfile : ${outfile}")

    val base = Util.createBase(rootdir)
    val images = base.files.map { fn =>
      ImageIoUtil.createNImage(base, fn)
    }

    val pattern = "NADJA"
    val nCanvas = Util.patternToCanvas(pattern)
    val patternImages = nCanvas.ids.flatMap { id => images.filter(i => i.filename.id == id) }.toSeq

    val positions: Iterable[Iterable[NImagePosition]] = ImageIoUtil.pulsePositions(patternImages, config.frameCount, config.popFactor)
    for (ip, i) <- positions.zipWithIndex do {
      val canvas = ImageIoUtil.createCanvas(config.width, config.height)
      ImageIoUtil.createImageFile(canvas, ip, i, t1)
    }

    Util.video(t1, config.frameRate, config.width, config.height, outfile)
    println(s"Wrote video to ${outfile}")
  }

}

def mainExplode(name: String) = {

  case class ExplodeConfig(
                          id: String,
                          width: Int,
                          height: Int,
                          frameCount: Int,
                          framesToLeave: Int,
                          frameRate: Int,
                        )

  val config = ExplodeConfig("02", 2000, 1500, 200, 60, 40)

  println(s"Creating explode for ${name}")

  val rootdir = os.pwd / "src" / "test" / "resources" / name
  // val rootdir = os.home / "work" / "nadja" / "name_chars" / name
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
  val patternImages = nCanvas.ids.flatMap { id => images.filter(i => i.filename.id == id) }.toSeq

  val p1 = explodePositions(patternImages, (config.frameCount * 0.5).toInt, config.framesToLeave, false).toList.reverse
  val p2 = explodePositions(patternImages, config.frameCount, config.framesToLeave, true).toList
  val positions = p1 ++ p2
  for (ip, i) <- positions.zipWithIndex do {
    val canvas = ImageIoUtil.createCanvas(config.width, config.height)
    ImageIoUtil.createImageFile(canvas, ip, i, t1)
  }

  Util.video(t1, config.frameRate, config.width, config.height, outfile)
  println(s"Wrote video to ${outfile}")

}

def explodePositions(images: Iterable[NImage], frameCount: Int, framesToLeave: Double, endless: Boolean): Iterable[Iterable[NImagePosition]] = {

  def fBase(a: Double, k: Double)(x: Double): Double = a + x * k

  def randomK(): Double = {
    val k1 = if Random.nextBoolean() 
    then framesToLeave - Random.nextDouble() * framesToLeave * 0.5 
    else Random.nextDouble() * framesToLeave * 0.5 - framesToLeave
    1.0 / k1
  }

  def thorus(x: Double): Double = {
    if x < -0.2 then thorus(x + 1.4)
    else if x > 1.2 then thorus(x - 1.4)
    else x
  }

  //val x = j.toDouble / images.size
  val fySeq: Seq[Double => Double] = (0 until images.size).map{i => fBase(0.5, randomK()) }
  val fxSeq: Seq[Double => Double] = (0 until images.size).map{i => fBase(i.toDouble / images.size, randomK()) }
  for i <- (0 to frameCount) yield {
    val positions = for (img, j) <- images.zipWithIndex yield {
      val width = 1.0 / images.size 
      val x = if endless then thorus(fxSeq(j)(i)) else fxSeq(j)(i)
      val y = if endless then thorus(fxSeq(j)(i)) else fySeq(j)(i)
      val align = Alignment.START
      println(s"Created image image ${j} y:${y}")
      NImagePosition(img, x, y,width, align)
    }
    println(s"Created positions for image ${i}")
    positions
  }
}

object ImageIoUtil {

  def pulsePositions(images: Iterable[NImage], frameCount: Int, popFactor: Double): Iterable[Iterable[NImagePosition]] = {
    for i <- (0 to frameCount) yield {
      val positions = for (img, j) <- images.zipWithIndex yield {
        val width = (if Random.nextDouble() > 0.8 then popFactor else 1.0) / images.size 
        //val w0 = (if (i + j) % 10 == 0 then config.popFactor else 1.0) / patternImages.size 
        val x = j.toDouble / images.size
        val y = 0.5
        val align = Alignment.START
        NImagePosition(img, x, y,width, align)
      }
      println(s"Created positions for image ${i}")
      positions
    }
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

  def scaleImage(image: NImage, width: Double): BufferedImage = {
    val iw = image.bufferedImage.getWidth()
    val _fw = width / iw
    val ih = image.bufferedImage.getHeight()
    val at = AffineTransform()
    at.scale(_fw, _fw)
    val op = AffineTransformOp(at, AffineTransformOp.TYPE_BICUBIC)
    val dest = BufferedImage((iw * _fw).toInt, (ih * _fw).toInt, image.bufferedImage.getType)
    op.filter(image.bufferedImage, dest)
    dest
  }

  def drawImage(
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
    val bi = ImageIO.read(p.toIO)
    NImage(filename, bi)
  }
}