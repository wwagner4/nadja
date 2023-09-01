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


object Main {

  @main def mainEntryPoint: Unit = {
    val names = List(
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
    val names1 = List(
      "lineal",
    )

    for name <- names do {
      mainPuls(name)
      Magick.mainMontage(name)
      //Magick.mainSwipe(name)
      //Magick.mainPulse(name)
    }

  }

  enum Alignment {
    case START, CENTER, END
  }

  def mainPuls(name: String) = {

    case class PulsConfig(
                           id: String,
                           width: Int,
                           height: Int,
                           frameCount: Int,
                           frameRate: Int,
                           popFactor: Double

                         )

    val config = PulsConfig("01", 2000, 1500, 50, 10, 0.4)

    println(s"Creating pulse for ${name}")

    // val rootdir = os.pwd / "src" / "test" / "resources" / name
    val rootdir = os.home / "work" / "nadja" / "name_chars" / name
    val outfile = os.home / "work" / "nadja" / "out" / s"pulse-${name}-${config.id}.mp4"
    val t1 = os.temp.dir()

    println(s"rootdir : ${rootdir}")
    println(s"outfile : ${outfile}")

    val base = Util.createBase(rootdir)
    val images = base.files.map { fn =>
      createNImage(base, fn)
    }

    val pattern = "NADJA"
    val nCanvas = Util.patternToCanvas(pattern)
    val patternImages = nCanvas.ids.flatMap { id => images.filter(i => i.filename.id == id) }.toSeq


    for i <- (0 to config.frameCount) do {
      val canvas = createCanvas(config.width, config.height)
      val imgWidth = canvas.getWidth() / patternImages.size
      for (img, j) <- patternImages.zipWithIndex do {
        //val pop = if (i + j) % 10 == 0 then imgWidth * config.popFactor else 0
        val pop = if Random.nextDouble() > 0.8 then imgWidth * config.popFactor else 0
        val scaled = scaleImage(img, imgWidth + pop)
        val x = j.toDouble / patternImages.size
        val y = 0.5
        drawImage(canvas, scaled, x, y, xAlign = Alignment.START)
      }
      val fileName = f"a_${i}%05d.jpg"
      val file = t1 / fileName
      val f1 = file.toIO
      val r = ImageIO.write(canvas, "jpg", f1)
      println(s"wrote to ${f1} ${r}")
    }
    Util.video(t1, 8, config.width, config.height, outfile)
    println(s"Wrote video to ${outfile}")
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