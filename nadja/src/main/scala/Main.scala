import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.awt.image.AffineTransformOp
import javax.imageio.ImageIO
import scala.collection.Iterable
import sys.process.Process
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

case class Canvas(
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
    val names1 = List(
      "coverall",
      "lineal",
      "m",
      "mandel",
      "monterey",
      "pampelmuse",
      "pilze",
      "shoes",
    )
    val names = List(
      "lineal",
    )

    for name <- names do {
      mainTryout(name)
      //Magick.mainMontage(name)
      //Magick.mainSwipe(name)
      //Magick.mainPulse(name)
    }

  }


  def mainTryout(name: String) = {

    case class Config(
                       id: String,
                       width: Int,
                       height: Int,
                     )

    val config = Config("00", 2000, 2000)

    println(s"Creating pulse for ${name}")

    // val rootdir = os.pwd / "src" / "test" / "resources" / name
    val rootdir = os.home / "work" / "nadja" / "name_chars" / name
    val outfile = os.home / "work" / "nadja" / "out" / s"pulse-${name}-${config.id}.mp4"
    val t1 = os.home / "work" / "nadja" / "out" / "t1"
    os.makeDir.all(t1)

    println(s"rootdir : ${rootdir}")
    println(s"outfile : ${outfile}")

    val base = Util.createBase(rootdir)
    val images = base.files.map { fn =>
      createNImage(base, fn)
    }

    for i <- (0 to 10) do {
      val canvas = createCanvas(config.width, config.height)
      val image = Random.shuffle(images).head
      val scaled = scaleImage(image, 400.0 + Random.nextDouble() * 200)
      drawImage(canvas, scaled, Random.nextDouble(), Random.nextDouble())
      val fileName = f"a_${i}%05d.jpg"
      val file = t1 / fileName
      val f1 = file.toIO
      val r = ImageIO.write(canvas, "jpg", f1)
      println(s"wrote to ${f1} ${r}")
    }
    // Util.video(t1, 10, config.width, config.height, outfile)
    //println(s"Wrote video to ${outfile}")
  }

  def scaleImage(image: NImage, width: Double): BufferedImage = {
    val iw = image.bufferedImage.getWidth()
    val _fw = width / iw
    val ih = image.bufferedImage.getHeight()
    println(s":: scale: ${width} ${_fw}")
    val at = AffineTransform()
    at.scale(_fw, _fw)
    val op = AffineTransformOp(at, AffineTransformOp.TYPE_BICUBIC)
    val dest = BufferedImage((iw * _fw).toInt, (ih * _fw).toInt, image.bufferedImage.getType)
    op.filter(image.bufferedImage, dest)
    dest
  }

  def drawImage(canvas: BufferedImage, image: BufferedImage, xpos: Double, ypos: Double) = {
    val cw = canvas.getWidth()
    val x1 = cw * xpos
    val ch = canvas.getHeight()
    val y1 = ch * ypos
    val x = x1 - image.getWidth() / 2.0
    val y = y1 - image.getHeight() / 2.0
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