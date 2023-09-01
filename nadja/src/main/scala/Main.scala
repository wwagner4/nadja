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

    for i <- (0 to 100) do {
      val canvas = createCanvas(config.width, config.height)
      scaleAndDrawImage(canvas,
        Random.shuffle(images).head,
        Random.nextDouble(),
        Random.nextDouble(),
        0.2)
      val fileName = f"a_${i}%05d.jpg"
      val file = t1 / fileName
      val f1 = file.toIO
      val r = ImageIO.write(canvas, "jpg", f1)
      println(s"wrote to ${f1} ${r}")
    }
    Util.video(t1, 10, config.width, config.height, outfile)
    println(s"Wrote video to ${outfile}")
  }

  def scaleAndDrawImage(canvas: BufferedImage, image: NImage, xpos: Double, ypos: Double, size: Double) = {
    val at = AffineTransform()
    val _fw = (canvas.getWidth() * size) / image.bufferedImage.getWidth()
    val _xpos = canvas.getWidth() * (xpos * size)
    val _ypos = canvas.getHeight() * (ypos * size)
    println(s"scale to ${_fw} pos: ${_xpos} ${_ypos}")
    at.scale(_fw, _fw)
    val op = AffineTransformOp(at, AffineTransformOp.TYPE_BICUBIC)
    val dest = BufferedImage(image.bufferedImage.getWidth(), image.bufferedImage.getHeight(), image.bufferedImage.getType)
    op.filter(image.bufferedImage, dest)
    val graphics = canvas.getGraphics.asInstanceOf[Graphics2D]
    graphics.drawImage(dest, _xpos.toInt, _ypos.toInt, null)
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

