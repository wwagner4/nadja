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
      mainTryout()
      //Magick.mainMontage(name)
      //Magick.mainSwipe(name)
      //Magick.mainPulse(name)
    }

  }

  def mainTryout() = {
    val s = "ABC".map(c => s"${c}")
    println(s)

  }


}

