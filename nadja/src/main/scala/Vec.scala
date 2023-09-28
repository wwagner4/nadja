object Vec {

    trait Showable {

        protected def asVec(): Iterable[Double]
        protected def nam(): String

        override def toString(): String = {
            asVec()
                .map("%.3f".format(_))
                .mkString(s"${nam()}[", "|", "]")
        }
    }


    case class Vec(x: Double, y: Double) extends Showable {
        def asVec() = Seq(x, y) 
        def nam() = "V" 
        def mul(v: Double) = Vec(x * v, y * v)
        def add(v: Vec) = Vec(x + v.x, y + v.y)
        def sub(v: Vec) = Vec(x - v.x, y - v.y)
        def add(p: Pol) = {
            val v = polToVec(p)
            Vec(x + v.x, y + v.y)
        }
        def sub(p: Pol) = {
            val v = polToVec(p)
            Vec(x - v.x, y - v.y)
        }
    }
    case class Pol(r: Double, alpha: Double) extends Showable {
        def asVec() = Seq(r, alpha, radToDeg(alpha)) 
        def nam() = "P" 
        def inverse(): Pol = Pol(r * (-1), alpha)
    }

    private def pos(v: Double): Double = {
        if v > (2 * math.Pi) then pos(v - (2 * math.Pi))
        else if v < 0.0 then pos(v + (2 * math.Pi))
        else v
    } 

    def vecToPol(vec: Vec): Pol = {
        val r = math.sqrt(vec.x * vec.x + vec.y * vec.y)
        val theta = pos(math.atan2(vec.y, vec.x))
        Pol(r, theta)
    }

    def polToVec(vec: Pol): Vec = {
        val x = vec.r * math.cos(vec.alpha)
        val y = vec.r * math.sin(vec.alpha)        
        Vec(x, y)
    }

    def circle(center: Vec, r: Double, n: Int): Iterable[Vec] = {
        val d = 360.0 / n
        (0 to n).map((a: Int) => degToRad(a.toDouble * d))
            .map(Pol(r, _))
            .map(polToVec)
            .map(_.add(center)) 
    }

    def skewCircle(offset: Pol, radius: Double, n: Int): Iterable[Vec] = {
        circle(polToVec(offset), radius, n).map(_.add(offset))
    } 

    def degToRad(v: Double): Double = v * math.Pi / 180
    def radToDeg(v: Double): Double = v * 180 / math.Pi

    def tryoutVec() = {

        import scala.math._
        val c = skewCircle(Pol(0.1, degToRad(0)), 1.0, 8)
        for (v, i) <- c.zipWithIndex do
            println(s"-- $i ${v} ${vecToPol(v)}")

    }

}