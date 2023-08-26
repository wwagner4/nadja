import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }

object FileCache {

    case class Save(key: String, file: os.Path, f: (os.Path) => Unit)


    val cacheActor: Behavior[Save] = Behaviors.setup { context => {

            var cache = Map[String, Array[Byte]]()

            Behaviors.receiveMessage {
                case Save(key, file, f) => 
                    println(s"SAVE ${key}")

                    cache.get(key) match {
                        case Some(content) => 
                            os.write.over(file, content)
                            println(s"CACHE: Wrote $key ${file} from cache")
                        case None =>
                            f(file)
                            println(s"CACHE: Saved to $key ${file}")
                            val content = os.read.bytes(file)
                            println(s"CACHE: Read content ${content.size}")
                            cache += (key -> content)
                            println(s"CACHE: put content to cache ${cache(key).size}")
                    }

                    Behaviors.same
            }
        }
    }
    val rootActor: ActorSystem[Save] = {
        println("AKKA starting")
        val s = ActorSystem(cacheActor, "cache")
        println("AKKA started")
        s
    }

    var cache = Map[String, Array[Byte]]()

    def save(key: String, file: os.Path, f: (os.Path) => Unit): Unit = {
        rootActor ! Save(key, file, f)
        println("SENT save")
    }
}
/*
        cache.get(key) match {
            case Some(content) => 
                os.write.over(file, content)
                println(s"CACHE: Wrote $key ${file} from cache")
            case None =>
                f(file)
                println(s"CACHE: Saved to $key ${file}")
                val content = os.read.bytes(file)
                println(s"CACHE: Read content ${content.size}")
                cache += (key -> content)
                println(s"CACHE: put content to cache ${cache(key).size}")
        }
*/



