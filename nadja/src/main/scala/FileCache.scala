object FileCache {

    var cache = Map[String, Array[Byte]]()

    def save(key: String, file: os.Path, f: (os.Path) => Unit): Unit = {
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

    }

}
