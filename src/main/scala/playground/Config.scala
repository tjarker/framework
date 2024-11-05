package playground

case class PathBuilder(path: Seq[Object]) {
  def /(key: Object): PathBuilder = PathBuilder(path :+ key)
}

object Config {

  private var db: Map[Object, Any] = Map()

  def set[T](key: Object, value: T): Unit = {
    db += (key -> value)
  }

  def get[T](key: Object): Option[T] = {
    db.get(key).asInstanceOf[Option[T]]
  }

  def put[T](keys: Seq[Object], value: T): Unit = {
    val lastKey = keys.last
    val initialMap = keys.dropRight(1).foldLeft(db) { (map, key) =>
      map.getOrElse(key, Map()).asInstanceOf[Map[Object, Any]]
    }
    db += (keys.head -> initialMap.+(lastKey -> value))
  }

  def get[T](keys: Seq[Object]): Option[T] = {
    val lastKey = keys.last
    val initialMap = keys.dropRight(1).foldLeft(db) { (map, key) =>
      map.getOrElse(key, Map()).asInstanceOf[Map[Object, Any]]
    }
    initialMap.get(lastKey).asInstanceOf[Option[T]]
  }

}

@main def testConfigDB(): Unit = {

  extension (c: Object) {
    def /(that: Object): PathBuilder = PathBuilder(Seq(c, that))
  }

  class Printer {
    Config.get("msg").foreach(println)
  }

  Config.set("name", "John Doe")
  Config.set("age", 42)

  val name = Config.get[String]("name")
  val age = Config.get[Int]("age")

  println(s"Name: $name, Age: $age")

}
