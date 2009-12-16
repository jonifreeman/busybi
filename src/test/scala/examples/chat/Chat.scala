import scala.actors.Actor._
import busybi._

object Chat {
  def main(args: Array[String]) = {
    var browsers = List[Browser]()

    val handler = actor {
      loop {
        react {
          case Connect(b) => 
            println("new browser")
            browsers = b :: browsers
          case Message(b, msg) => browsers.foreach(_.send("out ! " + msg))
          case Disconnect(b) => 
            println("browser left")
            browsers = browsers.remove(_ eq b)
        }
      }
    }

    val s = new Server("localhost", 1234, handler)
    s.start
  }
}
