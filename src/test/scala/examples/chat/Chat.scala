import scala.actors.Actor._
import busybi._

object Chat {
  def main(args: Array[String]) = {
    val handler = actor {
      var browsers = List[Browser]()

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

    new Server("localhost", 1234, handler).start
  }
}
