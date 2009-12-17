import scala.actors.Actor._
import busybi._

object Chat {
  def main(args: Array[String]) = {
    val chat = new App(List[Browser]()) {
      receive {
        case (bs, Connect(b)) => 
          println("new browser " + b)
          (b :: bs, Stream.empty)
        case (bs, Message(b, msg)) => 
          (bs, bs.map(Response(_, "out ! " + msg)).toStream)
        case (bs, Disconnect(b)) => 
          println("browser left " + b)
          (bs.remove(_ eq b), Stream.empty)
      }
    }

    new Server("localhost", 1234, chat).start
  }
}
