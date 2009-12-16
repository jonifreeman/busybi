package busybi

import java.io._
import java.net._
import scala.actors.Actor
import scala.actors.Actor._

trait Request
case class Message(browser: Browser, content: String) extends Request
case class Connect(browser: Browser) extends Request

case class Browser()(val socket: Socket) {
  lazy val out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))

  def send(s: String) = {
    out.write(s)
    out.flush
  }

  def close = out.close
}

class Server(address: String, port: Int, handler: Actor) {
  case class ClientSocket(socket: Socket)

  val conn = new ConnectionHandler

  def start = {
    conn.start
    val serverSocket = new ServerSocket(port)    
    while (true) {
      conn ! ClientSocket(serverSocket.accept)
    }
  }

  // FIXME handle close
  class ConnectionHandler extends Actor {
    def act = loop {
      react {
        case ClientSocket(s) =>
          val handshake = 
            "HTTP/1.1 101 Web Socket Protocol Handshake\r\n" +
            "Upgrade: WebSocket\r\n" + 
            "Connection: Upgrade\r\n" +
            "WebSocket-Origin: http://localhost\r\n" +
            "WebSocket-Location: " +
            "  ws://localhost:1234/websession\r\n\r\n"
          val browser = Browser()(s)
          browser.send(handshake)
          handler ! Connect(browser)
      }
    }
  }
}


object Test {
  def main(args: Array[String]) = {
    val handler = actor {
      loop {
        react {
          case Connect(b) => println("connected to " + b)
          case Message(b, msg) => println("got " + msg)
        }
      }
    }

    val s = new Server("localhost", 1234, handler)
    s.start
  }
}
