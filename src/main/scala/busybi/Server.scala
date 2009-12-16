package busybi

import java.io._
import java.net._
import scala.actors.Actor
import scala.actors.Actor._

trait Request
case class Message(browser: Browser, content: String) extends Request
case class Connect(browser: Browser) extends Request

case class Browser()(val out: BufferedWriter) {
  def send(s: String) = {
    println("sending " + s)
    out.write(0)
    out.write(s)
    out.write(0xff)
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
          val out = new BufferedWriter(new OutputStreamWriter(s.getOutputStream))
          val browser = Browser()(out)
          out.write(handshake)
          out.flush
          handler ! Connect(browser)
          val in = new BufferedReader(new InputStreamReader(s.getInputStream))
          skipHeaders(in)
          read(browser, in)
      }
    }
    
    private def skipHeaders(in: BufferedReader) = 
      while (in.readLine != "") {}

    // FIXME check spec, optimize, Content-Length
    private def read(browser: Browser, in: BufferedReader): Unit = {
      var c = -1
      val content = new StringBuilder
      while (c != 0xfffd) {
        c = in.read
        content.append(c.toChar)
      }
      handler ! Message(browser, content.toString)
      read(browser, in)
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
