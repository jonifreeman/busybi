package busybi

import java.io._
import java.net._
import scala.actors.Actor
import scala.actors.Actor._

trait Request
case class Message(browser: Browser, content: String) extends Request
case class Connect(browser: Browser) extends Request
case class Disconnect(browser: Browser) extends Request

class Browser(out: OutputStream) {
  def send(s: String) = {
    println("sending " + s)
    out.write(0)
    out.write(s.getBytes("UTF-8"))
    out.write(255)
    out.flush
  }

  def close = out.close
}

object Browser {
  def unapply(b: Browser): Option[Browser] = Some(b)
}

class Server(address: String, port: Int, handler: Actor) {
  case class ClientSocket(socket: Socket)

  def start = {
    val serverSocket = new ServerSocket(port)    
    while (true) {
      new ConnectionHandler ! ClientSocket(serverSocket.accept)
    }
  }

  class ConnectionHandler extends Actor {
    start

    def act = loop {
      react {
        case ClientSocket(s) =>
          // FIXME remove hardcodings
          val handshake = 
            "HTTP/1.1 101 Web Socket Protocol Handshake\r\n" +
            "Upgrade: WebSocket\r\n" + 
            "Connection: Upgrade\r\n" +
            "WebSocket-Origin: http://localhost\r\n" +
            "WebSocket-Location: " +
            "  ws://localhost:" + port + "/websession\r\n\r\n"
          val out = new BufferedOutputStream(s.getOutputStream)
          val browser = new Browser(out)
          out.write(handshake.getBytes("UTF-8"))
          out.flush
          handler ! Connect(browser)
          val in = new BufferedReader(new InputStreamReader(s.getInputStream))
          skipHeaders(in)
          read(browser, in)
      }
    }
    
    private def skipHeaders(in: BufferedReader) = while (in.readLine != "") {}

    // FIXME check spec, optimize, Content-Length
    private def read(browser: Browser, in: BufferedReader): Unit = {
      var c = -9999
      val content = new StringBuilder
      while (c != 0xfffd) {
        c = in.read
        if (c == -1) {
          handler ! Disconnect(browser)
          return;
        }
        content.append(c.toChar)
      }
      content.setLength(content.length-1)
      handler ! Message(browser, content.toString)
      read(browser, in)
    }
  }
}
