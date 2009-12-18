package busybi

import java.io._
import java.net._
import scala.actors.Actor
import scala.actors.Actor._

// FIXME, initial state is stored in a field, prevents gc
abstract class App[State](val initial: State) {
  type RequestState = PartialFunction[(State, Request), (State, Stream[Response])]

  private val nop: RequestState = { case (s, _) => (s, Stream.empty) }
  // FIXME eliminate var
  private[busybi] var f: RequestState = _

  def receive(f: RequestState) = this.f = f orElse nop
}

case class Response(browser: Browser, content: String)

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

class Server[S](address: String, port: Int, app: App[S]) {
  case class ClientSocket(socket: Socket)

  // FIXME replace with STM? concurrent requests can see different view of state
  val state = new scala.concurrent.SyncVar[S]
  state.set(app.initial)

  def start = {
    val serverSocket = new ServerSocket(port)    
    forever { new ConnectionHandler ! ClientSocket(serverSocket.accept) }
  }

  private def forever(f: => Unit): Unit = { f; forever(f) }

  class ConnectionHandler extends Actor {
    start

    def act = react {
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
        state.set(flush(app.f(state.get, Connect(browser))))
        val in = new BufferedReader(new InputStreamReader(s.getInputStream))
        skipHeaders(in)
        read(browser, in)
    }
    
    private def skipHeaders(in: BufferedReader) = while (in.readLine != "") {}

    private def flush(resp: (S, Stream[Response])) = {
      val (s, rs) = resp
      rs.foreach { r => r.browser.send(r.content) }
      s
    }

    // FIXME check spec, optimize, Content-Length
    private def read(browser: Browser, in: BufferedReader): Unit = {
      var c = -9999
      val content = new StringBuilder
      while (c != 0xfffd) {
        c = in.read
        if (c == -1) {
          state.set(flush(app.f(state.get, Disconnect(browser))))
          return;
        }
        content.append(c.toChar)
      }
      content.setLength(content.length-1)
      state.set(flush(app.f(state.get, Message(browser, content.toString))))
      read(browser, in)
    }
  }
}
