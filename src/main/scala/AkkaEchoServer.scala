import java.net._
import java.io._
import scala.io._
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}

object EchoServer {
  def props: Props = Props[EchoServer]
  final case class MySocket(socket: Socket)
}

class EchoServer extends Actor {
  import EchoServer._

  def read_and_write(in: BufferedReader, out:BufferedWriter): Unit = {
    val serverData = in.readLine()
    out.write(in.readLine())
    out.write("\r\n")

    try{
      readHome(serverData, out)
    }
    catch{
      case ex: FileNotFoundException =>{
        errorHandling(serverData, out)
      }
    }
    out.flush()
    in.close()
    out.close()
  }

  def receive: PartialFunction[Any, Unit] = {
    case MySocket(s) =>
      val in = new BufferedReader(new InputStreamReader(s.getInputStream))
      val out = new BufferedWriter(new OutputStreamWriter(s.getOutputStream))

      read_and_write(in, out)

      s.close()
  }
  def readHome(in: String, out:BufferedWriter): Unit = {
    val htmlFile = Source.fromFile("home.html")
    val serverIn = in.split(" ")

    sender() ! out.write(s"${serverIn(2)} Home Page\r\n")
    sender() ! out.write("Content-Type=text/html\r\n")
    sender() ! out.write("\r\n")
    sender() ! out.write(htmlFile.mkString)

  }

  def errorHandling(in: String, out:BufferedWriter): Unit = {
    val fourohfour = Source.fromFile("404.html")
    val serverIn = in.split(" ")

    sender() ! out.write(s"${serverIn(2)} 404 File Not Found \r\n")
    sender() ! out.write("Content-Type=text/html\r\n")
    sender() ! out.write("\r\n")
    sender() ! out.write(fourohfour.mkString)

  }

}

object AkkaEchoServer {
  import EchoServer._

  val system: ActorSystem = ActorSystem("AkkaEchoServer")
  val my_server: ActorRef = system.actorOf(EchoServer.props)

  def main(args: Array[String]) {
    val server = new ServerSocket(9999)
    while(true) {
      val s = server.accept()
      my_server ! MySocket(s)
    }
  }
}



