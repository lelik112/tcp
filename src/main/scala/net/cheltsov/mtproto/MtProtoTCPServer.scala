package net.cheltsov.mtproto
import zio.{ExitCode, URIO}

object MtProtoTCPServer extends zio.App {

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    Server.start("127.0.0.1", 669)
        .bracket(Server.stop) { server =>
          Server.accept(server).flatMap { client =>
            AuthProcess(client).flatMap(_.process)
          }.forever
        }.exitCode
  }
}
