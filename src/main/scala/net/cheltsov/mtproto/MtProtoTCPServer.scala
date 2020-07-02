package net.cheltsov.mtproto

object MtProtoTCPServer extends App {
  Server.start("127.0.0.1", 669)
    .bracket(Server.stop) {server =>
      Server.accept(server).flatMap { client =>
        AuthProcess(client)
      }.forever
    }
}
