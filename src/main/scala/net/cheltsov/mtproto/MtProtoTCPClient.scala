package net.cheltsov.mtproto
import net.cheltsov.mtproto.Helpers._
import net.cheltsov.mtproto.Random._
import zio.console.putStrLn
import zio.{ExitCode, URIO}

object MtProtoTCPClient extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    Client.start
      .bracket(Client.stop) { client =>
        for {
          _           <- Client.connect(client)("127.0.0.1", 669)
          reqPQ       <- nextReqPQ
          _           <- putStrLn(s"Sending message: $reqPQ")
          _           <- client.writeMTProtoMessage(reqPQ)
          resPQ       <- client.readMTProtoMessage()
          _           <- putStrLn(s"Message received: $resPQ")
          pQInnerData <- nextPQInnerData
          _           <- putStrLn(s"Encrypting message: $pQInnerData")
          encryptData <- KeyHolder.publicKey.encryptPQInnerData(pQInnerData)
          reqDHParams <- nextReqDHParams(encryptData)
          _           <- putStrLn(s"Sending message: $reqDHParams")
          _           <- client.writeMTProtoMessage(reqDHParams)
        } yield ()
      }.exitCode
  }
}
