package net.cheltsov.mtproto
import net.cheltsov.mtproto.Helpers._
import net.cheltsov.mtproto.Messages.EncryptedPQInnerData.encrypt
import net.cheltsov.mtproto.Messages.{DecodedMessage, PQInnerData, ReqDHParams, ReqPQ}
import zio.{ExitCode, URIO}

object MtProtoTCPClient extends zio.App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    Client.start
      .bracket(Client.stop) { client =>
        val encryptData = encrypt(PQInnerData(BigInt(2), 6L, 7L, BigInt(996999699693L), BigInt(77887878L), BigInt(55)), KeyHolder.publicKey)
        for {
          _ <- Client.connect(client)("127.0.0.1", 669)
          _ <- client.writeMTProtoMessage(DecodedMessage(513, 3, ReqPQ(BigInt(2))))
          _ <- client.readMTProtoMessage()
          _ <- client.writeMTProtoMessage(DecodedMessage(412, 5, ReqDHParams(BigInt(2), BigInt(996999699693L), 6L, 7L, 8L, encryptData)))
        } yield ()
      }.exitCode
  }
}
