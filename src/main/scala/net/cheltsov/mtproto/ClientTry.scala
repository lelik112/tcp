package net.cheltsov.mtproto

import java.io.DataOutputStream
import java.net.{InetSocketAddress, Socket}
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousSocketChannel

import net.cheltsov.mtproto.Messages.EncryptedPQInnerData._
import net.cheltsov.mtproto.Messages.{DecodedMessage, EncryptedPQInnerData, PQInnerData, ReqDHParams, ReqPQ, ResPQ, VectorLong}

object ClientTry extends App {

  val encryptData = encrypt(PQInnerData(BigInt(2), 6L, 7L, BigInt(996999699693L), BigInt(77887878L), BigInt(55)), KeyHolder.publicKey)
//  val decryptData = decrypt(encryptData, KeyHolder.privateKey)
  println(encryptData)


  val client = AsynchronousSocketChannel.open
  val hostAddress = new InetSocketAddress("localhost", 669)
  val future = client.connect(hostAddress)
  val buffer: ByteBuffer = ByteBuffer.wrap(DecodedMessage
    .codec
//    .encode(DecodedMessage(513, 3, ReqPQ(BigInt(2))))
    .encode(DecodedMessage(412, 5, ReqDHParams(BigInt(2), BigInt(996999699693L), 6L, 7L, 8L, encryptData)))
//    .encode(DecodedMessage(412, 5, ResPQ(BigInt(2), BigInt(996999699693L), BigInt(77885L), VectorLong(List[Long](1L, 99L)))))
    .require
    .bytes.toArray)

  val res = client.write(buffer)
  client.close()
  println("****" + res.get())

}
