package net.cheltsov.mtproto

import java.io.{DataInputStream, IOException, InputStream}
import java.nio.ByteBuffer
import java.net.{InetSocketAddress, ServerSocket, Socket}
import java.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel}
import java.util

import net.cheltsov.mtproto.Messages.{DecodedMessage, EncryptedPQInnerData}
import net.cheltsov.mtproto.Messages.EncryptedPQInnerData._

import scala.concurrent.{ExecutionContext, Future}


object ServerTry extends App {

  implicit val ec: ExecutionContext = ExecutionContext.global
//  val serverOld: ServerSocket = new ServerSocket(669)
//  val clientOld: Socket = serverOld.accept()
//  val inOld: DataInputStream = new DataInputStream(clientOld.getInputStream)

//  while (!clientOld.isClosed) {
//    val value: String = inOld.readUTF()
//    println(value)
//    if (value == "close") {
//      println("Closing")
//      clientOld.close()
//    }
//  }
//  serverOld.close()

  val socket = AsynchronousServerSocketChannel.open().bind(new InetSocketAddress("127.0.0.1", 669))
  Nio.accept(socket)
    .flatMap(Nio.readMTProtoMessage)
    .flatMap {s =>
      println(decrypt(s.message.asInstanceOf[EncryptedPQInnerData], KeyHolder.privateKey))
      Future.successful(s)
    }
    .foreach(_ => socket.close)

}

