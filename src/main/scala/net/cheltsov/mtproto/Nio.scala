package net.cheltsov.mtproto

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel, CompletionHandler}

import net.cheltsov.mtproto.Messages.DecodedMessage
import scodec.bits.BitVector

import scala.concurrent.{Future, Promise}

object Nio {
  def accept(server: AsynchronousServerSocketChannel): Future[AsynchronousSocketChannel] = {
    val promise = Promise[AsynchronousSocketChannel]
    server.accept(null, new CompletionHandler[AsynchronousSocketChannel, Void] {
      override def completed(client: AsynchronousSocketChannel, attachment: Void): Unit =
        promise.success(client)

      override def failed(exc: Throwable, attachment: Void): Unit = promise.failure(exc)
    })
    promise.future
  }

  def readMTProtoMessage(connection: AsynchronousSocketChannel): Future[DecodedMessage] = {
    val buffer = ByteBuffer.allocate(1024)
    val promise = Promise[DecodedMessage]
    connection.read(buffer, null, new CompletionHandler[Integer, Void] {
      override def completed(result: Integer, attachment: Void): Unit = {
        buffer.flip()
        promise.success(DecodedMessage.codec.decodeValue(BitVector(buffer)).require)
        connection.close()
      }

      override def failed(exc: Throwable, attachment: Void): Unit = {
        promise.failure(exc)
        connection.close()
      }
    })
    promise.future
  }
}
