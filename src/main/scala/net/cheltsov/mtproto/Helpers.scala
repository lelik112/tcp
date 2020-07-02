package net.cheltsov.mtproto

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousSocketChannel, CompletionHandler}

import net.cheltsov.mtproto.Messages.DecodedMessage
import scodec.bits.BitVector
import zio.IO

object Helpers {

  implicit class readWriteFromChanel(socket: AsynchronousSocketChannel) {
    def readMTProtoMessage(): IO[Throwable, DecodedMessage] = {
      val buffer = ByteBuffer.allocate(1024)
      IO.effectAsync { callback =>
        socket.read(buffer, (), new CompletionHandler[Integer, Unit] {
          override def completed(result: Integer, attachment: Unit): Unit = {
            buffer.flip()
            callback(IO.fromTry(DecodedMessage.codec.decodeValue(BitVector(buffer)).toTry))
          }

          override def failed(exc: Throwable, attachment: Unit): Unit = callback(IO.fail(exc))
        })
      }
    }

    def writeMTProtoMessage(message: DecodedMessage): IO[Throwable, Int] = {
      IO.fromTry(DecodedMessage.codec.encode(message).toTry)
        .map(bv => ByteBuffer.wrap(bv.toByteArray))
        .flatMap { bytes =>
          IO.effectAsync { callback =>
            socket.write(bytes, (), new CompletionHandler[Integer, Unit] {
              override def completed(result: Integer, attachment: Unit): Unit = callback(IO.succeed(result))

              override def failed(exc: Throwable, attachment: Unit): Unit = callback(IO.fail(exc))
            })
          }
        }
    }
  }
}
