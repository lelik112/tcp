package net.cheltsov.mtproto

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousSocketChannel, CompletionHandler}
import java.security.{MessageDigest, PrivateKey, PublicKey}

import javax.crypto.Cipher
import net.cheltsov.mtproto.Messages.{DecodedMessage, PQInnerData}
import scodec.bits.{BitVector, ByteVector}
import zio.IO

object Helpers {

  private val cipher: Cipher = Cipher.getInstance("RSA/ECB/NoPadding")

  private val digest = MessageDigest.getInstance("SHA-1")

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

  implicit class EncryptPQInnerData(publicKey: PublicKey) {
    def encryptPQInnerData(innerData: PQInnerData): IO[Throwable, ByteVector] = {
      IO.fromTry(PQInnerData.codec.encode(innerData).map(_.toByteArray).toTry).map { data =>
        val hash = digest.digest(data)
        cipher.init(Cipher.ENCRYPT_MODE, publicKey)
        val encryptedBytes = cipher.doFinal(hash ++ data)
        ByteVector(encryptedBytes)
      }
    }
  }

  implicit class DecryptPQInnerData(privateKey: PrivateKey) {
    def decryptPQInnerData(encryptedPQInnerData: ByteVector): IO[Throwable, PQInnerData] = {
      val encryptedBytes: Array[Byte] = encryptedPQInnerData.toArray
      cipher.init(Cipher.DECRYPT_MODE, privateKey)
      val allData = cipher.doFinal(encryptedBytes)
      val decryptPQInnerDataArray = allData.drop(160)
      IO.fromTry(PQInnerData.codec.decodeValue(BitVector(decryptPQInnerDataArray)).toTry)
    }
  }
}
