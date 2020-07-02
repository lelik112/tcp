package net.cheltsov.mtproto

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel, CompletionHandler}

import net.cheltsov.mtproto.Messages.DecodedMessage
import scodec.bits.BitVector
import zio.{IO, UIO}

object Server {
  def start(host: String, port: Int): IO[Throwable, AsynchronousServerSocketChannel] = {
    IO(AsynchronousServerSocketChannel.open().bind(new InetSocketAddress(host, port)))
  }

  def accept(server: AsynchronousServerSocketChannel): IO[Throwable, AsynchronousSocketChannel] = {
    IO.effectAsync {callback =>
      server.accept((), new CompletionHandler[AsynchronousSocketChannel, Unit] {
        override def completed(result: AsynchronousSocketChannel, attachment: Unit): Unit = callback(UIO(result))

        override def failed(exc: Throwable, attachment: Unit): Unit = callback(IO.fail(exc))
      })
    }
  }

  def stop(server: AsynchronousServerSocketChannel): UIO[Unit] = IO(server.close()).catchAll { e =>
    UIO(println(e))
  }
}
