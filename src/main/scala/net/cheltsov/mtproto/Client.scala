package net.cheltsov.mtproto

import java.net.InetSocketAddress
import java.nio.channels.{AsynchronousServerSocketChannel, AsynchronousSocketChannel, CompletionHandler}

import zio.{IO, UIO}

object Client {
  def start: IO[Throwable, AsynchronousSocketChannel] = {
    IO(AsynchronousSocketChannel.open())
  }


  def connect(client: AsynchronousSocketChannel)(host: String, port: Int): IO[Throwable, Unit] = {
    IO.effectAsync { callback =>
      client.connect(new InetSocketAddress(host, port), (), new CompletionHandler[Void, Unit] {
        override def completed(result: Void, attachment: Unit): Unit = callback(UIO(()))

        override def failed(exc: Throwable, attachment: Unit): Unit = callback(IO.fail(exc))
      })
    }
  }

  def stop(client: AsynchronousSocketChannel): UIO[Unit] = UIO(client.close())
}
