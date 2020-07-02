package net.cheltsov.mtproto

import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousSocketChannel, CompletionHandler}

import net.cheltsov.mtproto.Messages.{DecodedMessage, ReqDHParams, ReqPQ}
import scodec.bits.BitVector
import zio.{IO, Ref, UIO}

sealed trait StateValue

case object WaitForReqPQ extends StateValue

case object WaitForReqDHParams extends StateValue

case object Done extends StateValue

case class State(state: StateValue)

//TODO There is not a reason to use Ref[A]. It is not concurrent environment
class AuthProcess (state: Ref[State], client: AsynchronousSocketChannel) {

  def process(): IO[Throwable, Unit] = {

    for {
      waitPQ <- state.get
      reqPQ <- waitPQ match {
        case State(WaitForReqPQ) => readMTProtoMessage
      }
      _ <- reqPQ match {
        case DecodedMessage(_, _, _: ReqPQ) => state.set(State(WaitForReqDHParams))
      }
      _ <- writeMTProtoMessage(null)
      reqPQParams <- readMTProtoMessage
      _ <- state.set(State(Done))
    } yield println(reqPQParams)

  }

  //TODO make private
  def readMTProtoMessage: IO[Throwable, DecodedMessage] = {
    val buffer = ByteBuffer.allocate(1024)
    IO.effectAsync { callback =>
      client.read(buffer, (), new CompletionHandler[Integer, Unit] {
        override def completed(result: Integer, attachment: Unit): Unit = {
          buffer.flip()
          callback(IO.fromTry(DecodedMessage.codec.decodeValue(BitVector(buffer)).toTry))
        }

        override def failed(exc: Throwable, attachment: Unit): Unit = callback(IO.fail(exc))
      })
    }
  }

  //TODO make private
  def writeMTProtoMessage(message: DecodedMessage): IO[Throwable, Int] = {
    IO.fromTry(DecodedMessage.codec.encode(message).toTry)
      .map(bv => ByteBuffer.wrap(bv.toByteArray))
      .flatMap { bytes =>
        IO.effectAsync { callback =>
          client.write(bytes, (), new CompletionHandler[Integer, Unit] {
            override def completed(result: Integer, attachment: Unit): Unit = callback(IO.succeed(result))

            override def failed(exc: Throwable, attachment: Unit): Unit = callback(IO.fail(exc))
          })
        }
      }
  }

  def stop: UIO[Unit] = IO(client.close()).catchAll { e =>
    UIO(println(e))
  }

}

object AuthProcess {
  def apply(client: AsynchronousSocketChannel): IO[Throwable, AuthProcess] = {
    Ref.make(State(WaitForReqDHParams)).map(new AuthProcess(_, client))
  }
}
