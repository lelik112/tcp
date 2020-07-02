package net.cheltsov.mtproto

import java.nio.channels.AsynchronousSocketChannel

import net.cheltsov.mtproto.Helpers._
import net.cheltsov.mtproto.Messages._
import zio.{IO, Ref, Task, UIO}

sealed trait StateValue

case object WaitForReqPQ extends StateValue

case object WaitForReqDHParams extends StateValue

case object Done extends StateValue

case class State(state: StateValue)

class AuthProcess (state: Ref[State], client: AsynchronousSocketChannel) {

  def process: Task[Int] =
    processLogic
      .bracket(_ => UIO(client.close()))(_ => Task(0))

  private val processLogic: IO[Throwable, Unit] =
    for {
      initialState <- state.get
      reqPQ <- initialState match {
        case State(WaitForReqPQ) => client.readMTProtoMessage()
        case s => IO.fail(new IllegalStateException(s"Illegal state: $s"))
      }
      _ <- reqPQ match {
        case DecodedMessage(_, _, _: ReqPQ) => state.set(State(WaitForReqDHParams))
        case m => IO.fail(new IllegalStateException(s"Waiting for reqPQ but got: $m"))
      }
      //TODO create message with random data
      _ <- client.writeMTProtoMessage(DecodedMessage(412, 5, ResPQ(BigInt(2), BigInt(996999699693L), BigInt(77885L), VectorLong(List[Long](1L, 99L)))))
      _ <- state.set(State(WaitForReqDHParams)) //TODO Looks like there is not a reason yo change it
      reqDHParams <- client.readMTProtoMessage()
      _ <- reqDHParams match {
        case DecodedMessage(_, _, m: ReqDHParams) => println(m);state.set(State(Done))
        case m => IO.fail(new IllegalStateException(s"Waiting for ReqDHParams but got: $m"))
      }
      _ <- state.set(State(Done)) //TODO Looks like there is not a reason yo change it
    } yield ()
}

object AuthProcess {
  def apply(client: AsynchronousSocketChannel): IO[Throwable, AuthProcess] = {
    Ref.make(State(WaitForReqPQ)).map(new AuthProcess(_, client))
  }
}
