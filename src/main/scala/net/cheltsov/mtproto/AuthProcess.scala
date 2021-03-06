package net.cheltsov.mtproto

import java.nio.channels.AsynchronousSocketChannel

import net.cheltsov.mtproto.Helpers._
import net.cheltsov.mtproto.Random._
import net.cheltsov.mtproto.Messages._
import zio.{IO, Ref, Task, UIO, ZIO}
import zio.console._

sealed trait StateValue

case object WaitForReqPQ extends StateValue

case object WaitForReqDHParams extends StateValue

case object Done extends StateValue

case class State(state: StateValue)

class AuthProcess (state: Ref[State], client: AsynchronousSocketChannel) {

  def process: ZIO[Console, Throwable, Unit] =
    processLogic
      .bracket(_ => UIO(client.close()))(_ => Task(0))

  private val processLogic: ZIO[Console, Throwable, Unit] =
    for {
      initialState  <- state.get
      reqPQ         <- initialState match {
                        case State(WaitForReqPQ) => client.readMTProtoMessage()
                        case s => IO.fail(new IllegalStateException(s"Illegal state: $s"))
                      }
      _             <- reqPQ match {
                        case DecodedMessage(_, _: ReqPQ) => state.set(State(WaitForReqDHParams))
                        case m => IO.fail(new IllegalStateException(s"Waiting for reqPQ but got: $m"))
                      }
      _             <- putStrLn(s"Message received: $reqPQ")
      resPQ         <- nextResPQ
      _             <- putStrLn(s"Sending message: $resPQ")
      _             <- client.writeMTProtoMessage(resPQ)
      _             <- state.set(State(WaitForReqDHParams))
      reqDHParams   <- client.readMTProtoMessage()
      _             <- putStrLn(s"Message received: $reqDHParams")
      encryptedData <- reqDHParams match {
                        case DecodedMessage(_, ReqDHParams(_, _, _, _, _, encryptedData)) => IO(encryptedData)
                        case m => IO.fail(new IllegalStateException(s"Waiting for ReqDHParams but got: $m"))
                      }
      pQInnerData   <- KeyHolder.privateKey.decryptPQInnerData(encryptedData)
      _             <- putStrLn(s"Secret message: $pQInnerData")
      _             <- state.set(State(Done))
    } yield ()
}

object AuthProcess {
  def apply(client: AsynchronousSocketChannel): IO[Throwable, AuthProcess] = {
    Ref.make(State(WaitForReqPQ)).map(new AuthProcess(_, client))
  }
}
