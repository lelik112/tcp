package net.cheltsov.mtproto

import net.cheltsov.mtproto.Messages._
import scodec.bits.ByteVector
import zio.Task

import scala.util.Random

object Random {
  private val r = new Random()

  def nextReqPQ: Task[DecodedMessage] =
    Task(DecodedMessage(System.nanoTime, ReqPQ(nextBigInt(16))))

  def nextResPQ: Task[DecodedMessage] =
    Task(
      DecodedMessage(
        System.nanoTime,
        ResPQ(
          nextBigInt(16),
          nextBigInt(16),
          r.nextLong,
          VectorLong(List(r.nextLong))
        )
      )
    )

  def nextReqDHParams(encryptedData: ByteVector): Task[DecodedMessage] =
    Task(
      DecodedMessage(
        System.nanoTime,
        ReqDHParams(
          nextBigInt(16),
          nextBigInt(16),
          r.nextInt,
          r.nextInt,
          r.nextLong,
          encryptedData
        )
      )
    )

  def nextPQInnerData: Task[PQInnerData] =
    Task(
      PQInnerData(
        r.nextLong,
        r.nextInt,
        r.nextInt,
        nextBigInt(16),
        nextBigInt(16),
        nextBigInt(32)
      )
    )

  def nextBigInt(size: Int) = BigInt(r.nextBytes(size))

}
