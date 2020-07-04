package net.cheltsov.mtproto

import scodec.Attempt.Failure
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.{Codec, Err}

object Messages {

  case class DecodedMessage(messageId: Long, message: MTProtoMessage)

  sealed trait MTProtoMessage

  case class ReqPQ(nonce: BigInt) extends MTProtoMessage

  case class ReqDHParams(nonce: BigInt,
                         serverNonce: BigInt,
                         p: Int,
                         q: Int,
                         publicKeyFingerprint: Long,
                         encryptedData: ByteVector)
      extends MTProtoMessage {
    override def toString = s"ReqDHParams($nonce, $serverNonce, $p, $q, $publicKeyFingerprint, encrypted data)"
  }

  case class PQInnerData(pq: Long, p: Int, q: Int, nonce: BigInt, serverNonce: BigInt, newNonce: BigInt)

  case class ResPQ(nonce: BigInt,
                   serverNonce: BigInt,
                   pq: Long,
                   serverPublicKeyFingerprints: VectorLong)
      extends MTProtoMessage

  case class VectorLong(longs: List[Long])

  object DecodedMessage {
    val codec: Codec[DecodedMessage] = {
      ("auth_key_id" | constant(int64.encode(0).require)) ::
        ("message_id" | int64) ::
        ("message_data" | MTProtoMessage.codec)
    }.as[DecodedMessage]
  }

  object MTProtoMessage {
    val codec: Codec[MTProtoMessage] = variableSizeBytes(int32, bytes).exmapc[MTProtoMessage](bv => {
      int32.decodeValue(bv.bits).flatMap {
        case ReqPQ.classId        => ReqPQ.codec.decodeValue(bv.bits)
        case ReqDHParams.classId  => ReqDHParams.codec.decodeValue(bv.bits)
        case ResPQ.classId        => ResPQ.codec.decodeValue(bv.bits)
        case _                    => Failure(Err(s"Can not decode message. Constructor number 0x${bv.take(4).toHex}"))
      }
    }){
      case m: ReqPQ               => ReqPQ.codec.encode(m).map(_.bytes)
      case m: ReqDHParams         => ReqDHParams.codec.encode(m).map(_.bytes)
      case m: ResPQ               => ResPQ.codec.encode(m).map(_.bytes)
      case m                      => Failure(Err(s"Can not encode message $m"))
    }
  }

  object ReqPQ {
    val classId: Int = 0x60469778
    val codec: Codec[ReqPQ] = {
      ("constructor number" | constant(int32.encode(classId).require)) ::
        ("nonce" | bigIntCodec(16))
    }.dropUnits.as[ReqPQ]
  }

  object ReqDHParams {
    val classId: Int = 0xd712e4be

    val codec: Codec[ReqDHParams] = {
      ("constructor number" | constant(int32.encode(classId).require)) ::
        ("nonce" | bigIntCodec(16)) ::
        ("server_nonce" | bigIntCodec(16)) ::
        ("p" | fixedVariableInt32Codec) ::
        ("q" | fixedVariableInt32Codec) ::
        ("public_key_fingerprint" | int64) ::
        ("PQInnerData constructor number" | constant(int32.encode(PQInnerData.classId).require)) ::
        ("encrypted_data" | bytes(256))
    }.dropUnits.as[ReqDHParams]
  }

  object PQInnerData {
    val classId: Int = 0x83c95aec

    val codec: Codec[PQInnerData] = {
      ("constructor number" | constant(int32.encode(classId).require)) ::
        ("pq" | fixedVariableInt64Codec) ::
        ("p" | fixedVariableInt32Codec) ::
        ("q" | fixedVariableInt32Codec) ::
        ("nonce" | bigIntCodec(16)) ::
        ("server_nonce" | bigIntCodec(16)) ::
        ("new_nonce" | bigIntCodec(32))
    }.dropUnits.as[PQInnerData]
  }

  object ResPQ {
    val classId: Int = 0x05162463

    val codec: Codec[ResPQ] = {
      ("constructor number" | constant(int32.encode(classId).require)) ::
        ("nonce" | bigIntCodec(16)) ::
        ("server_nonce" | bigIntCodec(16)) ::
        ("pq" | fixedVariableInt64Codec) ::
        ("server_public_key_fingerprints" | VectorLong.codec)
    }.dropUnits.as[ResPQ]
  }

  object VectorLong {
    val classId: Int = 0x1cb5c415

    val codec: Codec[VectorLong] = {
      ("constructor number" | constant(int32.encode(classId).require)) ::
        ("vector" | listOfN(int32, int64))
    }.dropUnits.as[VectorLong]
  }

  def bigIntCodec(size: Long): Codec[BigInt] =
    fixedSizeBytes(size, bytes).xmapc(bv => BigInt(bv.reverse.toArray))(bi => ByteVector(bi.toByteArray.reverse))

  val fixedVariableInt32Codec: Codec[Int] =
    fixedVariableCodec(8, _.toInt(), v => BigInt(v).toByteArray)

  val fixedVariableInt64Codec: Codec[Long] =
    fixedVariableCodec(12, _.toLong(), v => BigInt(v).toByteArray)

  def fixedVariableCodec[T](fixedSize: Int, f: ByteVector => T, g: T => Array[Byte]): Codec[T] =
    fixedSizeBytes(fixedSize, variableSizeBytes(int32, bytes).xmapc[T](f)(v => ByteVector(g(v))))
}
