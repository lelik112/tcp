package net.cheltsov.mtproto

import scodec.Attempt.Failure
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.{Codec, Err}

object Messages {

  case class DecodedMessage(authKeyId: Long, messageId: Long, message: MTProtoMessage)

  sealed trait MTProtoMessage

  sealed trait RequestMessage extends MTProtoMessage

  sealed trait ResponseMessage extends MTProtoMessage

  case class ReqPQ(nonce: BigInt) extends RequestMessage

  case class ReqDHParams(nonce: BigInt,
                         serverNonce: BigInt,
                         p: Long,
                         q: Long,
                         publicKeyFingerprint: Long,
                         encryptedData: ByteVector)
      extends RequestMessage

  case class PQInnerData(pq: BigInt, p: Long, q: Long, nonce: BigInt, serverNonce: BigInt, newNonce: BigInt)

  case class ResPQ(nonce: BigInt,
                   serverNonce: BigInt,
                   pq: BigInt,
                   serverPublicKeyFingerprints: VectorLong)
      extends RequestMessage

  case class VectorLong(longs: List[Long])

  object DecodedMessage {
    val codec: Codec[DecodedMessage] = {
      ("auth_key_id" | int64L) ::
        ("message_id" | int64L) ::
        ("message_data" | MTProtoMessage.codec)
    }.as[DecodedMessage]
  }

  object MTProtoMessage {
    val codec: Codec[MTProtoMessage] = variableSizeBytes(int32L, bytes).exmapc[MTProtoMessage](bv => {
      int32L.decodeValue(bv.bits).flatMap {
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
      ("constructor number" | constant(int32L.encode(classId).require)) ::
        ("nonce" | bigIntCodec(16))
    }.dropUnits.as[ReqPQ]
  }

  object ReqDHParams {
    val classId: Int = 0xd712e4be

    val codec: Codec[ReqDHParams] = {
      ("constructor number" | constant(int32L.encode(classId).require)) ::
        ("nonce" | bigIntCodec(16)) ::
        ("server_nonce" | bigIntCodec(16)) ::
        ("p" | int64L) ::
        ("q" | int64L) ::
        ("public_key_fingerprint" | int64L) ::
        ("encrypted_data" | bytes(256))
    }.dropUnits.as[ReqDHParams]
  }

  object PQInnerData {
    val classId: Int = 0x83c95aec

    val codec: Codec[PQInnerData] = {
      ("constructor number" | constant(int32L.encode(classId).require)) ::
        ("pq" | bigIntCodec(12)) ::
        ("p" | int64L) ::
        ("q" | int64L) ::
        ("nonce" | bigIntCodec(16)) ::
        ("server_nonce" | bigIntCodec(16)) ::
        ("new_nonce" | bigIntCodec(32))
    }.dropUnits.as[PQInnerData]
  }

  object ResPQ {
    val classId: Int = 0x05162463

    val codec: Codec[ResPQ] = {
      ("constructor number" | constant(int32L.encode(classId).require)) ::
        ("nonce" | bigIntCodec(16)) ::
        ("server_nonce" | bigIntCodec(16)) ::
        ("pq" | bigIntCodec(12)) ::
        ("server_public_key_fingerprints" | VectorLong.codec)
    }.dropUnits.as[ResPQ]
  }

  object VectorLong {
    val classId: Int = 0x1cb5c415

    val codec: Codec[VectorLong] = {
      ("constructor number" | constant(int32L.encode(classId).require)) ::
        ("vector" | listOfN(int32L, int64L))
    }.dropUnits.as[VectorLong]
  }

  //TODO add additional codec for doing things through strings for p, q, pq
  def bigIntCodec(size: Long): Codec[BigInt] =
    fixedSizeBytes(size, bytes).xmapc(bv => BigInt(bv.reverse.toArray))(bi => ByteVector(bi.toByteArray.reverse))


}
