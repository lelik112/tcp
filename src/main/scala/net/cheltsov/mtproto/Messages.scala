package net.cheltsov.mtproto

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.{KeyPair, KeyPairGenerator, MessageDigest, PrivateKey, PublicKey, Signature}
import java.security.interfaces.RSAPublicKey

import javax.crypto.Cipher
import scodec.Attempt.Failure
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Codec, Err}

object Messages {

  private val cipher: Cipher = Cipher.getInstance("RSA/ECB/NoPadding")

  private val digest = MessageDigest.getInstance("SHA-1")

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
                         encryptedData: EncryptedPQInnerData)
      extends RequestMessage

  case class PQInnerData(pq: BigInt, p: Long, q: Long, nonce: BigInt, serverNonce: BigInt, newNonce: BigInt)

  case class EncryptedPQInnerData(data: String)

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
        ("encrypted_data" | utf8_32L.as[EncryptedPQInnerData])
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

  object EncryptedPQInnerData {
    def encrypt(innerData: PQInnerData, publicKey: PublicKey): EncryptedPQInnerData = {
      val data: Array[Byte] = PQInnerData.codec.encode(innerData).require.toByteArray
      val hash = digest.digest(data)
      val encodedKey = publicKey.getEncoded.takeRight(140) //TODO not sure about this
      cipher.init(Cipher.ENCRYPT_MODE, publicKey)
      val encryptedBytes = cipher.doFinal(hash ++ data)
      val encryptedString: String = new String(encryptedBytes, StandardCharsets.UTF_8)
      val bytes = encryptedString.getBytes(StandardCharsets.UTF_8)
//      val encryptedString: String = ascii32L.decodeValue(BitVector(encryptedBytes)).require
//      println("*******" + encryptedString)
      EncryptedPQInnerData(encryptedString)


//      val encodedKey = new Array[Byte](140)// publicKey.getEncoded.takeRight(140) //TODO not sure about this
//      //      val newEncodedKey = encodedKey.map(_ => 3.asInstanceOf[Byte])
//      val size = hash ++ data ++ encodedKey
//      cipher.init(Cipher.ENCRYPT_MODE, publicKey)
//      var goodKey = publicKey
//      var flag = false
//      var bytes: Array[Byte] = null
//
//      while (!flag) {
//        try {
//          val newEncodedKey = goodKey.getEncoded.takeRight(140)
//          val toEncode = hash ++ data ++ newEncodedKey
//          bytes = cipher.doFinal(toEncode)
//          true
//        } catch {
//          case _: Throwable => {
//            println("****************")
//            val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
//            keyPairGenerator.initialize(2048)
//            val keyPair: KeyPair = keyPairGenerator.generateKeyPair
//            goodKey = keyPair.getPublic
//            println(goodKey)
//            cipher.init(Cipher.ENCRYPT_MODE, publicKey)
//          }
//        }

      }

    def decrypt(encryptedPQInnerData: EncryptedPQInnerData, privateKey: PrivateKey): PQInnerData = {
      val encryptedString: String = encryptedPQInnerData.data
      val encryptedBytes: Array[Byte] = encryptedString.getBytes(StandardCharsets.UTF_8)

      cipher.init(Cipher.DECRYPT_MODE, privateKey)
      val allData = cipher.doFinal(encryptedBytes)
      val decryptPQInnerDataArray = allData.drop(20).dropRight(140)
      val decryptPQInnerData = PQInnerData.codec.decodeValue(BitVector(decryptPQInnerDataArray)).require
      decryptPQInnerData
    }
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
