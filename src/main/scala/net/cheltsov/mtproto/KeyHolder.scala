package net.cheltsov.mtproto

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

object KeyHolder {
  val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
  keyPairGenerator.initialize(2048)
  val keyPair: KeyPair = keyPairGenerator.generateKeyPair
  val publicKey: PublicKey = keyPair.getPublic
  val privateKey: PrivateKey = keyPair.getPrivate
}
