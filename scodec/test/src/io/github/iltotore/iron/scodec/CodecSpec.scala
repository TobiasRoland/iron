package io.github.iltotore.iron.scodec

import _root_.scodec
import _root_.scodec.*
import _root_.scodec.bits.BitVector
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.github.iltotore.iron.scodec.bits.*
import io.github.iltotore.iron.scodec.codecs.*
import utest.*

import scala.runtime.stdLibPatches.Predef.assert

object CodecSpec extends TestSuite:
  val tests: Tests = Tests:
    test("Codecs can be refined"):
      test("Boolean"):
        val hiCodec: Codec[Boolean :| High] = scodec.codecs.bool.refined
        val loCodec: Codec[Boolean :| Low] = scodec.codecs.bool.refined
        test - assert(loCodec.decodeValue(BitVector.low(1)).toOption.contains(false))
        test - assert(hiCodec.decodeValue(BitVector.low(1)).isFailure)
        test - assert(loCodec.decodeValue(BitVector.high(1)).isFailure)
        test - assert(hiCodec.decodeValue(BitVector.high(1)).toOption.contains(true))
      test("String"):
        val testCodec: Codec[String :| (LettersLowerCase & FixedLength[1])] = scodec.codecs.ascii.refined
        test("Valid input") - assert(testCodec.decodeValue(scodec.codecs.ascii.encode("a").require).toOption.contains("a"))
        test("Invalid input due to first constraint") - assert(testCodec.decodeValue(scodec.codecs.ascii.encode("A").require).isFailure)
        test("Invalid input due to second constraint") - assert(testCodec.decode(scodec.codecs.ascii.encode("aa").require).isFailure)
      test("BitVector"):
        val testCodec: Codec[BitVector :| (MinBitWidth[4L] & MaxBitWidth[8L])] = scodec.codecs.bits.refined
        test("Valid input") - assert(testCodec.decodeValue(BitVector.low(4)).toOption.contains(BitVector.low(4)))
        test("Invalid input") - assert(testCodec.decodeValue(BitVector.low(9L)).isFailure)
