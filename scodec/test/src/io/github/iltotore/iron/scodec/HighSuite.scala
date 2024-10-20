package io.github.iltotore.iron.scodec

import _root_.scodec.bits.{BitVector, ByteVector}
import io.github.iltotore.iron.scodec.bits.*
import io.github.iltotore.iron.*
import utest.*

import scala.runtime.stdLibPatches.Predef.assert

object HighSuite extends TestSuite:

  type HighBit = Boolean :| High
  type HighByte = Byte :| High
  type HighBitVector = BitVector :| High
  type HighByteVector = ByteVector :| High
  val hiByte: Byte = BitVector.fromValidBin("1111111").toByte()

  // There's only 2^7-1 of these, so not too bad to test all permutations.
  val allOneBytePermutations: Seq[Byte] =
    for
      a <- '0' to '1'
      b <- '0' to '1'
      c <- '0' to '1'
      d <- '0' to '1'
      e <- '0' to '1'
      f <- '0' to '1'
      g <- '0' to '1'
      h <- '0' to '1'
    yield BitVector.fromValidBin(s"$a$b$c$d$e$f$g$h").toByte()

  val tests: Tests = Tests:
    test("High"):

      /* Boolean */
      test("Boolean"):
        test("rejects") - assert:
          (false.refineOption: Option[HighBit]).isEmpty
        test("approves") - assert:
          (true.refineOption: Option[HighBit]).contains(true)

      /* Byte */
      test("Byte"):
        test("rejects"):
          val successfulRefinements: Seq[Byte] =
            allOneBytePermutations
              .filterNot(_ == hiByte)
              .flatMap(_.refineOption: Option[HighByte])
          assert(
            assertion = successfulRefinements.isEmpty,
            message =
              s"No byte values other than `11111111` should be possible to refine with High, got: ${successfulRefinements.mkString}"
          )
        test("approves") - assert:
          (hiByte.refineOption: Option[HighByte]).contains(hiByte)

      /* BitVector */
      test("BitVector"):
        test("rejects"):
          test("0") - assert:
            (BitVector.fromValidBin("0").refineOption: Option[HighBitVector]).isEmpty
          test("10") - assert:
            (BitVector.fromValidBin("10").refineOption: Option[HighBitVector]).isEmpty
          test("01") - assert:
            (BitVector.fromValidBin("01").refineOption: Option[HighBitVector]).isEmpty
          test("011") - assert:
            (BitVector.fromValidBin("011").refineOption: Option[HighBitVector]).isEmpty
          test("000") - assert:
            (BitVector.fromValidBin("000").refineOption: Option[HighBitVector]).isEmpty
          test("110") - assert:
            (BitVector.fromValidBin("110").refineOption: Option[HighBitVector]).isEmpty
          test("100") - assert:
            (BitVector.fromValidBin("100").refineOption: Option[HighBitVector]).isEmpty
          test("101") - assert:
            (BitVector.fromValidBin("101").refineOption: Option[HighBitVector]).isEmpty
          test("256 zeroes") - assert:
            (BitVector.low(256).refineOption: Option[HighBitVector]).isEmpty
        test("approves"):
          test("empty vector should qualify"):
            // TODO double check that this is reasonable
            assert(
              (BitVector.empty.refineOption: Option[HighBitVector]).contains(BitVector.high(0)),
              "empty vector should qualify as high, to retain parity with the logic of scodec that 'BitVector.high(length = 0) == BitVector.empty'"
            )
          test("1") - assert:
            (BitVector.fromValidBin("1").refineOption: Option[HighBitVector]).contains(BitVector.high(1))
          test("11") - assert:
            (BitVector.fromValidBin("11").refineOption: Option[HighBitVector]).contains(BitVector.high(2))
          test("111") - assert:
            (BitVector.fromValidBin("111").refineOption: Option[HighBitVector]).contains(BitVector.high(3))
          test("1111") - assert:
            (BitVector.fromValidBin("1111").refineOption: Option[HighBitVector]).contains(BitVector.high(4))
          test("256 ones") - assert:
            (BitVector.high(256).refineOption: Option[HighBitVector]).contains(BitVector.high(256))

      /* ByteVector */
      test("ByteVector"):
        test("rejects"):
          test("11111110") - assert:
            (ByteVector.fromValidBin("11111110").refineOption: Option[HighByteVector]).isEmpty
          test("11111111_11111110") - assert(
            (ByteVector.fromValidBin("11111111_11111110").refineOption: Option[HighByteVector]).isEmpty
          )
          test("11111110_11111111") - assert(
            (ByteVector.fromValidBin("11111110_11111111").refineOption: Option[HighByteVector]).isEmpty
          )
          test("11111111_11111110_1111111") - assert(
            (ByteVector.fromValidBin("11111111_11111110_11111111").refineOption: Option[HighByteVector]).isEmpty
          )
        test("approves"):
          test("empty vector should qualify") - assert( // TODO double check this is reasonable logic
            (ByteVector.empty.refineOption: Option[HighByteVector]).contains(ByteVector.empty),
            "empty vector should qualify as low, to retain parity with the logic of scodec that 'ByteVector.low(length = 0) == BitVector.empty'"
          )
          test("11111111") - assert:
            (ByteVector.fromValidBin("11111111").refineOption: Option[HighByteVector]).contains(ByteVector.high(1))
          test("11111111_11111111") - assert:
            (ByteVector.fromValidBin("11111111_11111111").refineOption: Option[HighByteVector]).contains(ByteVector.high(2))
          test("11111111_11111111_11111111") - assert(
            (ByteVector.fromValidBin("11111111_11111111_11111111").refineOption: Option[HighByteVector]).contains(ByteVector.high(3))
          )
          test("256 high bytes") - assert:
            (ByteVector.high(256).refineOption: Option[HighByteVector]).contains(ByteVector.high(256))
