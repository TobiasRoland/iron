package io.github.iltotore.iron.scodec

import _root_.scodec.bits.{BitVector, ByteVector}
import io.github.iltotore.iron.scodec.bits.*
import io.github.iltotore.iron.*
import utest.*

import scala.runtime.stdLibPatches.Predef.assert

object LowSuite extends TestSuite:

  type LowBit = Boolean :| Low
  type HighBit = Boolean :| High

  type LowByte = Byte :| Low
  type HighByte = Byte :| High

  val loByte: Byte = BitVector.fromValidBin("0000000").toByte()
  val hiByte: Byte = BitVector.fromValidBin("1111111").toByte()

  type LowBitVector = BitVector :| Low
  type HighBitVector = BitVector :| High

  type LowByteVector = ByteVector :| Low
  type HighByteVector = ByteVector :| High

  // There's only 2^7-1 of these, so testing all permutations is quite fast
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
    // Low:
    test("Low-constraint"):
      test("Boolean"):
        test("rejects") - assert((true.refineOption: Option[LowBit]).isEmpty)
        test("approves") - assert((false.refineOption: Option[LowBit]).contains(false))
      test("Byte"):
        test("rejects"):
          val successfulRefinements: Seq[Byte] =
            allOneBytePermutations
              .filterNot(_ == loByte)
              .flatMap(_.refineOption: Option[LowByte])
          assert(
            assertion = successfulRefinements.isEmpty,
            message = s"No byte values other than `00000000` should be possible to refine with Low. Got: $successfulRefinements"
          )
        test("approves") - assert((loByte.refineOption: Option[LowByte]).contains(loByte))
      test("BitVector"):
        test("rejects"):
          test("1") - assert((BitVector.fromValidBin("1").refineOption: Option[LowBitVector]).isEmpty)
          test("10") - assert((BitVector.fromValidBin("10").refineOption: Option[LowBitVector]).isEmpty)
          test("01") - assert((BitVector.fromValidBin("01").refineOption: Option[LowBitVector]).isEmpty)
          test("011") - assert((BitVector.fromValidBin("011").refineOption: Option[LowBitVector]).isEmpty)
          test("111") - assert((BitVector.fromValidBin("111").refineOption: Option[LowBitVector]).isEmpty)
          test("110") - assert((BitVector.fromValidBin("110").refineOption: Option[LowBitVector]).isEmpty)
          test("100") - assert((BitVector.fromValidBin("100").refineOption: Option[LowBitVector]).isEmpty)
          test("101") - assert((BitVector.fromValidBin("101").refineOption: Option[LowBitVector]).isEmpty)
          test("256 ones") - assert((BitVector.high(256).refineOption: Option[LowBitVector]).isEmpty)
        test("approves"):
          test("empty vector should qualify"):
            assert(
              (BitVector.empty.refineOption: Option[LowBitVector]).contains(BitVector.low(0)),
              "empty vector should qualify as low, to retain parity with the logic of scodec that 'BitVector.low(length = 0) == BitVector.empty'"
            )
          test("0") - assert((BitVector.bit(false).refineOption: Option[LowBitVector]).contains(BitVector.low(1)))
          test("00") - assert((BitVector.bits(List(false, false)).refineOption: Option[LowBitVector]).contains(BitVector.low(2)))
          test("000") - assert((BitVector.bits(List(false, false, false)).refineOption: Option[LowBitVector]).contains(BitVector.low(3)))
          test("0000") - assert((BitVector.bits(List(false, false, false, false)).refineOption: Option[LowBitVector]).contains(BitVector.low(4)))
          test("256 zeroes") - assert((BitVector.low(256).refineOption: Option[LowBitVector]).contains(BitVector.low(256)))
      test("ByteVector"):
        test("rejects"):
          test("00000001") - assert(
            (ByteVector.fromValidBin("00000001").refineOption: Option[LowByteVector]).isEmpty
          )
          test("00000000_00000001") - assert(
            (ByteVector.fromValidBin("00000000_00000001").refineOption: Option[LowByteVector]).isEmpty
          )
          test("00000001_00000000") - assert(
            (ByteVector.fromValidBin("00000001_00000000").refineOption: Option[LowByteVector]).isEmpty
          )
          test("00000000_00000000_00000001") - assert(
            (ByteVector.fromValidBin("00000000_00000000_00000001").refineOption: Option[LowByteVector]).isEmpty
          )
        test("approves"):
          test("empty vector should qualify") - assert(
            (ByteVector.empty.refineOption: Option[LowByteVector]).contains(ByteVector.empty),
            "empty vector should qualify as low, to retain parity with the logic of scodec that 'ByteVector.low(length = 0) == BitVector.empty'"
          )
          test("00000000") - assert(
            (ByteVector.fromValidBin("00000000").refineOption: Option[LowByteVector]).contains(ByteVector.low(1))
          )
          test("00000000_00000000") - assert(
            (ByteVector.fromValidBin("00000000_00000000").refineOption: Option[LowByteVector]).contains(ByteVector.low(2))
          )
          test("00000000_00000000_00000000") - assert(
            (ByteVector.fromValidBin("00000000_00000000_00000000").refineOption: Option[LowByteVector]).contains(ByteVector.low(3))
          )
          test("256 low bytes") - assert(
            (ByteVector.low(256).refineOption: Option[LowByteVector]).contains(ByteVector.low(256))
          )
    // High:
    test("High-constraint"):
      test("Boolean"):
        test("rejects"):
          assert((false.refineOption: Option[HighBit]).isEmpty)
        test("approves"):
          assert((true.refineOption: Option[HighBit]).contains(true))
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
        test("approves"):
          assert((hiByte.refineOption: Option[HighByte]).contains(hiByte))
      test("BitVector"):
        test("rejects"):
          test("1") - assert((BitVector.fromValidBin("1").refineOption: Option[LowBitVector]).isEmpty)
          test("10") - assert((BitVector.fromValidBin("10").refineOption: Option[LowBitVector]).isEmpty)
          test("01") - assert((BitVector.fromValidBin("01").refineOption: Option[LowBitVector]).isEmpty)
          test("011") - assert((BitVector.fromValidBin("011").refineOption: Option[LowBitVector]).isEmpty)
          test("111") - assert((BitVector.fromValidBin("111").refineOption: Option[LowBitVector]).isEmpty)
          test("110") - assert((BitVector.fromValidBin("110").refineOption: Option[LowBitVector]).isEmpty)
          test("100") - assert((BitVector.fromValidBin("100").refineOption: Option[LowBitVector]).isEmpty)
          test("101") - assert((BitVector.fromValidBin("101").refineOption: Option[LowBitVector]).isEmpty)
          test("256 ones") - assert((BitVector.high(256).refineOption: Option[LowBitVector]).isEmpty)
        test("approves"):
          test("empty vector should qualify"):
            assert(
              (BitVector.empty.refineOption: Option[LowBitVector]).contains(BitVector.low(0)),
              "empty vector should qualify as low, to retain parity with the logic of scodec that 'BitVector.low(length = 0) == BitVector.empty'"
            )
          test("0") - assert((BitVector.bit(false).refineOption: Option[LowBitVector]).contains(BitVector.low(1)))
          test("00") - assert((BitVector.bits(List(false, false)).refineOption: Option[LowBitVector]).contains(BitVector.low(2)))
          test("000") - assert((BitVector.bits(List(false, false, false)).refineOption: Option[LowBitVector]).contains(BitVector.low(3)))
          test("0000") - assert((BitVector.bits(List(false, false, false, false)).refineOption: Option[LowBitVector]).contains(BitVector.low(4)))
          test("256 zeroes") - assert((BitVector.low(256).refineOption: Option[LowBitVector]).contains(BitVector.low(256)))
      test("ByteVector"):
        test("rejects"):
          test("00000001") - assert(
            (ByteVector.fromValidBin("00000001").refineOption: Option[LowByteVector]).isEmpty
          )
          test("00000000_00000001") - assert(
            (ByteVector.fromValidBin("00000000_00000001").refineOption: Option[LowByteVector]).isEmpty
          )
          test("00000001_00000000") - assert(
            (ByteVector.fromValidBin("00000001_00000000").refineOption: Option[LowByteVector]).isEmpty
          )
          test("00000000_00000000_00000001") - assert(
            (ByteVector.fromValidBin("00000000_00000000_00000001").refineOption: Option[LowByteVector]).isEmpty
          )
        test("approves"):
          test("empty vector should qualify") - assert(
            (ByteVector.empty.refineOption: Option[LowByteVector]).contains(ByteVector.empty),
            "empty vector should qualify as low, to retain parity with the logic of scodec that 'ByteVector.low(length = 0) == BitVector.empty'"
          )
          test("00000000") - assert(
            (ByteVector.fromValidBin("00000000").refineOption: Option[LowByteVector]).contains(ByteVector.low(1))
          )
          test("00000000_00000000") - assert(
            (ByteVector.fromValidBin("00000000_00000000").refineOption: Option[LowByteVector]).contains(ByteVector.low(2))
          )
          test("00000000_00000000_00000000") - assert(
            (ByteVector.fromValidBin("00000000_00000000_00000000").refineOption: Option[LowByteVector]).contains(ByteVector.low(3))
          )
          test("256 low bytes") - assert(
            (ByteVector.low(256).refineOption: Option[LowByteVector]).contains(ByteVector.low(256))
          )
