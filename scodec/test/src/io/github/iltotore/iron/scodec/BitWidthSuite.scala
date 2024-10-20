package io.github.iltotore.iron.scodec

import _root_.scodec.bits.{BitVector, ByteVector}
import io.github.iltotore.iron.scodec.bits.*
import io.github.iltotore.iron.*
import utest.*

import scala.runtime.stdLibPatches.Predef.assert

object BitWidthSuite extends TestSuite:

  val tests: Tests = Tests:
    /* BitVector */
    test("Bitvector"):
      test("Min"):
        test("<"):
          test("0") - assert((BitVector.low(0).refineOption: Option[BitVector :| MinBitWidth[2L]]).isEmpty)
          test("1") - assert((BitVector.low(1).refineOption: Option[BitVector :| MinBitWidth[2L]]).isEmpty)
        test(">="):
          test("2") - assert((BitVector.low(2).refineOption: Option[BitVector :| MinBitWidth[2L]]).contains(BitVector.low(2)))
          test("3") - assert((BitVector.low(3).refineOption: Option[BitVector :| MinBitWidth[2L]]).contains(BitVector.low(3)))
          test("256") - assert((BitVector.low(256).refineOption: Option[BitVector :| MinBitWidth[2L]]).contains(BitVector.low(256)))
      test("Max"):
        test("<="):
          test("0") - assert((BitVector.low(0).refineOption: Option[BitVector :| MaxBitWidth[2L]]).contains(BitVector.empty))
          test("1") - assert((BitVector.low(1).refineOption: Option[BitVector :| MaxBitWidth[2L]]).contains(BitVector.low(1)))
          test("2") - assert((BitVector.low(2).refineOption: Option[BitVector :| MaxBitWidth[2L]]).contains(BitVector.low(2)))
        test(">"):
          test("3") - assert((BitVector.low(3).refineOption: Option[BitVector :| MaxBitWidth[2L]]).isEmpty)
          test("256") - assert((BitVector.low(256).refineOption: Option[BitVector :| MaxBitWidth[2L]]).isEmpty)
      test("Fixed"):
        test("<"):
          test("0") - assert((BitVector.low(0).refineOption: Option[BitVector :| FixedBitWidth[2L]]).isEmpty)
          test("1") - assert((BitVector.low(1).refineOption: Option[BitVector :| FixedBitWidth[2L]]).isEmpty)
        test("=="):
          test("2") - assert((BitVector.low(2).refineOption: Option[BitVector :| FixedBitWidth[2L]]).contains(BitVector.low(2)))
        test(">"):
          test("3") - assert((BitVector.low(3).refineOption: Option[BitVector :| FixedBitWidth[2L]]).isEmpty)
          test("256") - assert((BitVector.low(256).refineOption: Option[BitVector :| FixedBitWidth[2L]]).isEmpty)

      /* ByteVector */
    test("ByteVector"):
      test("Min"):
        test("<"):
          test("0") - assert((ByteVector.low(0).refineOption: Option[ByteVector :| MinBitWidth[9L]]).isEmpty)
          test("8") - assert((ByteVector.low(1).refineOption: Option[ByteVector :| MinBitWidth[9L]]).isEmpty)
        test(">="):
          test("16") - assert((ByteVector.low(2).refineOption: Option[ByteVector :| MinBitWidth[9L]]).contains(ByteVector.low(2)))
          test("24") - assert((ByteVector.low(3).refineOption: Option[ByteVector :| MinBitWidth[9L]]).contains(ByteVector.low(3)))
          test("256") - assert((ByteVector.low(32).refineOption: Option[ByteVector :| MinBitWidth[9L]]).contains(ByteVector.low(32)))
      test("Max"):
        test("<="):
          test("0") - assert((ByteVector.low(0).refineOption: Option[ByteVector :| MaxBitWidth[9L]]).contains(ByteVector.empty))
          test("8") - assert((ByteVector.low(1).refineOption: Option[ByteVector :| MaxBitWidth[9L]]).contains(ByteVector.low(1)))
        test(">"):
          test("16") - assert((ByteVector.low(2).refineOption: Option[ByteVector :| MaxBitWidth[9L]]).isEmpty)
          test("24") - assert((ByteVector.low(3).refineOption: Option[ByteVector :| MaxBitWidth[9L]]).isEmpty)
          test("256") - assert((ByteVector.low(256).refineOption: Option[ByteVector :| MaxBitWidth[9L]]).isEmpty)
      test("Fixed"):
        test("<"):
          test("0") - assert((ByteVector.low(0).refineOption: Option[ByteVector :| FixedBitWidth[16L]]).isEmpty)
          test("8") - assert((ByteVector.low(1).refineOption: Option[ByteVector :| FixedBitWidth[16L]]).isEmpty)
        test("==")
        test("2") - assert((ByteVector.low(2).refineOption: Option[ByteVector :| FixedBitWidth[16L]]).contains(ByteVector.low(2)))
        test(">"):
          test("3") - assert((ByteVector.low(3).refineOption: Option[ByteVector :| FixedBitWidth[16L]]).isEmpty)
          test("256") - assert((ByteVector.low(256).refineOption: Option[ByteVector :| FixedBitWidth[16L]]).isEmpty)
