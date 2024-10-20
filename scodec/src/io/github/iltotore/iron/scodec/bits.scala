package io.github.iltotore.iron.scodec

import _root_.scodec.bits.{BitVector, ByteVector}
import io.github.iltotore.iron.*
import io.github.iltotore.iron.compileTime.*
import io.github.iltotore.iron.constraint.any.DescribedAs
import io.github.iltotore.iron.constraint.numeric.{GreaterEqual, LessEqual}
import io.github.iltotore.iron.macros.reflectUtil

import scala.compiletime.{constValue, summonInline}
import scala.quoted.*

/**
 * Scodec-related constraints.
 *
 * Also includes bit specific terminology for [[Byte]] and [[Boolean]].
 */
object bits: // mirrors scodecs classification

  /** Tests that all bits represented by the type should be low (0) */
  type Low

  /** Tests that all bits represented by the type should be high (1) */
  type High

  /**
   * Tests that the sequence of bits or bytes is of expected length. Supports [[BitVector]] and [[ByteVector]] by default.
   *
   * @tparam C - the constraint you require on the length
   */
  type BitWidth[C]

  /**
   * Tests minimum binary width. Supports [[BitVector]] and [[ByteVector]] by default.
   *
   * @tparam V the minimum width of the tested input
   */
  type MinBitWidth[V <: Long] = DescribedAs[BitWidth[GreaterEqual[V]], "Should have minimum width of " + V + " bits"]

  /**
   * Tests maximum width. Supports [[BitVector]] and [[ByteVector]] by default.
   *
   * @tparam V the maximum width of the tested input
   */
  type MaxBitWidth[V <: Long] = DescribedAs[BitWidth[LessEqual[V]], "Should have maximum width of " + V + " bits"]

  /**
   * Tests exact width. Supports [[BitVector]] and [[ByteVector]] by default.
   *
   * @tparam V the exact width in bits of the tested input
   */
  type FixedBitWidth[V <: Long] = DescribedAs[BitWidth[StrictEqual[V]], "Should have fixed width of " + V + " bits"]

  type MinBytes[V <: Long] = DescribedAs[MinBitWidth[V * 8L], "Should have minimum width of " + V + " bytes (" + (V * 8L) + " bits)"]
  type MaxBytes[V <: Long] = DescribedAs[MaxBitWidth[V * 8L], "Should have maximum width of " + V + " bytes (" + (V * 8L) + " bits)"]
  type FixedBytes[V <: Long] = DescribedAs[FixedBitWidth[V * 8L], "Should have fixed width of " + V + " bytes (" + (V * 8L) + " bits)"]

  /** Tests that the input is empty. Supports [[BitVector]] and [[ByteVector]] by default. */
  type NoWidth = DescribedAs[FixedBitWidth[0L], "Should be empty"]

  /** 'Low' meaning "all bits set to 0" */
  object Low:
    private trait LowConstraint[A] extends Constraint[A, Low]:
      override inline def message: String = "Should be 'low', meaning all bits set to 0"

    inline given LowConstraint[BitVector] with
      override inline def test(inline value: BitVector): Boolean = value.populationCount == 0

    inline given LowConstraint[Boolean] with
      override inline def test(inline value: Boolean): Boolean = !value

    inline given LowConstraint[Byte] with
      override inline def test(inline value: Byte): Boolean = summon[LowConstraint[BitVector]].test(BitVector.fromByte(value))

    inline given LowConstraint[ByteVector] with
      override inline def test(inline value: ByteVector): Boolean = summon[LowConstraint[BitVector]].test(value.toBitVector)

  /** 'High' meaning "all bits set to 1" */
  object High:
    private trait HighConstraint[A] extends Constraint[A, High]:
      override inline def message: String = "Should be 'high', meaning all bits set to 1"

    inline given HighConstraint[BitVector] with
      override inline def test(inline value: BitVector): Boolean = value.populationCount == value.size

    inline given HighConstraint[Boolean] with
      override inline def test(inline value: Boolean): Boolean = value

    inline given HighConstraint[Byte] with
      override inline def test(inline value: Byte): Boolean = summon[HighConstraint[BitVector]].test(BitVector.fromByte(value))

    inline given HighConstraint[ByteVector] with
      override inline def test(inline value: ByteVector): Boolean = summon[HighConstraint[BitVector]].test(value.toBitVector)

  /** Width, that is, length/size in bits of the underlying type being tested */
  object BitWidth:

    trait BitWidthConstraint

    class BitWidthBitVector[C, Impl <: Constraint[Long, C]](using Impl) extends Constraint[BitVector, BitWidth[C]]:
      override inline def test(inline value: BitVector): Boolean = ${ checkBitVector('value, '{ summonInline[Impl] }) }
      override inline def message: String = "BitWidth of BitVector: (" + summonInline[Impl].message + ")"

    class BitWidthByteVector[C, Impl <: Constraint[Long, C]](using Impl) extends Constraint[ByteVector, BitWidth[C]]:
      override inline def test(inline value: ByteVector): Boolean = ${ checkByteVector('value, '{ summonInline[Impl] }) }
      override inline def message: String = "BitWidth of ByteVector: (" + summonInline[Impl].message + ")"

    inline given bitWidthBitVector[C, Impl <: Constraint[Long, C]](using inline impl: Impl): BitWidthBitVector[C, Impl] = new BitWidthBitVector
    inline given bitWidthByteVector[C, Impl <: Constraint[Long, C]](using inline impl: Impl): BitWidthByteVector[C, Impl] = new BitWidthByteVector

    private def checkBitVector[C, Impl <: Constraint[Long, C]](expr: Expr[BitVector], constraintExpr: Expr[Impl])(using Quotes): Expr[Boolean] =
      val rflUtil = reflectUtil
      import rflUtil.*
      expr.decode match
        case Right(value: BitVector) => applyConstraint(Expr(value.size), constraintExpr)
        case _                       => applyConstraint('{ $expr.size }, constraintExpr)

    private def checkByteVector[C, Impl <: Constraint[Long, C]](expr: Expr[ByteVector], constraintExpr: Expr[Impl])(using Quotes): Expr[Boolean] =
      val rflUtil = reflectUtil
      import rflUtil.*
      // * 8 because ByteVector's size is in Bytes (of size 8)
      expr.decode match
        case Right(value: ByteVector) => applyConstraint(Expr(value.size * 8L), constraintExpr)
        case _                        => applyConstraint('{ $expr.size * 8L }, constraintExpr)

    given [C1, C2](using C1 ==> C2): (BitWidth[C1] ==> BitWidth[C2]) = Implication()
