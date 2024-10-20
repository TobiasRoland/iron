package io.github.iltotore.iron.scodec

import io.github.iltotore.iron.compileTime.+
import io.github.iltotore.iron.constraint.any.DescribedAs
import io.github.iltotore.iron.macros.reflectUtil
import io.github.iltotore.iron.scodec.bits.*

import scala.compiletime.ops.long.{^, `*`}

/**
 * Common units and sizes to make working with refined bit/byte vectors legible.
 *
 * For example: ```type Payload = BitVector :| MinKiB[126L] :| MaxGiB[3L]```
 */
object units:

  object all:
    export metric.*
    export binary.*

  /**
   * Standard metric (SI) convention unit sizes up to Terabyte; for binary sizes, see [[binary]].
   */
  object metric:

    // format: off
    type MinKilobit[V <: Long]   = DescribedAs[MinBitWidth[V * (10L ^ 3L)],          "Should have minimum width of " + V + " kilobits (" + (V * (10L ^ 3L))        + "bits)"]
    type MinMegabit[V <: Long]   = DescribedAs[MinBitWidth[V * (10L ^ 6L)],          "Should have minimum width of " + V + " megabits (" + (V * (10L ^ 6L))        + "bits)"]
    type MinGigabit[V <: Long]   = DescribedAs[MinBitWidth[V * (10L ^ 9L)],          "Should have minimum width of " + V + " gigabits (" + (V * (10L ^ 9L))        + "bits)"]
    type MinTerabit[V <: Long]   = DescribedAs[MinBitWidth[V * (10L ^ 12L)],         "Should have minimum width of " + V + " terabits (" + (V * (10L ^ 12L))       + "bits)"]
    type MaxKilobit[V <: Long]   = DescribedAs[MaxBitWidth[V * (10L ^ 3L)],          "Should have maximum width of " + V + " kilobits (" + (V * (10L ^ 3L))        + "bits)"]
    type MaxMegabit[V <: Long]   = DescribedAs[MaxBitWidth[V * (10L ^ 6L)],          "Should have maximum width of " + V + " megabits (" + (V * (10L ^ 6L))        + "bits)"]
    type MaxGigabit[V <: Long]   = DescribedAs[MaxBitWidth[V * (10L ^ 9L)],          "Should have maximum width of " + V + " gigabits (" + (V * (10L ^ 9L))        + "bits)"]
    type MaxTerabit[V <: Long]   = DescribedAs[MaxBitWidth[V * (10L ^ 12L)],         "Should have maximum width of " + V + " terabits (" + (V * (10L ^ 12L))       + "bits)"]
    type FixedKilobit[V <: Long] = DescribedAs[FixedBitWidth[V * (10L ^ 3L)],          "Should have fixed width of "   + V + " kilobits (" + (V * (10L ^ 3L))        + "bits)"]
    type FixedMegabit[V <: Long] = DescribedAs[FixedBitWidth[V * (10L ^ 6L)],          "Should have fixed width of "   + V + " megabits (" + (V * (10L ^ 6L))        + "bits)"]
    type FixedGigabit[V <: Long] = DescribedAs[FixedBitWidth[V * (10L ^ 9L)],          "Should have fixed width of "   + V + " gigabits (" + (V * (10L ^ 9L))        + "bits)"]
    type FixedTerabit[V <: Long] = DescribedAs[FixedBitWidth[V * (10L ^ 12L)],         "Should have fixed width of "   + V + " terabits (" + (V * (10L ^ 12L))       + "bits)"]
    type MinKB[V <: Long]        = DescribedAs[MinBitWidth[V * 8L * (10L ^ 3L)],     "Should have minimum width of " + V + " kilobytes (" + (V * 8L * (10L ^ 3L))  + "bits)"]
    type MinMB[V <: Long]        = DescribedAs[MinBitWidth[V * 8L * (10L ^ 6L)],     "Should have minimum width of " + V + " megabytes (" + (V * 8L * (10L ^ 6L))  + "bits)"]
    type MinGB[V <: Long]        = DescribedAs[MinBitWidth[V * 8L * (10L ^ 9L)],     "Should have minimum width of " + V + " gigabytes (" + (V * 8L * (10L ^ 9L))  + "bits)"]
    type MinTB[V <: Long]        = DescribedAs[MinBitWidth[V * 8L * (10L ^ 12L)],    "Should have minimum width of " + V + " terabytes (" + (V * 8L * (10L ^ 12L)) + "bits)"]
    type MaxKB[V <: Long]        = DescribedAs[MaxBitWidth[V * 8L * (10L ^ 3L)],     "Should have maximum width of " + V + " kilobytes (" + (V * 8L * (10L ^ 3L))  + "bits)"]
    type MaxMB[V <: Long]        = DescribedAs[MaxBitWidth[V * 8L * (10L ^ 6L)],     "Should have maximum width of " + V + " megabytes (" + (V * 8L * (10L ^ 6L))  + "bits)"]
    type MaxGB[V <: Long]        = DescribedAs[MaxBitWidth[V * 8L * (10L ^ 9L)],     "Should have maximum width of " + V + " gigabytes (" + (V * 8L * (10L ^ 9L))  + "bits)"]
    type MaxTB[V <: Long]        = DescribedAs[MaxBitWidth[V * 8L * (10L ^ 12L)],    "Should have maximum width of " + V + " terabytes (" + (V * 8L * (10L ^ 12L)) + "bits)"]
    type FixedKB[V <: Long]      = DescribedAs[FixedBitWidth[V * 8L * (10L ^ 3L)],   "Should have fixed width of "   + V + " kilobytes (" + (V * 8L * (10L ^ 3L))  + "bits)"]
    type FixedMB[V <: Long]      = DescribedAs[FixedBitWidth[V * 8L * (10L ^ 6L)],   "Should have fixed width of "   + V + " megabytes (" + (V * 8L * (10L ^ 6L))  + "bits)"]
    type FixedGB[V <: Long]      = DescribedAs[FixedBitWidth[V * 8L * (10L ^ 9L)],   "Should have fixed width of "   + V + " gigabytes (" + (V * 8L * (10L ^ 9L))  + "bits)"]
    type FixedTB[V <: Long]      = DescribedAs[FixedBitWidth[V * 8L * (10L ^ 12L)],  "Should have fixed width of "   + V + " terabytes (" + (V * 8L * (10L ^ 12L)) + "bits)"]
    // format: on

  /** Standard widths up to TiB; for metric (SI) widths, see [[metric]] */
  object binary:

    // format: off
    type MinKibibit[V <: Long]   = DescribedAs[MinBitWidth[V * (2L ^ 10L)],        "Should have minimum width of " + V + " kibibits ("  + (V * (2L ^ 10L))      + " bits)"]
    type MinMebibit[V <: Long]   = DescribedAs[MinBitWidth[V * (2L ^ 20L)],        "Should have minimum width of " + V + " mebibits ("  + (V * (2L ^ 20L))      + " bits)"]
    type MinGibibit[V <: Long]   = DescribedAs[MinBitWidth[V * (2L ^ 30L)],        "Should have minimum width of " + V + " gibibits ("  + (V * (2L ^ 30L))      + " bits)"]
    type MinTebibit[V <: Long]   = DescribedAs[MinBitWidth[V * (2L ^ 40L)],        "Should have minimum width of " + V + " tebibits ("  + (V * (2L ^ 40L))      + " bits)"]
    type MaxKibibit[V <: Long]   = DescribedAs[MaxBitWidth[V * (2L ^ 10L)],        "Should have maximum width of " + V + " kibibits ("  + (V * (2L ^ 10L))      + " bits)"]
    type MaxMebibit[V <: Long]   = DescribedAs[MaxBitWidth[V * (2L ^ 20L)],        "Should have maximum width of " + V + " mebibits ("  + (V * (2L ^ 20L))      + " bits)"]
    type MaxGibibit[V <: Long]   = DescribedAs[MaxBitWidth[V * (2L ^ 30L)],        "Should have maximum width of " + V + " gibibits ("  + (V * (2L ^ 30L))      + " bits)"]
    type MaxTebibit[V <: Long]   = DescribedAs[MaxBitWidth[V * (2L ^ 40L)],        "Should have maximum width of " + V + " tebibits ("  + (V * (2L ^ 40L))      + " bits)"]
    type FixedKibibit[V <: Long] = DescribedAs[MinBitWidth[V * (2L ^ 10L)],        "Should have fixed width of "   + V + " kibibits ("  + (V * (2L ^ 10L))      + " bits)"]
    type FixedMebibit[V <: Long] = DescribedAs[MinBitWidth[V * (2L ^ 20L)],        "Should have fixed width of "   + V + " mebibits ("  + (V * (2L ^ 20L))      + " bits)"]
    type FixedGibibit[V <: Long] = DescribedAs[MinBitWidth[V * (2L ^ 30L)],        "Should have fixed width of "   + V + " gibibits ("  + (V * (2L ^ 30L))      + " bits)"]
    type FixedTebibit[V <: Long] = DescribedAs[MinBitWidth[V * (2L ^ 40L)],        "Should have fixed width of "   + V + " tebibits ("  + (V * (2L ^ 40L))      + " bits)"]
    type MinKiB[V <: Long]       = DescribedAs[MinBitWidth[V * 8L * (2L ^ 10L)],   "Should have minimum width of " + V + " kibibytes (" + (V * 8L * (2L ^ 10L)) + " bits)"]
    type MinMiB[V <: Long]       = DescribedAs[MinBitWidth[V * 8L * (2L ^ 20L)],   "Should have minimum width of " + V + " mebibytes (" + (V * 8L * (2L ^ 20L)) + " bits)"]
    type MinGiB[V <: Long]       = DescribedAs[MinBitWidth[V * 8L * (2L ^ 30L)],   "Should have minimum width of " + V + " gibibytes (" + (V * 8L * (2L ^ 30L)) + " bits)"]
    type MinTiB[V <: Long]       = DescribedAs[MinBitWidth[V * 8L * (2L ^ 40L)],   "Should have minimum width of " + V + " tebibytes (" + (V * 8L * (2L ^ 40L)) + " bits)"]
    type MaxKiB[V <: Long]       = DescribedAs[MaxBitWidth[V * 8L * (2L ^ 10L)],   "Should have maximum width of " + V + " kibibytes (" + (V * 8L * (2L ^ 10L)) + " bits)"]
    type MaxMiB[V <: Long]       = DescribedAs[MaxBitWidth[V * 8L * (2L ^ 20L)],   "Should have maximum width of " + V + " mebibytes (" + (V * 8L * (2L ^ 20L)) + " bits)"]
    type MaxGiB[V <: Long]       = DescribedAs[MaxBitWidth[V * 8L * (2L ^ 30L)],   "Should have maximum width of " + V + " gibibytes (" + (V * 8L * (2L ^ 30L)) + " bits)"]
    type MaxTiB[V <: Long]       = DescribedAs[MaxBitWidth[V * 8L * (2L ^ 40L)],   "Should have maximum width of " + V + " tebibytes (" + (V * 8L * (2L ^ 40L)) + " bits)"]
    type FixedKiB[V <: Long]     = DescribedAs[FixedBitWidth[V * 8L * (2L ^ 10L)], "Should have fixed width of "   + V + " kibibytes (" + (V * 8L * (2L ^ 10L)) + " bits)"]
    type FixedMiB[V <: Long]     = DescribedAs[FixedBitWidth[V * 8L * (2L ^ 20L)], "Should have fixed width of "   + V + " mebibytes (" + (V * 8L * (2L ^ 20L)) + " bits)"]
    type FixedGiB[V <: Long]     = DescribedAs[FixedBitWidth[V * 8L * (2L ^ 30L)], "Should have fixed width of "   + V + " gibibytes (" + (V * 8L * (2L ^ 30L)) + " bits)"]
    type FixedTiB[V <: Long]     = DescribedAs[FixedBitWidth[V * 8L * (2L ^ 40L)], "Should have fixed width of "   + V + " tebibytes (" + (V * 8L * (2L ^ 40L)) + " bits)"]
