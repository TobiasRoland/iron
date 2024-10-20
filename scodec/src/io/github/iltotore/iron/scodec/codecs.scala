package io.github.iltotore.iron.scodec

import _root_.scodec.*
import io.github.iltotore.iron.*
import io.github.iltotore.iron.macros.reflectUtil

import scala.compiletime.{constValue, summonInline}

object codecs:

  extension [A](codec: Codec[A])
    inline def refined[C](using inline constraint: Constraint[A, C]): Codec[A :| C] =
      codec.exmap(
        f = _.refineEither[C] match
          case Right(refined) => Attempt.successful(refined)
          case Left(reason)   => Attempt.failure(Err(s"Codec could not refine value after decoding. $reason")), // A => A :| C
        g = Attempt.successful // A :| C => A, and as A is always a subtype of A :| C, this is always successful)
      )

  extension [A](decoder: Decoder[A])
    // different function name since Codec extends Decoder, and it would get confusing otherwise.
    inline def refinedDecoder[C](using inline constraint: Constraint[A, C]): Decoder[A :| C] =
      decoder.emap(
        _.refineEither[C] match
          case Right(refined) => Attempt.successful(refined)
          case Left(reason)   => Attempt.failure(Err(s"Decoder could not refine value after decoding. $reason"))
      )
