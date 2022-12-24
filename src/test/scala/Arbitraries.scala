package cp420

import org.scalacheck.*

object Arbitraries:
  given Arbitrary[RegisterName] =
    Arbitrary(Gen.oneOf(RegisterName.values.toIndexedSeq))

  given Arbitrary[AluOperator] =
    Arbitrary(Gen.oneOf(AluOperator.values.toIndexedSeq))

  given Arbitrary[Flag] =
    Arbitrary(Gen.oneOf(Flag.values.toIndexedSeq))