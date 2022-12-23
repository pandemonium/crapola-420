package cp420

import org.scalacheck.*

object Arbitraries:
  given Arbitrary[RegisterName] =
    Arbitrary(Gen.oneOf(RegisterName.values))