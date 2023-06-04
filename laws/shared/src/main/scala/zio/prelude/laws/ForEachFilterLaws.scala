package zio.prelude
package laws

import zio.prelude.Equal
import zio.prelude.coherent.DeriveEqualForEachFilter
import zio.test.laws._

object ForEachFilterLaws extends LawfulF.Covariant[DeriveEqualForEachFilter, Equal] {
  lazy val laws: LawsF.Covariant[DeriveEqualForEachFilter, Equal] =
    CovariantFilterLaws.laws
}
