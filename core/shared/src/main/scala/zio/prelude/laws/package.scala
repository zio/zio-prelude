package zio.prelude

import zio.URIO
import zio.test.laws.GenF
import zio.test.{ Gen, TestConfig, TestResult }

package object laws {
  object LawfulF {
    type Traversable[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]] = ZLawfulF.Traversable[CapsF, CapsG, Caps, Any]
  }

  object LawsF {
    type Traversable[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]] = ZLawsF.Traversable[CapsF, CapsG, Caps, Any]

    object Covariant {
      type MapLaw[-CapsF[_[+_]], -Caps[_]] = ZLawsF.Covariant.MapLaw[CapsF, Caps]
    }

    object Traversable {
      type PurityLaw[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]]           =
        ZLawsF.Traversable.PurityLaw[CapsF, CapsG, Caps]
      type SequentialFusionLaw[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]] =
        ZLawsF.Traversable.SequentialFusionLaw[CapsF, CapsG, Caps]
      type NaturalityFusionLaw[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_]] =
        ZLawsF.Traversable.NaturalityFusionLaw[CapsF, CapsG, Caps]
    }
  }

  def checkAllLaws[CapsF[_[+_]], CapsG[_[+_]], Caps[_], R <: TestConfig, R1 <: R, F[+_]: CapsF, G[+_]: CapsG, A: Caps](
    lawful: ZLawfulF.Traversable[CapsF, CapsG, Caps, R]
  )(genF: GenF[R1, F], genG: GenF[R1, G], gen: Gen[R1, A]): URIO[R1, TestResult] =
    lawful.laws.run(genF, genG, gen)
}
