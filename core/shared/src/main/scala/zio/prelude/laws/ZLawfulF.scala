package zio.prelude.laws

object ZLawfulF {

  /** `ZLawful` for traversable type constructors. */
  trait Traversable[-CapsF[_[+_]], -CapsG[_[+_]], -Caps[_], -R] { self =>
    def laws: ZLawsF.Traversable[CapsF, CapsG, Caps, R]
    def +[CapsF1[x[+_]] <: CapsF[x], CapsG1[x[+_]] <: CapsG[x], Caps1[x] <: Caps[x], R1 <: R](
      that: Traversable[CapsF1, CapsG1, Caps1, R1]
    ): Traversable[CapsF1, CapsG1, Caps1, R1] =
      new Traversable[CapsF1, CapsG1, Caps1, R1] {
        val laws = self.laws + that.laws
      }
  }
}
