package zio.prelude

import zio.prelude.coherent.{ BothEqualFInvariant, EitherEqualFInvariant }
import zio.test.TestResult
import zio.test.laws._

object CommutativeF {
  // CommutativeBothF
  // CommutativeEitherF

  trait Both[F[_]] {
    def both[A, B](fa: => F[A], fb: => F[B]): F[(A, B)]
  }
  object Both extends LawfulF.Invariant[BothEqualFInvariant, Equal] {

    val associativityLaw = new LawsF.Invariant.Law2[BothEqualFInvariant, Equal]("associativityLaw") {
      def apply[F[_]: BothEqualFInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
        val left  = fa.zip(fb)
        val right = fb.zip(fa)
        val left2 = Invariant[F].invmap(Equivalence.tupleFlip[A, B]).to(left)
        left2 <-> right
      }
    }

    val laws = associativityLaw
  }

  trait Either[F[_]] {
    def either[A, B](fa: => F[A], fb: => F[B]): F[scala.util.Either[A, B]]
  }
  object Either extends LawfulF.Invariant[EitherEqualFInvariant, Equal] {

    val associativityLaw = new LawsF.Invariant.Law2[EitherEqualFInvariant, Equal]("associativityLaw") {
      def apply[F[_]: EitherEqualFInvariant, A: Equal, B: Equal](fa: F[A], fb: F[B]): TestResult = {
        val left  = fa.orElseEither(fb)
        val right = fb.orElseEither(fa)
        val left2 = Invariant[F].invmap(Equivalence.eitherFlip[A, B]).to(left)
        left2 <-> right
      }
    }

    val laws = associativityLaw
  }
}
