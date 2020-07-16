package zio.prelude

import zio.{ Chunk, NonEmptyChunk }

/**
 * `EqualF[F]` represents a universally quantified function from `Equal[A]` to
 * `Equal[F[A]]` for some `F[_]`. You can think of `EqualF` as a "recipe" for
 * building an `EqualF[A]]` instance given an `Equal[A]`.
 *
 * For example, if we know how to compare values of type `A` for equality then
 * we can compare lists with elements of type `A` for equality by checking
 * that the length of the lists is the same and each pair of corresponding
 * elements are equal. And we can do this for any type `A` as long as it has
 * an `Equal` instance.
 *
 * This is used by the library to derive `Equal` instances for higher kinded
 * types given `Equal` instances for the type they are parameterized on.
 */
trait EqualF[F[_]] {

  /**
   * Derive an `Equal[F[A]]` given an `Equal[A]`.
   */
  def deriveEqual[A: Equal]: Equal[F[A]]
}

object EqualF {

  /**
   * Summon an implicit `EqualF[F]`
   */
  def apply[F[_]](implicit equalF: EqualF[F]): EqualF[F] =
    equalF

  /**
   * The `EqualF` instance for `Chunk`.
   */
  implicit val ChunkEqualF: EqualF[Chunk] =
    new EqualF[Chunk] {
      def deriveEqual[A: Equal]: Equal[Chunk[A]] =
        Equal.ChunkEqual
    }

  /**
   * The `EqualF` instance for `List`.
   */
  implicit val ListEqualF: EqualF[List] =
    new EqualF[List] {
      def deriveEqual[A: Equal]: Equal[List[A]] =
        Equal.ListEqual
    }

  /**
   * The `EqualF` instance for `Either`.
   */
  implicit def EitherEqualF[E: Equal]: EqualF[({ type lambda[+x] = Either[E, x] })#lambda] =
    new EqualF[({ type lambda[+x] = Either[E, x] })#lambda] {
      def deriveEqual[A: Equal]: Equal[Either[E, A]] =
        Equal.EitherEqual
    }

  /**
   * The `EqualF` instance for `Map`.
   */
  implicit def MapEqualF[A]: EqualF[({ type lambda[+x] = Map[A, x] })#lambda] =
    new EqualF[({ type lambda[+x] = Map[A, x] })#lambda] {
      def deriveEqual[B: Equal]: Equal[Map[A, B]] =
        Equal.MapEqual
    }

  /**
   * The `EqualF` instance for  `NonEmptyChunk`.
   */
  implicit val NonEmptyChunkEqualF: EqualF[NonEmptyChunk] = new EqualF[NonEmptyChunk] {
    def deriveEqual[A: Equal]: Equal[NonEmptyChunk[A]] =
      Equal.NonEmptyChunkEqual
  }

  /**
   * The `EqualF` instance for `Option`.
   */
  implicit val OptionEqualF: EqualF[Option] =
    new EqualF[Option] {
      def deriveEqual[A: Equal]: Equal[Option[A]] =
        Equal.OptionEqual
    }

  /**
   * The `EqualF` instance for `Set`.
   */
  implicit def SetEqualF[A]: EqualF[({ type lambda[x] = Set[A] })#lambda] =
    new EqualF[({ type lambda[x] = Set[A] })#lambda] {
      def deriveEqual[B: Equal]: Equal[Set[A]] =
        Equal.SetEqual
    }

  /**
   * The `EqualF` instance for `Tuple2`.
   */
  implicit def Tuple2EqualF[A: Equal]: EqualF[({ type lambda[x] = (A, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, x) })#lambda] {
      def deriveEqual[B: Equal]: Equal[(A, B)] =
        Equal.Tuple2Equal
    }

  /**
   * The `EqualF` instance for `Tuple3`.
   */
  implicit def Tuple3EqualF[A: Equal, B: Equal]: EqualF[({ type lambda[x] = (A, B, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, x) })#lambda] {
      def deriveEqual[C: Equal]: Equal[(A, B, C)] =
        Equal.Tuple3Equal
    }

  /**
   * The `EqualF` instance for `Tuple4`.
   */
  implicit def Tuple4EqualF[A: Equal, B: Equal, C: Equal]: EqualF[({ type lambda[x] = (A, B, C, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, x) })#lambda] {
      def deriveEqual[D: Equal]: Equal[(A, B, C, D)] =
        Equal.Tuple4Equal
    }

  /**
   * The `EqualF` instance for `Tuple5`.
   */
  implicit def Tuple5EqualF[A: Equal, B: Equal, C: Equal, D: Equal]
    : EqualF[({ type lambda[x] = (A, B, C, D, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, x) })#lambda] {
      def deriveEqual[E: Equal]: Equal[(A, B, C, D, E)] =
        Equal.Tuple5Equal
    }

  /**
   * The `EqualF` instance for `Tuple6`.
   */
  implicit def Tuple6EqualF[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal]
    : EqualF[({ type lambda[x] = (A, B, C, D, E, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, x) })#lambda] {
      def deriveEqual[F: Equal]: Equal[(A, B, C, D, E, F)] =
        Equal.Tuple6Equal
    }

  /**
   * The `EqualF` instance for `Tuple7`.
   */
  implicit def Tuple7EqualF[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal]
    : EqualF[({ type lambda[x] = (A, B, C, D, E, F, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, x) })#lambda] {
      def deriveEqual[G: Equal]: Equal[(A, B, C, D, E, F, G)] =
        Equal.Tuple7Equal
    }

  /**
   * The `EqualF` instance for `Tuple8`.
   */
  implicit def Tuple8EqualF[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal]
    : EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, x) })#lambda] {
      def deriveEqual[H: Equal]: Equal[(A, B, C, D, E, F, G, H)] =
        Equal.Tuple8Equal
    }

  /**
   * The `EqualF` instance for `Tuple9`.
   */
  implicit def Tuple9EqualF[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal, H: Equal]
    : EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, x) })#lambda] {
      def deriveEqual[I: Equal]: Equal[(A, B, C, D, E, F, G, H, I)] =
        Equal.Tuple9Equal
    }

  /**
   * The `EqualF` instance for `Tuple10`.
   */
  implicit def Tuple10EqualF[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal, H: Equal, I: Equal]
    : EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, x) })#lambda] {
      def deriveEqual[J: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J)] =
        Equal.Tuple10Equal
    }

  /**
   * The `EqualF` instance for `Tuple11`.
   */
  implicit def Tuple11EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, x) })#lambda] {
      def deriveEqual[K: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K)] =
        Equal.Tuple11Equal
    }

  /**
   * The `EqualF` instance for `Tuple12`.
   */
  implicit def Tuple12EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, x) })#lambda] {
      def deriveEqual[L: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L)] =
        Equal.Tuple12Equal
    }

  /**
   * The `EqualF` instance for `Tuple13`.
   */
  implicit def Tuple13EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, x) })#lambda] {
      def deriveEqual[M: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
        Equal.Tuple13Equal
    }

  /**
   * The `EqualF` instance for `Tuple14`.
   */
  implicit def Tuple14EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, x) })#lambda] {
      def deriveEqual[N: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
        Equal.Tuple14Equal
    }

  /**
   * The `EqualF` instance for `Tuple15`.
   */
  implicit def Tuple15EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, x) })#lambda] {
      def deriveEqual[O: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
        Equal.Tuple15Equal
    }

  /**
   * The `EqualF` instance for `Tuple16`.
   */
  implicit def Tuple16EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, x) })#lambda] {
      def deriveEqual[P: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
        Equal.Tuple16Equal
    }

  /**
   * The `EqualF` instance for `Tuple17`.
   */
  implicit def Tuple17EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, x) })#lambda] {
      def deriveEqual[Q: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
        Equal.Tuple17Equal
    }

  /**
   * The `EqualF` instance for `Tuple18`.
   */
  implicit def Tuple18EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, x) })#lambda] {
      def deriveEqual[R: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
        Equal.Tuple18Equal
    }

  /**
   * The `EqualF` instance for `Tuple19`.
   */
  implicit def Tuple19EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, x) })#lambda] {
      def deriveEqual[S: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
        Equal.Tuple19Equal
    }

  /**
   * The `EqualF` instance for `Tuple20`.
   */
  implicit def Tuple20EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, x) })#lambda] {
      def deriveEqual[T: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
        Equal.Tuple20Equal
    }

  /**
   * The `EqualF` instance for `Tuple21`.
   */
  implicit def Tuple21EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal,
    T: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, x) })#lambda] {
      def deriveEqual[U: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
        Equal.Tuple21Equal
    }

  /**
   * The `EqualF` instance for `Tuple22`.
   */
  implicit def Tuple22EqualF[
    A: Equal,
    B: Equal,
    C: Equal,
    D: Equal,
    E: Equal,
    F: Equal,
    G: Equal,
    H: Equal,
    I: Equal,
    J: Equal,
    K: Equal,
    L: Equal,
    M: Equal,
    N: Equal,
    O: Equal,
    P: Equal,
    Q: Equal,
    R: Equal,
    S: Equal,
    T: Equal,
    U: Equal
  ]: EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, x) })#lambda] =
    new EqualF[({ type lambda[x] = (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, x) })#lambda] {
      def deriveEqual[V: Equal]: Equal[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
        Equal.Tuple22Equal
    }

  /**
   * The `EqualF` instance for `Vector`.
   */
  implicit val VectorEqualF: EqualF[Vector] =
    new EqualF[Vector] {
      def deriveEqual[A: Equal]: Equal[Vector[A]] =
        Equal.VectorEqual
    }

}
