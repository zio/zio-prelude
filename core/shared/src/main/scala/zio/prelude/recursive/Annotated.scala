package zio.prelude.recursive

/**
 * An `Annotated[Case, A]` is a recursive data structure where each case `Case`
 * has been annotated with some additional information `A`.
 */
final case class Annotated[Case[+_], A](caseValue: Case[Annotated[Case, A]], annotations: A)
