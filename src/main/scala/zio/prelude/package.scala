package zio

package object prelude extends DebugSyntax with EqualSyntax with OrdSyntax with HashSyntax with ClosureSyntax with NewtypeExports {
  implicit class BoolSyntax(l: Boolean) {
    def ==>(r: Boolean): Boolean = r || !l

    def <==> (r: Boolean): Boolean = l == r
  }
}
