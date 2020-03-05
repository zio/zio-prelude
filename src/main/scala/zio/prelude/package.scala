package zio

package object prelude extends DebugSyntax with EqualSyntax with OrdSyntax {
  implicit class BoolSyntax(l: Boolean) {
    def ==>(r: Boolean): Boolean = r || !l
  }
}
