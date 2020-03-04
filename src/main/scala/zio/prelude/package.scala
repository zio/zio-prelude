package zio

package object prelude extends DebugSyntax with EqualSyntax {
  implicit class BoolSyntax(l: Boolean) {
    def ==>(r: Boolean): Boolean = r || !l
  }
}
