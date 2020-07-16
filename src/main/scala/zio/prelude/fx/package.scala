package zio.prelude

import zio.{ Has, UIO }

package object fx {
  type RefState[S] = Has[RefState.Service[S]]
  object RefState {
    trait Service[S] {

      def modify[A](f: S => (S, A)): UIO[A]
    }
  }
}
