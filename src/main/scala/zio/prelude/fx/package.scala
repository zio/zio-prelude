package zio.prelude

import zio.{ Ref, ZIO }

package object fx {
  type StatefulZIO[S, -R, +E, +A] = ZIO[(R, Ref[S]), E, A]
}
