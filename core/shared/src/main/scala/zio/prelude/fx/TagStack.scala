package zio.prelude.fx

/**
 * Lightweight port of zio.internal.Stack, optimized for usage with ZPure
 */
private final class TagStack[A <: AnyRef] { self =>
  import TagStack._

  private[this] var array  = new Array[AnyRef](ArrSize + 1)
  private[this] var packed = 0
  private[this] var tags   = 0 // Keep tags as bits in this Int

  array(ArrSize) = new Array[Int](1) // When allocating new array, current tags will be saved here

  def clear(): Unit = {
    var i = 0
    while (i < ArrSize && (array(i) ne null)) {
      array(i) = null
      i += 1
    }
    packed = 0
  }

  /**
   * Pushes an item onto the stack.
   */
  def push(tag: Boolean, a: A): Unit = {
    val packed0 = packed
    val used    = packed0 & 0xf
    val array0  = array
    if (used == ArrSize) {
      val newArr = new Array[AnyRef](ArrSize + 1)
      (array0(ArrSize).asInstanceOf[Array[Int]])(0) = tags
      newArr(0) = array0
      newArr(1) = a
      newArr(ArrSize) = new Array[Int](1)
      tags = if (tag) 2 else 0 // First item will go to array(1), so set the second bit
      array = newArr
      packed += 3
    } else {
      array0(used) = a
      if (tag) {
        tags |= 1 << used
      } else {
        tags &= ~(1 << used)
      }
      packed += 1
    }
  }

  /**
   * Pops an item off the stack, or returns `null` if the stack is empty.
   */
  def pop(): A = {
    val packed0 = packed
    if (packed0 == 0) {
      null.asInstanceOf[A]
    } else {
      val used = packed0 & 0xf
      val idx  = used - 1
      var a    = array(idx)
      if (idx == 0 && packed0 != 1) {
        val arr0 = a.asInstanceOf[Array[AnyRef]]
        a = arr0(ArrSize - 1)
        array = arr0
        tags = arr0(ArrSize).asInstanceOf[Array[Int]](0)
        packed -= 3
      } else {
        packed -= 1
      }
      a.asInstanceOf[A]
    }
  }

  /**
   *  Returns `true` if tag is set for the item at the top. Returns `false` if the stack is empty.
   */
  def tagged: Boolean = {
    val packed0 = packed
    if (packed0 == 0) {
      false
    } else {
      val used = packed0 & 0xf
      val idx  = used - 1
      if (idx == 0 && packed0 != 1) {
        val tags0 = (array(idx).asInstanceOf[Array[AnyRef]])(ArrSize).asInstanceOf[Array[Int]](0)
        (tags0 >> (ArrSize - 1) & 1) == 1
      } else {
        (tags >> idx & 1) == 1
      }
    }
  }
}

private object TagStack {
  private final val ArrSize = 15 // Can be made smaller, but not larger
}
