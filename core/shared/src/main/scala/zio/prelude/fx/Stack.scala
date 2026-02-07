package zio.prelude.fx

/**
 * Lightweight port of zio.internal.Stack, optimized for usage with ZPure
 */
private final class Stack { self =>
  import Stack._

  private[this] var array  = new Array[AnyRef](ArrSize)
  private[this] var packed = 0

  def clear(): Unit = {
    var i     = 0
    val array = this.array
    while (i < ArrSize && (array(i) ne null)) {
      array(i) = null
      i += 1
    }
    packed = 0
  }

  /**
   * Pushes an item onto the stack.
   */
  def push(a: AnyRef): Unit = {
    val packed0 = packed
    val used    = packed0 & 0xf
    if (used == ArrSize) {
      val newArr = new Array[AnyRef](ArrSize)
      newArr(0) = array
      newArr(1) = a
      array = newArr
      packed = packed0 + 3
    } else {
      array(used) = a
      packed = packed0 + 1
    }
  }

  def push2(a: AnyRef, b: AnyRef): Unit = {
    val packed0 = packed
    val used    = packed0 & 0xf
    if (used >= ArrSize - 1) {
      push(a)
      push(b)
    } else {
      val array = this.array
      array(used) = a
      array(used + 1) = b
      packed = packed0 + 2
    }
  }

  /**
   * Pops an item off the stack, or returns `null` if the stack is empty.
   */
  def pop(): AnyRef = {
    val packed0 = packed
    if (packed0 == 0) {
      null
    } else {
      val used = packed0 & 0xf
      val idx  = used - 1
      var a    = array(idx)
      if (idx == 0 && packed0 != 1) {
        val arr0 = a.asInstanceOf[Array[AnyRef]]
        a = arr0(ArrSize - 1)
        array = arr0
        packed = packed0 - 3
      } else {
        packed = packed0 - 1
      }
      a
    }
  }
}

private object Stack {
  private final val ArrSize = 15 // Can be made smaller, but not larger
}
