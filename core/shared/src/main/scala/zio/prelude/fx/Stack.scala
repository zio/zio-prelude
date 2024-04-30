package zio.prelude.fx

/**
 * Lightweight port of zio.internal.Stack, optimized for usage with ZPure
 */
private final class Stack[A <: AnyRef] { self =>
  private[this] final val N = 14

  private[this] var array  = new Array[AnyRef](N + 1)
  private[this] var packed = 0

  /**
   * Pushes an item onto the stack.
   */
  def push(a: A): Unit = {
    val packed0 = packed
    val used    = packed0 & 0xf
    if (used == N + 1) {
      val newArr = new Array[AnyRef](N + 1)
      newArr(0) = array
      newArr(1) = a
      array = newArr
      packed = packed0 + 3
    } else {
      array(used) = a
      packed = packed0 + 1
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
        a = arr0(N)
        array = arr0
        packed = packed0 - 3
      } else {
        packed = packed0 - 1
      }
      a.asInstanceOf[A]
    }
  }

  /**
   * Optimized variant of [[pop]] for cases that we don't want to do anything with the item
   */
  def popDiscard(): Unit = {
    val packed0 = packed
    if (packed0 != 0) {
      val used = packed0 & 0xf
      val idx  = used - 1
      if (idx == 0 && packed0 != 1) {
        val arr0 = array(idx).asInstanceOf[Array[AnyRef]]
        array = arr0
        packed = packed0 - 3
      } else {
        packed = packed0 - 1
      }
    }
  }

  /**
   * Peeks the item on the head of the stack, or returns `null` if empty.
   */
  def peek(): A = {
    val packed0 = packed
    if (packed0 == 0) {
      null.asInstanceOf[A]
    } else {
      val idx = (packed0 & 0xf) - 1
      val out =
        if (idx == 0 && packed0 != 1) {
          array(0).asInstanceOf[Array[AnyRef]](N)
        } else {
          array(idx)
        }
      out.asInstanceOf[A]
    }
  }
}

private object Stack {
  def apply[A <: AnyRef](): Stack[A] =
    new Stack[A]

  def apply[A <: AnyRef](a: A): Stack[A] = {
    val stack = new Stack[A]
    stack.push(a)
    stack
  }
}
