import week3.Rational

val r = new Rational(1, 2)

val x = if (true) 1 else false

object scratch {
  def error(msg:String) = throw new Error(msg)

  error("hello scala errors")
}

val s: String = null

// Error
// val n: Int = null