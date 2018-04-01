package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      b() * b() - 4 * a() * c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      if (delta() < 0) Set[Double]()
      else if (delta() == 0) {
        val ans = - b() / (2 * a())
        Set[Double](ans)
      } else {
        val a1 = - b() + Math.sqrt(delta()) / (2 * a())
        val a2 = - b() - Math.sqrt(delta()) / (2 * a())
        Set[Double](a1, a2)
      }
    }
  }
}
