1+3
def abs(x: Double) = if (x < 0) -x else x
abs(-908)


def sqrtIter(guess: Double, x: Double): Double = {
  if (isGoodEnough(guess, x)) return guess
  else sqrtIter(improveGuess(guess, x), x)
}

def isGoodEnough(guess: Double, x: Double): Boolean = {
  abs(guess * guess - x) / x < 0.001
}

def improveGuess(guess: Double, x: Double): Double = {
  (guess + x / guess) / 2
}

def sqrt(x: Double) = sqrtIter(1, x)

sqrt(9)
sqrt(7)
sqrt(64)
sqrt(0.001)
sqrt(1e-6)
sqrt(1e60)





