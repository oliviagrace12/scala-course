import scala.annotation.tailrec

@tailrec
def iterfactorial(location: Double, running: Double): Double = {
  if (location == 0 || location == 1) return running
  iterfactorial(location - 1, running * location)
}

def factorial(x: Double): Double = {
  iterfactorial(x, 1)
}

factorial(3)
factorial(7)