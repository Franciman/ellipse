def fib = fix lam[fib: Int -> Int, n: Int] {
  cond {
  (< n 2) ~> { n }
  else ~> { + (fib (- n 1)) (fib (- n 2)) }
  }
}

def main = fib 40
