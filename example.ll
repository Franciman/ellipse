def sum = fix lam[sum: Int -> Int, n: Int] {
  cond {
    (< n 1) ~> { 0 }
    else    ~> { + n (sum (- n 1)) }
  }
}

def main = sum 10000000
