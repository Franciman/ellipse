def fib = fix lam[fib: Int -> Int, n: Int] {
    cond {
        (= n 0) ~> { 0 }
        (= n 1) ~> { 1 }
        else    ~> { + (fib (- n 1)) (fib (- n 2)) }
    }
}

def main = fib 32
