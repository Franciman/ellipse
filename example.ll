def sum = fix lam [sum: Int -> Int, n: Int] {
cond {
    (= n 0) ~> { 0 }
    else ~> { + n (sum (- n 1)) }
}
}

def main = sum 100000
