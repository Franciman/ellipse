def fact = fix lam [fact: Int -> Int, n: Int] {
    cond {
        (= n 0) ~> { 1 }
        else    ~> { * n (fact (- n 1)) }
    }
}

def main = fact 15
