(define factorial
  (lambda n
    (if n
      (* n
        (factorial (- n 1)))
      (1))))

(trace (factorial 6))

