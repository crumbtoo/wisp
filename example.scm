(define ==
  (lambda a
	(lambda b
	  (if (- a b)
		0
		1))))

(define factorial
  (lambda n
	(if (== n 0)
	  (1)
	  (* n
		 (factorial (- n 1))))))

(trace (factorial 1))

