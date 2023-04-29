(define !=
  (lambda a
	(lambda b
	  (- a b))))

(define ==
  (lambda a
	(lambda b
	  (if (!= a b)
	    (0)
		(1)))))

(define factorial
  (lambda n
	(if (== n 0)
	  1
	  (* n (factorial (- n 1))))))

(factorial 1)
(== 0 20)
(== 3 20)
(== 20 3)
(== 20 20)
(== 0 0)
