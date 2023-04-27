; (lambda succ
;   (succ succ 0))
; (lambda n (+ n 1))

(define not
  (lambda n
	(if n 0 1)))

(define factorial
  (lambda n
	(if (n) ; if n == 0
	  (* n (factorial (- n 1)))
	  (1))))

; factorial 6
factorial 6
