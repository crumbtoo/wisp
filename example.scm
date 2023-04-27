; (lambda succ
;   (succ succ 0))
; (lambda n (+ n 1))

(lambda a ((lambda b (+ a b)) 2)) 1
