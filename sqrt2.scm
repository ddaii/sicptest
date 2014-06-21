(define (my_abs mun)
  (cond ((< mun 0) (- mun))
        (else mun)))
(define (get_guess guess x)
   (/ (+ (/ x guess) guess) 2.0))
(define (test_guess guess pre_guess)
  (< (my_abs (/ (- pre_guess guess) pre_guess)) 0.001))
(define (mysqrt-iter guess x pre_guess)
  (if (test_guess guess pre_guess)
    guess
    (mysqrt-iter (get_guess guess x) x guess)))

(define (mysqrt x) 
  (mysqrt-iter 1.0 x 2.0)) 
