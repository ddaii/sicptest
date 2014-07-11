(define (square n)
  (* n n))
(define (smallest-divisor n )
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (filtered-accumulate combiner null-value filtered term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) 
            (if (filtered a)
              (combiner result (term a))
              result))))
  (iter a null-value))


(define (prime-sum a b)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (filtered-accumulate + 0 prime? identity a inc b))


(define (gcd-product n)
  (define (identity x) x)
  (define (inc x) (+ x 1))
  (define (coprime? i)
    (= (gcd n i) 1))
  (filtered-accumulate * 1 coprime? identity 1 inc n))

