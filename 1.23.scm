(define (square n)
  (* n n))
(define (next n)
  (if (= n 2)
    3
    (+ n 2)))
(define (smallest-divisor n )
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-loop n)
  (define (loopi f n i)
    (if (> i 0)
      (and (f n) (loopi f n (- i 1)))
      (f n)))
  (loopi prime? n 1000))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (cond ((prime-loop n)
         (report-prime (- (current-milliseconds) start-time))
         1)
        (else 0)))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start i)
  (define (loopi f n d i)
    (if (< d i)
      (loopi f (+ n 1) (+ d (f n)) i)))
  (loopi timed-prime-test start 0 i))
