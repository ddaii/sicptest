(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime-loop n)
  (define (loopi f n t i)
    (if (> i 0)
      (and (f n t) (loopi f n t (- i 1)))
      (f n t)))
  (loopi fast-prime? n 3 1000))


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
