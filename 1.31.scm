(define (product-a term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (product-b term a next b)
  (if (> a b)
    0
    (* (term a)
       (product-b term (next a) next b))))

(define (factorial n)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (product-b identity 1 inc n)) 

(define (my-pi n)
  (define (get-numerator x)
    (if (even? x)
      (+ x 2)
      (+ x 1)))
  (define (get-denominator x)
    (if (even? x)
      (+ x 1)
      (+ x 2)))
  (define (div x)
    (/ (get-numerator x) (get-denominator x)))
  (define (inc x) (+ x 1))
  (* 4.0 (product-a div 1.0 inc n)))

