(define (accumulate-b combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (accumulate-b combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate-b combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate-a + 0 term a next b))

(define (product term a next b)
  (accumulate-a * 1 term a next b))
