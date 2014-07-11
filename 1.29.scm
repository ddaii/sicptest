(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define (inc x) (+ x 1))
  (define (get-h h) 
    (define (yk k)
      (* (cond ((or (= k 0)
                    (= k n))
                1)
               ((even? k) 2)
               (else 4))
         (f (+ a (* k h)))))
    (* (/ h 3) (sum yk 0 inc n)))
  (get-h (/ (- b a) n)))
