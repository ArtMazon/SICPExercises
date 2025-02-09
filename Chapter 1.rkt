#lang sicp
;Exercise 1.1
10 ; 10

(+ 5 3 4) ; 12

(- 9 1); 8

(/ 6 3); 2

(+ (* 2 4) (- 4 6)) ; 6

(define a 3) ;nothig

(define b (+ a 1));nothing

(+ a b (* a b));19

(= a b); false

(if (and (> b a) ( < b (* a b)))
    b
    a);4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25));16

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)); 16
;Exercise 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;Exercise 1.3

(define (sqr x) (* x x))
(define (abs x )  (if (< x 0)
                       (- x)
                       x))

(define (largest a b)  (/ (+ a b (abs (- a b)))
                           2))

(sqr 5)
(largest 4 3) 
(define (square-large-three a b c) (+ (sqr(largest a b))
                                      (sqr(largest a c))))

