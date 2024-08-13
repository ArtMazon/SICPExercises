#lang sicp

;This is a recursive process, this can be understood as
;the function f(a,b) defined as
;f(a,b)= Inc(f(a-1,b) , if a != 0 and f(0,b) = b
;and this function is recursive in nature
(define (+new a b) 
  (if (= a 0 ) b ( inc(+ (dec a) b))))



; This is an iterative process, it can be expressed as the
; iterative process
; (sum-iter a b ) ( if ( a = 0) b (sum-iter (- a 1) (+ b 1))
;In this case, every step of the process is given by the parameters a and b
; and the next step is decided consdiring the fact whether a is or not,drecreasing the first and increasing the second
; until the first parameter is equal to zero
;Therefore this is an interative process
(define (+alt a b)
  (if (= a 0) b (+ (dec a) (inc b))))



;Ackerman Function
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))


#|This function can defined as f(n) = 2*n |#
(define (f n) (A 0 n))


#|This function can be defined as g(n) = 2^n |#


(define (g n) (A 1 n))

#|This function can be defined as h(n) = 2^(2^2^2^...^2) n-times
 where the total amount of raising this consecutive powers
is n|#
(define (h n) (A 2 n))

#|Exercise 1.11 F-first is a recursive process  |#

(define (F-first x )
  (cond ((or (= x 1) (= x 2) (= x 3)) x)
        (else (+ (F-first (- x 1))
                 (* 2 (F-first (- x 2)))
                 (* 3 (F-first (- x 3)))))))


(define (F-second x)
  (cond ((or (= x 1) ( = x 2 ) (= x 3)) x)
        (else (F-iter 3 2 1 (- x 3)))))

(define (F-iter a b c x )
  (if (= 0 (- x 1)) (+ a (* 2 b) (* 3 c))
      (F-iter (+ a (* 2 b) (* 3 c)) a b (- x 1))))




(F-first 10)
(F-second 10)



#| Exercise 1.12 Pascal Triangle |#


(define (Pascal-triangle-value row position)
  (cond ((or (= position row) (= position 1)) 1)
        (else (+ (Pascal-triangle-value (- row 1) position)
                 (Pascal-triangle-value (- row 1) (- position 1))))))


(Pascal-triangle-value 5 3)

#| Exercise 1.15 |#
(define (cube x ) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define counter 0)
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle 
      (p (sine (/ angle 3.0)))) )

#|a) with each p procedure applied, the value of the angle
is divided by 3, therefore it is requiered at most 3 times
 to make the value 12.15 less than 0.1|#

#|b) Considering both the number of steps and growth in space
 depends on how quickle the value of angle can be
made less than 0.1, thus both magnitudes have order
O(log_3(n)) |#

(sine 12.15)


;Exercise 1.16

(define (fast-exp-iterative x n) (fast-exp-iter 1  x  n ))

(define (square x) (* x x))

(define (iseven? x)
  (if (= (remainder x 2) 0) #t #f))

(define (fast-exp-iter a b n )
  (cond ((= n 1 ) (* a b))
        ((iseven? n) (fast-exp-iter a (square b) (/ n 2)))
        (else (fast-exp-iter (* a b) b (- n 1)))))


;Exercise 1.17 and 1.18 made in one step (by mistake)
#|Even so, 1.17 must look something like this

(define (fast-mult a b)
  (cond (= b 0) 0
        (iseven? b ) (fast-mult  (double a) (halve b))
        (else ( a + (fast-mult a (- b 1)))

|#
(define (double x) (* x 2))
(define (halve x ) (/ x 2))

(define (fast-mult b c ) (fast-mult-iter 0  b  c ))

(define (fast-mult-iter a b c)
  (cond ((= c 1 ) (+ a b))
        ((iseven? c) (fast-mult-iter a (double b) (halve c)))
        (else (fast-mult-iter (+ a b) b (- c 1)))))




(fast-mult 15 7)

;Exercise 1.19

(define (sum-squares x y)
  (+ (square x) (square y)))

(define (fast-fib n)
  (fast-fib-iter 1 0 0 1 n))

(define (fast-fib-iter a b p q count)
  (cond ((= count 0) b)
        ((iseven? count)
         (fast-fib-iter a
                        b
                        (sum-squares p q)
                        (+ (* 2 p q) (square q))
                        (/ count 2)))
        (else (fast-fib-iter (+ (* b q) (* a q ) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1)))))



(fast-fib 9)


(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor ) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? divisor dividend ) (= (remainder dividend divisor) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

;Exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))


;(timed-prime-test 199999)

;Exercise 1.22

(define (search-for-primes start-value consecutive)
  (cond ((= consecutive 0) (display "\nFinished"))
        ((prime? start-value) (timed-prime-test start-value)
                              (search-for-primes (+ start-value 1)
                                                 (- consecutive 1)))
        (else (search-for-primes (+ start-value 1) consecutive))))



;(search-for-primes 1000000 3)

;Exercise 1.23

(define (next n)
  (if (= n 2) 3
      (+ n 2)))

(define (fast-smallest-divisor n) (fast-find-divisor n 2))
(define (fast-find-divisor n test-divisor)
  (cond ((> (square test-divisor ) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (fast-find-divisor n (next test-divisor)))))
                              
(define (fast-prime? n)
  (= (fast-smallest-divisor n) n))

(define (fast-start-prime-test n start-time)
  (if (fast-prime? n)
      (report-prime (- (runtime) start-time))))

(define (fast-timed-prime-test n)
  (newline)
  (display n)
  (fast-start-prime-test n (runtime)))


;Exercise 1.24

(define (fast-search-for-primes start-value consecutive)
  (cond ((= consecutive 0) (display "\nFinished"))
        ((fast-prime? start-value) (fast-timed-prime-test start-value)
                              (fast-search-for-primes (+ start-value 1)
                                                 (- consecutive 1)))
        (else (fast-search-for-primes (+ start-value 1) consecutive))))
 


;(search-for-primes 10000000 3)
;(fast-search-for-primes 10000000 3)

;Exercise 1.27

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base
             (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (lil-fermat a n)
  (= (expmod a n n) a))

(define (fooled-test n) (fooled-test-process 1 n))

(define (fooled-test-process initial-value n)
  (cond ((= initial-value n) (display "Fooled the test"))
        ((lil-fermat initial-value n) (fooled-test-process (+ initial-value 1) n))
        (else (display "Not fooled the test"))))



(fooled-test 6601)


 







