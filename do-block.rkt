#lang racket

; Simulates (very roughly) a Haskell do-block by transforming a
; value-or-#f (poor man's Maybe) through multiple functions of the type
; >>= expects - taking a pure value and returning a value-or-#f.
; The problem: each function only sees the value passed directly
; to it, i.e. the one defined in the line above in the do-block.
(define (>>= x . fs)
  (foldl (lambda (f x) (and x (f x))) x fs)) ; the lambda is the actual >>=

; (>>= x f1 f2 f3)
; <=> ((x >>= f1) >>= f2) >>= f3)
;   where (x >>= f) <=> (and x (f x))

(define (test->>= x)
  (>>= x
       (lambda (x) (+ x 5)) ; this never fails, it's actually a pure function :)
       (lambda (x) (and (> x 10) "iei")) ; this "fails" for x<=10
       string-length
       (lambda (x) (* x 2))))

; Ideally, to simulate a real do-block, we'd like
; to have something like this for example:
;(define (do-stuff-with x)
;  (do-block (y <- (f x))
;            (z <- (g x y)) ; use more than one identifier
;            (w <- (h z))
;            (foo w z))) ; the value to be returned (on success)
; So, we transform the above to:
;(let [(y (f x))]
;  (and y (do-block (z <- (g x y))
;                   (w <- (h z))
;                   (foo w z))))
(define-syntax do-block
  (syntax-rules (<-)
    [(do-block res) res]
    [(do-block (id <- expr) exprs ...) (let [(id expr)] (and id (do-block exprs ...)))]))

(define (test-do x)
  (do-block (y <- (and (even? x) (- x 5))) ; this "fails" for odd x
            (z <- (+ x y)) ; again a pure function
            (+ x y z)))
              
(and (equal? (test->>= 10) 6)
     (equal? (test->>= 5) #f)
     (equal? (test-do 10) 30)
     (equal? (test-do 5) #f))
