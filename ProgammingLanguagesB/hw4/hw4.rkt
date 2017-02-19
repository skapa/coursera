
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)(if (<= low high)
                                      (cons low (sequence (+ low stride) high stride))
                                      null))
(define (string-append-map xs suffix) (map string-append
                                           xs
                                           (make-list (length xs) suffix)))


(define (list-nth-mod xs n) (cond
                              [(negative? n) ( error "list-nth-mod: negative number")]
                              [(null? xs) ( error "list-nth-mod: empty list")]
                              [#t (let ([i (remainder n (length xs))])
                                    (car (list-tail xs i)))]))

(define (stream-for-n-steps s n) (if (= 0 n)
                                     null
                                     (let ([p (s)])
                                         (cons (car p) (stream-for-n-steps (cdr p) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda(x) (cons (if (= 0 (modulo x 5))
                                   (* -1 x)
                                   x)
                               (lambda() (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda(d) (cons d
                                (lambda() (f (if (string=? d "dan.jpg")
                                                 "dog.jpg"
                                                 "dan.jpg")))))])
    (lambda() (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda(s) (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
           (lambda() (f s))))














