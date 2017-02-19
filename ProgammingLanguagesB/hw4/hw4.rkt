
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (<= low high)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map string-append
       xs
       (make-list (length xs) suffix)))


(define (list-nth-mod xs n)
  (cond [(negative? n) ( error "list-nth-mod: negative number")]
        [(null? xs) ( error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (if (= 0 n)
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
  (letrec ([f (lambda(d)
                (cons d (lambda()
                          (f (if (string=? d "dan.jpg")
                                 "dog.jpg"
                                 "dan.jpg")))))])
    (lambda() (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda(s)
                (cons (cons 0 (car (s))) (lambda ()
                                           (f (cdr (s))))))])
    (lambda() (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda()
                                                                       (f (+ n 1)))))])
  (lambda() (f 0))))

(define (vector-assoc v vec)
  (letrec ([vl (vector-length vec)]
           [f (lambda (n)
                (if (= n vl)
                    #f
                    (let ([e (vector-ref vec n)])
                           (if (and (pair? e) (equal? (car e) v))
                               e
                               (f (+ n 1))))))])
    (f 0)))

 (define (cached-assoc xs n)
   (letrec ([pos 0]
            [cache (make-vector n '(cons #f #f))]
            [f (lambda (v)
                 (let ([r (assoc v (vector->list cache))])
                   (if r
                       r
                       (let ([fr (assoc v xs)])
                         (if fr
                             (begin (vector-set! cache pos fr)
                              (set! pos (modulo (+ 1 pos) n))
                              fr)
                             #f)))))])
   (lambda (v) (f v))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([test e1])
       (letrec ([while (lambda(body-res)
                         (if (< body-res test)
                             (while e2)
                             #t))])
         (while e2)))]))








