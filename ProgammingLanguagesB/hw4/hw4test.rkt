#lang racket
;; Programming Languages Homework4 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and change HOMEWORK_FILE to the name of your homework file.
(require "hw4.rkt")

(require rackunit)

;; Helper functions
(define ones (lambda () (cons 1 ones)))
(define a 2)

(define pair-stream
  (letrec ([f (lambda(x) (cons (cons x x)
                               (lambda() (f (+ x 1)))))])
    (lambda () (f 1))))

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence tests
   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")
   (check-equal? (sequence -5 5 3) (list -5 -2 1 4) "Sequence test with negatives")
   (check-equal? (sequence 0 5 6) (list 0) "Sequence test just low returned")
   (check-equal? (sequence 6 5 1) null "Sequence test empty")
   
   ; string-append-map tests
      (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")

      (check-equal? (string-append-map (list "a" "b" "c") "x") '("ax" "bx" "cx") "string-append-map simple test")
      (check-equal? (string-append-map null "x") null "string-append-map empty test")
   
   ; list-nth-mod tests
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 1) 1 "list-nth-mod test 1 ")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test 2 ")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 8) 3 "list-nth-mod test 3 ")
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 0) 0 "list-nth-mod test 4 ")
   
   ; stream-for-n-steps test
   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")
   
   ; funny-number-stream test
   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")
   
   ; dan-then-dog tests
   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test")
   (check-equal? (stream-for-n-steps dan-then-dog 5) (list "dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg") "dan-then-dog test long")
   
   ; stream-add-zero test
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test")
   (check-equal? (stream-for-n-steps (stream-add-zero ones) 3) (list (cons 0 1)(cons 0 1)(cons 0 1)) "stream-add-zero test longer")
   (check-equal? (stream-for-n-steps (stream-add-zero dan-then-dog) 3) (list (cons 0 "dan.jpg")(cons 0 "dog.jpg")(cons 0 "dan.jpg")) "stream-add-zero test dog")
   
   ; cycle-lists test
   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")) 
                 "cycle-lists test")
   
   ; vector-assoc tests
   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")
   (check-equal? (vector-assoc 6 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) #f "vector-assoc test false")
   (check-equal? (vector-assoc 6 (vector "a" (cons 3 1) 2 (cons 5 1) (cons 6 1) (cons 6 7))) (cons 6 1) "vector-assoc test complicated")
   (check-equal? (vector-assoc 6 (vector "a" (list 6 5 4) (cons 3 1) 2 (cons 5 1) (cons 6 1) (cons 6 7))) (list 6 5 4) "vector-assoc test list as pair")
   
   ; cached-assoc tests
   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")
   
   ; while-less test
   (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
