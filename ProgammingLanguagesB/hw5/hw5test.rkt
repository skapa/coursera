#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
(require "hw5.rkt")

(require rackunit)

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   (check-equal? (racketlist->mupllist (list (var "a") (var "foo"))) (apair (var "a") (apair (var "foo") (aunit))) "racketlist->mupllist test strings")
   (check-equal? (racketlist->mupllist (list (int 3) (var "foo") (int 33))) (apair (int 3) (apair (var "foo") (apair (int 33) (aunit)))) "racketlist->mupllist test long mixed")
   (check-equal? (racketlist->mupllist (list (int "foo") (var 4))) (apair (int "foo") (apair (var 4) (aunit))) "racketlist->mupllist test not checked")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")
   (check-equal? (mupllist->racketlist (apair (var "a") (apair (var "b") (apair (var "c") (aunit))))) (list (var "a") (var "b") (var "c")) "racketlist->mupllist test")

   ;; add test
   (check-equal? (eval-exp (add (int 3) (int 2))) (int 5) "add test")
   (check-equal? (eval-exp (add (int 3) (add (int 3) (int 2)))) (int 8) "add test deep")
   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   (check-equal? (eval-exp (ifaunit (aunit) (int 2) (int 3))) (int 2) "ifaunit test positive")
   (check-equal? (eval-exp (ifaunit (var "a") (int 3) (add (int 5) (int 4)))) (int 9) "ifaunit test with add")

   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   (check-equal? (eval-under-env (ifgreater (int 3) (int 1) (var "a") (int 3)) (list (cons "a" (int 10)))) (int 10) "ifgreater test with var")
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   (check-equal? (eval-exp (mlet "x" (mlet "y" (int 4) (add (int 1) (var "y"))) (add (int 5) (var "x")))) (int 10) "mlet test nested")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (closure '() (fun "until10" "x" (ifgreater (var "x") (int 20) (var "x") (call (var "until10") (add (int 1) (var "x")))))) (int 0))) (int 21) "call test recursive")
   
   ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   (check-equal? (eval-exp (fst (apair (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1)) (int 2)))) (int 8) "fst test with call inside")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* (list (cons "a" (int 1)) (cons "b" (int 2))) (add (var "a") (var "b")))) (int 3) "mlet* test bigger")
   
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (ifgreater (var "x") (int 3) (add (var "x") (int 7)) (add (var "x") (int 4))))) (apair (int 2) (apair (int 7) (apair (int 3) (aunit))))))
                 (apair (int 6) (apair (int 14) (apair (int 7) (aunit)))) "mupl-map test complex")

   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")

   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
