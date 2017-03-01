;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rl)
  (if (null? rl)
      (aunit)
      (apair (car rl) (racketlist->mupllist (cdr rl)))))

(define (mupllist->racketlist ml)
  (if (aunit? ml)
      null
      (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(fun? e)
         (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL comparison applied to non-number")))]
        [(mlet? e)
         (let ([var (mlet-var e)]
               [v1 (eval-under-env (mlet-e e) env)])
           (if (string? var)
               (eval-under-env (mlet-body e) (cons (cons var v1) env))
               (error "MUPL variable name has to be string")))]
        [(call? e)
         (let ([clsr (eval-under-env (call-funexp e) env)]
               [param (eval-under-env (call-actual e) env)])
           (if (closure? clsr)
               (let ([f (closure-fun clsr)]
                     [fenv (closure-env clsr)])
                 (eval-under-env (fun-body f)
                                 (cons (cons (fun-formal f) param)
                                       (if (fun-nameopt f)
                                           (cons (cons (fun-nameopt f) clsr) fenv)
                                           fenv))))
               (error "MUPL call funexp has to be closure")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env)
                (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([p (eval-under-env (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL fst evaluated on not pair")))]
        [(snd? e)
         (let ([p (eval-under-env (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL snd evaluated on not pair")))]
        [(isaunit? e) 
         (if (aunit? (eval-under-env (isaunit-e e) env)) (int 1) (int 0))]
        [(aunit? e) e]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([s (car (car lstlst))]
            [e (cdr (car lstlst))])
        (mlet s e (mlet* (cdr lstlst) e2)))))
        
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

;; Problem 4

(define mupl-map
  (fun #f "f" (fun "r" "l"
                   (ifeq (isaunit (var "l")) (int 1)
                       (var "l")
                       (apair (call (var "f") (fst (var "l"))) (call (var "r") (snd (var "l"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "a" (call (var "map") (fun #f "x" (add (var "x") (var "a"))) ))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (letrec ([h (lambda (e)
                (cond [(var? e) (set (var-string e))]
                      [(mlet? e) (set-remove (set-union (h (mlet-e e)) (h (mlet-body e))) (mlet-var e))]
                      [(call? e)(set-union (h (call-funexp e)) (h (call-actual e)))]
                      [(add? e)(set-union (h (add-e1 e)) (h (add-e2 e)))]
                      [(ifgreater? e) (set-union (h (ifgreater-e1 e)) (h (ifgreater-e2 e)) (h (ifgreater-e3 e)) (h (ifgreater-e4 e)))]
                      [(apair? e)(set-union (h (apair-e1 e)) (h (apair-e2 e)))]
                      [(fst? e)(h (fst-e e))]
                      [(snd? e)(h (snd-e e))]
                      [(isaunit? e)(h (isaunit-e e))]
                      [(fun? e)(set-remove (set-remove (h (fun-body e)) (fun-formal e)) (fun-nameopt e))]
                      ;[(closure? e) (set-subtract (h (closure-fun e)) (list->set (map (lambda (x) (car x))(closure-env e))))]
                      [(closure? e) (h (closure-fun e))]
                      [#t (set)]))])

    (cond [(mlet? e) (mlet (mlet-var e) (compute-free-vars (mlet-e e)) (compute-free-vars (mlet-body e)))]
          [(call? e) (call (compute-free-vars (call-funexp e)) (compute-free-vars (call-actual e)))]
          [(add? e) (add (compute-free-vars (add-e1 e)) (compute-free-vars  (add-e2 e)))]
          [(ifgreater? e) (ifgreater (compute-free-vars  (ifgreater-e1 e)) (compute-free-vars  (ifgreater-e2 e)) (compute-free-vars (ifgreater-e3 e)) (compute-free-vars (ifgreater-e4 e)))]
          [(apair? e) (apair (compute-free-vars (apair-e1 e)) (compute-free-vars (apair-e2 e)))]
          [(fst? e)(fst (compute-free-vars (fst-e e)))]
          [(snd? e)(snd (compute-free-vars (snd-e e)))]
          [(isaunit? e) (isaunit (compute-free-vars (isaunit-e e)))]
          [(fun? e)(fun-challenge (fun-nameopt e) (fun-formal e) (compute-free-vars (fun-body e)) (h e))]
          ;[(fun? e)(fun-challenge (fun-nameopt e) (fun-formal e) (fun-body e) (h e))]
          [(closure? e) (closure (closure-env e) (compute-free-vars (closure-fun e)))]
          [#t e])))


;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env-c (add-e1 e) env)]
               [v2 (eval-under-env-c (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(fun-challenge? e)
         (closure (filter (lambda (v) (set-member? (fun-challenge-freevars e) (car v))) env) e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env-c (ifgreater-e1 e) env)]
               [v2 (eval-under-env-c (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env-c (ifgreater-e3 e) env)
                   (eval-under-env-c (ifgreater-e4 e) env))
               (error "MUPL comparison applied to non-number")))]
        [(mlet? e)
         (let ([var (mlet-var e)]
               [v1 (eval-under-env-c (mlet-e e) env)])
           (if (string? var)
               (eval-under-env-c (mlet-body e) (cons (cons var v1) env))
               (error "MUPL variable name has to be string")))]
        [(call? e)
         (let ([clsr (eval-under-env-c (call-funexp e) env)]
               [param (eval-under-env-c (call-actual e) env)])
           (if (closure? clsr)
               (let ([f (closure-fun clsr)]
                     [fenv (closure-env clsr)])
                 (eval-under-env-c (fun-challenge-body f)
                                 (cons (cons (fun-challenge-formal f) param)
                                       (if (fun-challenge-nameopt f)
                                           (cons (cons (fun-challenge-nameopt f) clsr) fenv)
                                           fenv))))
               (error "MUPL call funexp has to be closure")))]
        [(apair? e)
         (apair (eval-under-env-c (apair-e1 e) env)
                (eval-under-env-c (apair-e2 e) env))]
        [(fst? e)
         (let ([p (eval-under-env-c (fst-e e) env)])
           (if (apair? p)
               (apair-e1 p)
               (error "MUPL fst evaluated on not pair")))]
        [(snd? e)
         (let ([p (eval-under-env-c (snd-e e) env)])
           (if (apair? p)
               (apair-e2 p)
               (error "MUPL snd evaluated on not pair")))]
        [(isaunit? e) 
         (if (aunit? (eval-under-env-c (isaunit-e e) env)) (int 1) (int 0))]
        [(aunit? e) e]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
