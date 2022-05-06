;;;; Here are some examples from the preface - "Short Summary of Scheme" on p. xviii

;; try some vectors!
#(do re mi)

;;; conditionals

;; cond - very similar to Clojure's cond
(define x 'flap)
(cond
 ((eq? x 'flip) 'flop)
 ((eq? x 'flop) 'flip)
 (else (list x "neither flip nor flop")))

;; rewriting the example above using `case`
(case x
  ((flip) 'flop)
  ((flop) 'flip)
  (else (list x "neither flip nor flop")))


;;; functions & define
;;; read the manual, especially for let, let*, and letrec
;;; - with let*, bindings are defined sequentially - just as Clojure's let
;;; - letrec is similar to Clojure's letfn

;; functions are defined with lambda:
(define rev
  (lambda (l)
    ;; we use letrec here to be able to call reverse from within itself
    (letrec ((reverse (lambda (l r)
                        (if (pair? l)
                            (reverse (cdr l) (cons (car l) r))
                            r))))
      (reverse l '()))))

(rev '(1 2 3))

;; ... we can rewrite the above with some syntax sugar
(define (rev l)
  (define nil '())
  (define (reverse l r)
    (if (pair? l)
        (reverse (cdr l) (cons (car l) r))
        r))
  (reverse l nil))

(rev '(1 2 3))
