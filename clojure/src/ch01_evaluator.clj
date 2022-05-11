(ns ch01-evaluator
  "Chapter 1: Basic evaluator.
  Starts with section 1.2 on p. 4.")

;;; 1.2 (p.4): define `evaluate` signature
(defn evaluate [exp env])


;;; 1.3 (p.4): start building the implementation
;;; - introduces `atom?` check and `case` for non-atomic forms
;;; - discusses the difference between 'program' and 'representation';
;;;   evaluate works on the _representation_ 

;; they define atom as anything that's not a "pair" (cons cell)
;; - we use `list?` for Clojure which should be good enough
(def atom? (complement list?))
(atom? 1)
;; => true
(atom? "ahoj")
;; => true
(atom? \c)
;; => true
(atom? '(1 2 3))
;; => false
;; TODO: notice that other composite data structures like vectors are considered atoms!
(atom? [1 2 3])
;; => true
(atom? {1 2 3 4})
;; => true


(defn evaluate [exp env]
  (if (atom? exp)
    ,,,
    ;; we use `first` instead of `car`
    (case (first exp)
      ,,,)))


;; An atomic expression can be a symbol (represents a variable whose value is defined by the environment);
;; it can also be actual data like a number or a string
(declare lookup)
(defn evaluate [exp env]
  (if (atom? exp)
    (if (symbol? exp)
      (lookup exp env)
      exp)

    ;; we use `first` instead of `car`
    (case (first exp)
      ,,,)))
;; don't try to call it yet because `lookup` isn't defined
#_(evaluate 'ahoj {})


;; autoquoted objects like numbers and strings don't have to be quoted
;; and are represented by their values
;; Note: we define our own `wrong` function here
(defn wrong [msg exp & args]
  (throw (ex-info msg {:expression exp :rest-args args})))

(defn evaluate [exp env]
  (if (atom? exp)
    (cond
      (symbol? exp) (lookup exp env)
      ;; TODO: why is vector? here??
      ;; TODO: what about keywords? nil?
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp)
      ,,,)))

(evaluate 1 {})
;; => 1
(evaluate "ahoj" {})
;; => "ahoj"
(evaluate [1 2 3] {})
;; => [1 2 3]




;;; 1.4 Evaluating forms (p. 6 - 12)
;;; Discusses "special forms" like quote, if, set!, lambda, begin
;;; Scheme only has 4 special forms (the above without `begin`?)

;; we introduce a `case` check to identify special forms
(declare eprogn update! make-function invoke evlis)
(defn evaluate [exp env]
  (if (atom? exp)
    (cond
      (symbol? exp) (lookup exp env)
      ;; TODO: why is vector? here??
      ;; TODO: what about keywords? nil?
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp)
      quote (second exp)
      if (if (evaluate (second exp) env)
           (evaluate (nth exp 2) env)
           (evaluate (nth exp 3) env))
      begin (eprogn (rest exp) env)
      set! (update! (second exp) env (evaluate (nth exp 2) env))
      lambda (make-function (second exp) (nnext exp) env)
      :else (invoke (evaluate (first exp) env)
                    (evlis (rest exp) env)))))

;; 1.4.1 quote - distinguishes program from data
