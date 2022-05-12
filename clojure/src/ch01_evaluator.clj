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
(atom? :atomic)
;; => true

(defn evaluate [exp env]
  (if (atom? exp)
    ,,,
    ;; we use `first` instead of `car`
    (case (first exp))))

;; An atomic expression can be a symbol (represents a variable whose value is defined by the environment);
;; it can also be actual data like a number or a string
(declare lookup)
(defn evaluate [exp env]
  (if (atom? exp)
    (if (symbol? exp)
      (lookup exp env)
      exp)

    ;; we use `first` instead of `car`
    (case (first exp))))
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
      ((some-fn number? string? char? boolean? vector? keyword?) exp) exp
      :else (wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp))))

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
      ((some-fn number? string? char? boolean? vector? keyword? nil?) exp) exp
      :else (wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp)
      quote (second exp)
      ;; (p.8) we gloss over the fact that in `(if pred)` we use boolean semantics
      ;; of the implementation language (Clojure - which means `nil` will be falsy);
      ;; more precisely, we should write `(if-not (= the-false-value (evaluate (second exp) env)))
      if (if (evaluate (second exp) env)
           (evaluate (nth exp 2) env)
           (evaluate (nth exp 3) env))
      begin (eprogn (rest exp) env)
      set! (update! (second exp) env (evaluate (nth exp 2) env))
      lambda (make-function (second exp) (nnext exp) env)
      ;; it's not a special form, just ordinary function => call it!
      (invoke (evaluate (first exp) env)
              (evlis (rest exp) env)))))

;; 1.4.1 quote - distinguishes program from data

;; 1.4.2 Alternatives (if)
;; in our implementation, we simply the check
;; and use the boolean semantics of the implementation language (Clojure)
;; - this means, we will handle `nil` as falsy
;; a more precise check would be something like
(comment
  (def the-false-value false)
  (if-not (false? (evaluate (second exp) env))
    (evaluate (nth exp 2) env)
    (evaluate (nth exp 3) env))

  .)


;; 1.4.3 Sequence (p.9)
;; - to form a group of forms to evaluate them sequentially
;; here we define the helper `eprogn` function
;;
;; Check also alternative representation of sequences by using only `if` or `lambda` (p.10/11)

(defn eprogn [exps env]
  (if (list? exps)
    (let [[fst & rst] exps
          ;; first expression is always evaluated
          fst-val (evaluate fst env)]
      (if (list? rst)
        (eprogn rst env) ; process the rest of the forms by calling `eprogn` recursively
        ;; the final term in the sequence
        fst-val))
    ;; here we could also return `nil` or other value - see discussion on p.10
    ()))

;; ... we cannot call `eprogn` with for this sequence yet because `lookup` isn't defined
;; (it would try to lookup the `println` symbol)
;; so let's define lookup very simply and temporarily
(defn lookup [exp env]
  (if-let [[k v] (find env exp)]
    v
    ;; resolve symbols to the object it points to
    (some-> exp resolve var-get)))
(lookup 'println {})
;; => #function[clojure.core/println]

;; ... we also need evlis which is only defined in section 1.4.6 (p.12)
(defn evlis [exps env]
  (if (list? exps)
    (map #(evaluate % env) exps)
    ()))
(evlis '(a (println b) "hello") {})
;; => (nil nil "hello")

;; ... and `invoke`
(defn invoke [f args]
  (if (fn? f)
    (apply f args)
    (wrong "Not a function" f args)))
(invoke inc '(10))
;; => 11

;; ... and finally we can try it
(eprogn '((println "I'm first")
          (println "I'm second")
          "Finally a value!")
        {})
;; These will be printed in the REPL:
;;   (I'm first)
;;   (I'm second)
;; => "Finally a value!"


;; what about calling `evaluate` on a sequence? 
#_(evaluate '(do '(1 2 3) '(4 5 6)) {})
;; Not a function
;; ERROR: Unhandled REPL handler exception processing message {:op stacktrace, :nrepl.middleware.print/stream? 1, :nrepl.middleware.print/print cider.nrepl.pprint/pprint, :nrepl.middleware.print/quota 1048576, :nrepl.middleware.print/buffer-size 4096, :nrepl.middleware.print/options {:right-margin 100}, :session 793d408e-4d60-41b4-a7e3-5afdb8601653, :id 994}
;; clojure.lang.Exception Info: Not a function {:expression 1, :rest-args ((2 3))}
;; ...

(evaluate '(inc 1) {});; => 
;; => 2

(evaluate '(begin (inc 1) ) {})
;; => 2

(evaluate '(begin (inc 1) (println "ahoj") (map inc (range 10))) {})
;; => (1 2 3 4 5 6 7 8 9 10)

