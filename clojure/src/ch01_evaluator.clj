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
;; while keywords are not really supported by our language's evaluate function we consider them atomic
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
      ((some-fn number? string? char? boolean? vector?) exp) exp
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
      ;; Notice that `keyword?` isn't here because keywords are Clojure's thing
      ;; and aren't present in the Lisp we are trying to implement
      ((some-fn number? string? char? boolean? vector?) exp) exp
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


;; 1.5 Environment (p.12-)

;; this is how `set!` is implemented in `evaluate`:
;;   set! (update! (second exp) env (evaluate (nth exp 2) env))

;; what set! does in scheme:
;; (define x 2)
;; ;Value: x
;; x
;; ;Value: 2

;; (set! x 1)
;; ;Value: 2
;; x
;; ;Value: 1

;; alter-var-root in clojure?
;; (def x 2)
;; (alter-var-root #'x inc)
;; (println x)

(defn update! [id env value]
  (if (map? env)
    (assoc env id value)
    (wrong "Can't understand environment" env {:id id
                                               :env env
                                               :value value})))

(evaluate '(set! x 1) {})
;; => {x 1}


;; An empty environment.
;; Note: in the book they use the name `env.init` but that has a special meaning in Clojure.
;; It designates a fully-qualified class name such as java.lang.String.
;; See https://clojure.org/reference/reader#_symbols
(def env-init {})


;; `extend` can enrich environment by assigning variables to their values
(defn extend [env variables values]
  ;; we do not yet support special variables like `& args` capturing all the remaining values
  (if (= (count variables) (count values))
    (into env (zipmap variables values))
    (wrong "The number of variables does not match the number of values"
           [(count variables) (count values)]
           {:variables variables :values values})))


(def my-env (extend env-init '[name title age] ["Juraj" "Programmer" 36]))
;; => {name "Juraj", title "Programmer", age 36}

(extend my-env '[hobbies website] [["climbing" "reading"] "https://curiousprogrammer.net"])
;; => {name "Juraj",
;;     title "Programmer",
;;     age 36,
;;     hobbies ["climbing" "reading"],
;;     website "https://curiousprogrammer.net"}

;; This shouldn't work:
#_(extend my-env '[hair eyes] ["brown"])
;; The number of variables does not match the number of values
;; {:expression [2 1], :rest-args ({:variables [hair eyes], :values ["brown"]})}
