(ns ch01-evaluator-final
  "Chapter 1: Basic evaluator - final version.
  This is similar to `ch01-evaluator` but skips intermediate steps and experiments.
  As such it provides a clearer picture of the current state of the code,
  but it lacks some explanations.")


;;; 1.3 (p.4): start building the implementation
;;; - introduces `atom?` check and `case` for non-atomic forms
;;; - discusses the difference between 'program' and 'representation';
;;;   evaluate works on the _representation_ 

;; they define atom as anything that's not a "pair" (cons cell)
;; - we use `list?` for Clojure which should be good enough
(def atom? (complement list?))
(assert (true? (atom? 1)))
(assert (true? (atom? "ahoj")))
(assert (true? (atom? \c)))
;; TODO: notice that vectors are considered atoms!
(assert (true? (atom? [1 2 3])))
(assert (true? (atom? {1 2 10 20})))
;; while keywords are not really supported by our language's evaluate function we consider them atomic
(assert (true? (atom? :atomic)))
(assert (false? (atom? '(1 2 3))))


;; We define our own `wrong` function here
(defn wrong [msg exp & extra-info]
  (throw (ex-info msg {:expression exp :extra-info extra-info})))

(declare evaluate)

(defn eprogn
  "Evaluates sequence of expressions in given environment."
  [exps env]
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

(defn lookup [exp env]
  (if-let [[_k v] (find env exp)]
    v
    ;; resolve symbols to the object it points to
    (some-> exp resolve var-get)))

;; ... we also need evlis which is only defined in section 1.4.6 (p.12)
(defn evlis [exps env]
  (if (list? exps)
    (map #(evaluate % env) exps)
    ()))

;; ... and `invoke`
(defn invoke [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f args)))

(defn update! [id env value]
  (if (map? env)
    (assoc env id value)
    (wrong "Can't understand environment" env {:id id
                                               :env env
                                               :value value})))

(def env-init {})

;; `extend` can enrich environment by assigning variables to their values
;; - be careful when referring to this from somewhere else
;;   because there's also `clojure.core/extend` which is shadowed by `extend`
(defn extend [env variables values]
  ;; we do not yet support special variables like `& args` capturing all the remaining values
  (if (= (count variables) (count values))
    (into env (zipmap variables values))
    (wrong "The number of variables does not match the number of values"
           {:var-count (count variables) :val-count (count values)}
           {:env env :variables variables :values values})))
(comment
  (def my-env (extend env-init '[name title age] ["Juraj" "Programmer" 36]))
  ;; => {name "Juraj", title "Programmer", age 36}

  (extend my-env '[hobbies website] [["climbing" "reading"] "https://curiousprogrammer.net"])
  ;; => {name "Juraj", title "Programmer", age 36, hobbies ["climbing" "reading"], website "https://curiousprogrammer.net"}
  .)

;; The idea is simple: use the `env` passed as an argument to `make-function`
;; and pass it further to `eprogn` executed when the defined lambda is called.
(defn make-function [variables body env]
  (fn [values]
    ;; use `env`, not `env-global` here:
    (eprogn body (extend env variables values))))

(def env-global env-init)


(defmacro definitial
  "Defines a new symbol in the global environment bound to given value
  or 'ch01-evaluator/void if no value is provided."
  ([name]
   ;; (prn name) ; the value of name
   ;; (prn (type name)) ; clojure.lang.Symbol
   ;; since definitial is a macro, simply ~name will do the job
   `(definitial ~name :ch01-evaluator/void))
  ([name value]
   ;; notice how we use `extend` instead of relying on internal env structure
   `(alter-var-root #'env-global #(extend % ['~name] [~value]))))


;; p28 - we now define  a few useful constants
(defn define-constants []
  (definitial t true)
  (definitial f false)
  (definitial nil nil))
(define-constants)

(defn evaluate [exp env]
  (if (atom? exp)
    (cond
      ;; lock immutability of t and f in the interpreter
      (= 't exp) true
      (= 'f exp) false
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

;; now define some random variables that Lispers typically use :) (p.27)
(defn setup-vars []
  (definitial foo)
  (definitial bar)
  (definitial fib)
  (definitial fact))
(setup-vars)
;; => {t true,
;;     f false,
;;     nil nil,
;;     foo :ch01-evaluator/void,
;;     bar :ch01-evaluator/void,
;;     fib :ch01-evaluator/void,
;;     fact :ch01-evaluator/void}env-global


;;; From Ex 1.6 and 1.8: enhanced `defprimitive`, `list`, `apply`
(defn lift
  "Lifts native (host language) function `f` into a function
  that can be used in our interpreter.
  The resulting function still needs to be registered, presumably via `definitial`.
  In most cases, you can use `defprimitive` which uses `lift` internally."
  [f arity]
  (fn [values]
    (let [val-count (count values)]
      (if (or (and (nat-int? arity) (= arity val-count))
              (and (map? arity)
                   (<= (:min-arity arity -1) val-count)
                   (>= (:max-arity arity Long/MAX_VALUE) val-count)))
        (apply f values)
        (wrong "Incorrect ~arity" [f values] {:expected-arity arity :actual-arity val-count})))))

(defmacro defprimitive
  "Defines a primitive operation denoted by the symbol with given name,
  implemented as function f of given arity."
  [name f arity]
  `(definitial ~name (lift ~f ~arity)))


#_(defmacro defprimitive
  "Defines a primitive operation denoted by the symbol with given name,
  implemented as function f of given arity."
  [name f arity]
  `(definitial
     ~name
     (fn [~'values]
       (let [val-count# (count ~'values)]
         (if (or (and (nat-int? ~arity) (= ~arity val-count#))
                 (and (map? ~arity)
                      (<= (:min-arity ~arity -1) val-count#)
                      (>= (:max-arity ~arity Long/MAX_VALUE) val-count#)))
           (apply ~f ~'values)
           (wrong "Incorrect ~arity" [~f ~'values] {:expected-arity ~arity :actual-arity val-count#}))))))


;; Finally, define a few useful library functions
(defn stdlib []
  (defprimitive cons cons 2)
  (defprimitive car first 1)
  ;; set-cdr! function is more complicated because it actually mutates the list
  ;; consider this example from mit-scheme implementation:
  ;;
  ;;     (define x '(1 2 3))
  ;;
  ;;     x
  ;;     ;Value: (1 2 3)

  ;;     (set-cdr! x '(20 30))
  ;;     ;Unspecified return value
  ;;
  ;;     x
  ;;     ;Value: (1 20 30)
  ;; For now, we simply ignore the mutability aspect and return a modified copy
  (defprimitive set-cdr! (fn [xs new-rst] (cons (first xs) new-rst)) 2)
  (defprimitive + + 2)
  (defprimitive eq? = 2)
  (defprimitive < < 2)
  (defprimitive list (fn [& values] (or values ())) {:min-arity 0})
  (defprimitive apply (fn [f & args] (invoke f (apply concat args))) {:min-arity 2})
  ;; From ex. 1.9 - a better definition of `repl` with support for `end` function.
  (defprimitive end (fn [] 'repl.exit) 0))
(stdlib)
;; => {nil nil,
;;     t true,
;;     end #function[ch01-evaluator-final/stdlib/fn--12427/fn--12428],
;;     < #function[ch01-evaluator-final/stdlib/fn--12390/fn--12391],
;;     cons #function[ch01-evaluator-final/stdlib/fn--12341/fn--12342],
;;     fib :ch01-evaluator/void,
;;     set-cdr! #function[ch01-evaluator-final/stdlib/fn--12359/fn--12360],
;;     car #function[ch01-evaluator-final/stdlib/fn--12350/fn--12351],
;;     bar :ch01-evaluator/void,
;;     fact :ch01-evaluator/void,
;;     list #function[ch01-evaluator-final/stdlib/fn--12399/fn--12400],
;;     + #function[ch01-evaluator-final/stdlib/fn--12372/fn--12373],
;;     eq? #function[ch01-evaluator-final/stdlib/fn--12381/fn--12382],
;;     foo :ch01-evaluator/void,
;;     apply #function[ch01-evaluator-final/stdlib/fn--12414/fn--12415],
;;     f false}

(assert (= '(1 2 3) (evaluate '(cons 1 [2 3]) env-global)))
(assert (= 1 (evaluate '(car [1 2 3]) env-global)))
(assert (= '(1 20 30) (evaluate '(set-cdr! [1 2 3] [20 30]) env-global)))
(assert (= 110 (evaluate '(+ 10 100) env-global)))
(assert (false? (evaluate '(eq? '(1 2) [1 2 3]) env-global)))
(assert (true? (evaluate '(eq? '(1 2 3) [1 2 3]) env-global)))
(assert (true? (evaluate '(< 2 3) env-global)))
;; true is true regardless of what local binding we use (not saying that can't be confusing either)
(assert (= 1 (evaluate '((lambda (t) (if t 1 0))
                         false)
                       env-global)))



;;; 1.8 (p.27): The Intepreter, finally!
;;; we build a basic Read-Eval-Print-Loop using `evaluate`

(defn repl1
  "Reads a single line from stdin, `evaluate`s it and prints the result to stdout.
  Returns the evaluated expression."
  []
  (-> (read) (evaluate env-global) (doto (prn))))

;; this is called `toplevel` in the book
(defn repl
  "`repl1` in a loop with support for a clean exit via `(end)`.
  To exit enter "
  ([]
   (println "Welcome to the REPL!")
   (println "You can evaluate forms one by one - they are read from stdin.")
   (println "When you are done, type (end)")
   (repl nil))
  ([last-ret]
   (if (= last-ret 'repl.exit)
     (println "Bye!")
     (recur (repl1)))))

;; quick demo
(with-in-str
  "(+ 10 20) (list 1 2 3) (end)"
  (repl))
;; will print:
;;   Welcome to the REPL!
;;   You can evaluate forms one by one - they are read from stdin.
;;   When you are done, type (end)
;;   30
;;   (1 2 3)
;;   repl.exit
;;   Bye!

(comment
  ;; run the repl once and enter some expressions, e
  ;; .g. (cons 1 [2 3]), (quote (1 2 3)), etc.
  ;; ... then check the repl buffer for the printed result
  (repl1)

  ;; beaware: infinite loop - but you can break it by entering invalid expression
  (repl)

  .)


