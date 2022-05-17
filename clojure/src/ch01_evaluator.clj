(ns ch01-evaluator
  "Chapter 1: Basic evaluator.
  Starts with section 1.2 on p. 4.

  Naming note: they sometimes use symbols like `env.init` and `d.invoke`.
  We can't do that in Clojure because '.' has a special meaning in symbols.
  Instead, we simply replace it with dash, as with `env-init`.")

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
  (throw (ex-info msg {:expression exp :args args})))

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


;;; 1.5 Environment (p.12-14)
;;; We define an empty environment and function `extend` to bind variables to values

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
;; - be careful when referring to this from somewhere else
;;   because there's also `clojure.core/extend` which is shadowed by `extend`
(defn extend [env variables values]
  ;; we do not yet support special variables like `& args` capturing all the remaining values
  (if (= (count variables) (count values))
    (into env (zipmap variables values))
    (wrong "The number of variables does not match the number of values"
           {:var-count (count variables) :val-count (count values)}
           {:env env :variables variables :values values})))


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



;;; 1.6: Representing Functions (p.15-25)
;;; This is where things start to get really interesting.
;;; We'll try a few approaches for handling environments,
;;; identify a problem with each approach and fix it.
;;; We'll also look at dynamic and lexical binding.

;; We already defined `invoke` but here it is again
;; Note that our definition forces all the arguments being evaluated
;; before validation (is it a fn?) and application
;; This is due to how Clojure (and many other languages) evaluate arguments - eagerly, from left to right
(defn invoke [f args]
  (if (fn? f)
    (apply f args)
    (wrong "Not a function" f args)))

;; Previously, we made `lookup` too smart by using `resolve` and `var-get`.
;; We should start with an empty environment and not allow the usual Clojure functions like `println` to be used.
;; Let's start again with the minimalistic implementation of lookup (p. 13):
(defn lookup [id env]
  (if-let [[k v] (find env id)]
    v
    (wrong "No such binding" id)))
#_(lookup 'println {})
;; 1. Unhandled clojure.lang.ExceptionInfo
;; No such binding
;; {:expression println, :rest-args nil}
(lookup 'a '{a 10 b 20})
;; => 10


;; p.16: evaluating a function comes down to evaluating its body
;; in an environment where its variables are bound to the values

;; We will now define `make-function`:
;; here's the relevant portion of `evaluate` calling `make-function`
;;       lambda (make-function (second exp) (nnext exp) env)
;; We will try multiple approaches and fix the problems that arise as we go

;; i) Minimal environment
;; First, try with 'minimal environment' (i.e. `env-init` - empty env)
(defn make-function [variables body env]
  (fn [values] ; does `values` really do what we want?? (spoiler: no!)
    (eprogn body (extend env-init variables values))))
;; try it!
#_((evaluate '(lambda (a b) a)
           {})
 1 2)
;; 1. Unhandled clojure.lang.ArityException
;; Wrong number of args (2) passed to: ch01-evaluator/make-function/fn--13185

;; Let's try again with `& values`
;; notice that `env` is unused which leads to the problem described below.
(defn make-function [variables body env]
  (fn [& values]
    (eprogn body (extend env-init variables values))))
((evaluate '(lambda (a b) a)
           {})
 1 2)
;; => 1

;; Now the problem with the minimal environment is that we don't have access to the global env,
;; not even to functions like `cons` and `car` (will add those fns to the global env in section 1.7)
#_(evaluate '(car 1 '())
          {})
;; No such binding
;; {:expression car, :rest-args nil}



;; ii) Patched environment:
;; We will update `make-function` to use the global environment instead of `env-init`
;; `env` param is still unused and that will cause another problem shortly.
(def env-global env-init)
(defn make-function [variables body env]
  (fn [& values]
    (eprogn body (extend env-global variables values))))

;; For demo, let's add the `car` function to our global environment:
(def env-global (assoc env-global 'car first))
;; This works  but it's not related to `make-function`'s implementation above
;; because we aren't creating a lambda.
(evaluate '(car '(1 2 3)) env-global)
;; => 1

;; This is more relevant - we create a lambda that uses `car`.
;; That lambda accepts a list as an input arg
(evaluate '((lambda (a) (car a))
            '(30 20 10))
          env-global)
;; => 30

;; This is all great but we still have a  problem: nested lambdas don't work
;; The environment of the outer function isn't available to the inner function
(def env-global (assoc env-global
                       '+ +
                       'list list))
#_(evaluate '((lambda (a)
                    ;; `a` isn't present in the environment visible to this inner lambda
                    ((lambda (b) (list a b))
                     (+ 2 a)))
            1)
          env-global)
;; 1. Unhandled clojure.lang.ExceptionInfo
;; No such binding
;; {:expression a, :rest-args nil}



;; iii) Improving the patch: so we can see the outer variables in the inner functions
;; Our, quick and naive, solution is to just pass the current environment
;; to `invoke` which then passes it to the functions being invoked.
;; In both cases, the env is passed as the last argument.
;; We'll redefine `evaluate`, `make-function`, and `invoke`.
;; To avoid confusion with previous definitions we well use new names with `d-` prefix
;; (this is also used because the solution won't be the final one that we'll use)

;; `d-invoke`, unlike `invoke` accepts an environment as 3rd argument
(defn d-invoke [f args env]
  (if (fn? f)
    ;; In Scheme, they simply had `(f args env)` - I'm not sure how that worked,
    ;; but in clojure we need to use `apply` to apply the function to `args` and add `env` as the last element of `args`.
    (apply f (conj (vec args) env))
    (wrong "Not a function" f args)))

;; Notice how we have two environments here:
;; - `def-env` is the environment at the time the function is defined/created - it's not used here at all
;; - `current-env` is the environment at the time the function is called - this is the one we are using
(defn d-make-function [variables body def-env]
  ;; here we need to extract the env arg as the last value in `values`
  ;; I'm wondering how does Scheme makes it work out of the box as in `(lambda (values current.env))`
  (fn [& values]
    (let [current-env (last values)]
      ;; here we pass only `current-env` received as an argument of our function;
      ;; we don't use `def-env` at all
      ;; note: here we must drop the last argument from `values` which is `current-env` itself
      (eprogn body (extend current-env variables (butlast values))))))

;; Inside `evaluate`, we only change the way `invoke` is called - adding `env` as an extra parameter
(defn d-evaluate [exp env]
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
      ;; CHANGE: call `d-make-function` instead of `make-function`
      lambda (d-make-function (second exp) (nnext exp) env)
      ;; it's not a special form, just ordinary function => call it!
      ;; CHANGE: call `d-invoke` instead of `invoke` and also `d-evaluate`
      (d-invoke (d-evaluate (first exp) env)
              (evlis (rest exp) env)
              ;; CHANGE: env is passed in as a new argument
              env))))

;; now try a simple example
(d-evaluate '((lambda (a) (car a))
            '(30 20 10))
          env-global)
;; ... the first time I tried this I got an error
   ;; The number of variables does not match the number of values
   ;; {:expression {:var-count 1, :val-count 2},
   ;;  :args
   ;;  ({:env
   ;;    {car #function[clojure.core/first--5449],
   ;;     + #function[clojure.core/+],
   ;;     list #function[clojure.lang.PersistentList/Primordial]},
   ;;    :variables (a),
   ;;    :values
   ;;    ((30 20 10)
   ;;     {car #function[clojure.core/first--5449],
   ;;      + #function[clojure.core/+],
   ;;      list #function[clojure.lang.PersistentList/Primordial]})})}
   ;;      ch01_evaluator.clj:   61  ch01-evaluator/wrong
   ;;      ch01_evaluator.clj:   60  ch01-evaluator/wrong
   ;;             RestFn.java:  442  clojure.lang.RestFn/invoke
   ;;      ch01_evaluator.clj:  249  ch01-evaluator/extend
   ;;      ch01_evaluator.clj:  245  ch01-evaluator/extend
   ;;      ch01_evaluator.clj:  406  ch01-evaluator/d-make-function/fn
   ;;             RestFn.java:  137  clojure.lang.RestFn/applyTo
   ;;                core.clj:  667  clojure.core/apply
   ;;                core.clj:  662  clojure.core/apply
   ;;      ch01_evaluator.clj:  392  ch01-evaluator/d-invoke
   ;;      ch01_evaluator.clj:  388  ch01-evaluator/d-invoke
   ;;      ch01_evaluator.clj:  433  ch01-evaluator/d-evaluate
   ;;      ch01_evaluator.clj:  409  ch01-evaluator/d-evaluate

;; ... the fix was to use `butlast` to remove `current-env` from the list of all args
;; when calling the function created via `d-make-function`.
(d-evaluate '((lambda (a) (car a))
              '(30 20 10))
            env-global)
;; => 30
