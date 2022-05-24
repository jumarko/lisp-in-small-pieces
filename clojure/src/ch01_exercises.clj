(ns ch01-exercises
  "Chapter 1 exercises listed on the p.28-29."
  (:require
   [ch01-evaluator :as e]
   [clojure.edn :as edn]
   [clojure.string :as str]))


;;; Exercise 1.1: Modify the function `evaluate` so it becomes a tracer.
;;; - all function calls should display their arguments and results.
;;; You can imagine extending such a rudimentary tracer
;;; to a step-by-step debugger that could modify the running program.

;; Solution:
;; We probably need to redefine make-function to make it work;
;; and similarly to `ch01-evaluator.d-evaluate`,
;; you also need to redefine a bunch of other functions
(declare trace-evaluate)

(defn trace-eprogn
  "Evaluates sequence of expressions in given environment."
  [exps env]
  (if (list? exps)
    (let [[fst & rst] exps
          ;; CHANGE:
          fst-val (trace-evaluate fst env)]
      (if (list? rst)
        ;; CHANGE:
        (trace-eprogn rst env)
        fst-val))
    ()))

(defn trace-make-function [variables body env]
  (fn [values]
    (prn "fn args: " values)
    (let [result (trace-eprogn body (e/extend env variables values))]
      (prn "fn ret: " result)
      result)))

(defn trace-evlis [exps env]
  (if (list? exps)
    ;; CHANGE:
    (map #(trace-evaluate % env) exps)
    ()))

;; use `trace-make-function` inside `trace-evaluate`
(defn trace-evaluate [exp env]
  (if (e/atom? exp)
    (cond
      (symbol? exp) (e/lookup exp env)
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot trace-evaluate - unknown atomic expression?" exp))

    (case (first exp)
      quote (second exp)
      if (if (trace-evaluate (second exp) env)
           (trace-evaluate (nth exp 2) env)
           (trace-evaluate (nth exp 3) env))
      begin (e/eprogn (rest exp) env)
      set! (e/update! (second exp) env (trace-evaluate (nth exp 2) env))
      ;; Special version of  `make-function` implements the tracing
      lambda (trace-make-function (second exp) (nnext exp) env)
      (e/invoke (trace-evaluate (first exp) env)
              (e/evlis (rest exp) env)))))

(assert (= 50
           (trace-evaluate '((lambda (a b) (+ a b))
                             30 20)
                           e/env-global)))
;; It should print this in the REPL:
;;   "fn args: " (30 20)
;;   "fn ret: " 50

(def my-env (assoc e/env-global
                   ;; see ch01-evaluate (env-global definition on the line 308)
                   'list identity))
(assert (= '(1 2)
           (trace-evaluate '(((lambda (a)
                                      (lambda (b) (list a b)))
                              1)
                             2)
                           my-env)))
;; It prints:
;; "fn args: " (1)
;; "fn ret: " #function[ch01-exercises/trace-make-function/fn--14593]
;; "fn args: " (2)
;; "fn ret: " (1 2)

(assert (= '(1 3)
           (trace-evaluate '((lambda (a)
                                     ((lambda (b) (list a b))
                                      (+ 2 a)))
                             1)
                           my-env)))
;; It prints:
;; "fn args: " (1)
;; "fn args: " (3)
;; "fn ret: " (1 3)
;; "fn ret: " (1 3)



(defn trace-make-function
  "A very simple variant of `trace-make-function` which still prints the args and the return value,
  but also stops the execution whenever a function is being called
  and reads the updated environment definition from standard input via `clojure.edn/read-string`.
  This updated environment is then merged into the current execution environment."
  [variables body env]
  (fn [values]
    (prn "fn args: " (zipmap variables values))
    (println "You can redefine the environment however you like - use well-formed Clojure code.")
    (let [extended-env (e/extend env variables values)
          _ (prn "Current env: " extended-env)
          user-input (read-line)
          new-env (when-not (str/blank? user-input) (edn/read-string user-input))
          modified-env (merge extended-env new-env)
          result (trace-eprogn body modified-env)]
      (prn "fn ret: " result)
      result)))

;; Now try to modify the bindings - for example,
;; enter '{a 10}' for the first prompt and just empty input for the second prompt
(comment 
  (trace-evaluate '((lambda (a)
                            ((lambda (b) (list a b))
                             (+ 2 a)))
                    1)
                  e/env-global)
  ;;=> (10 12)
  .)



;;; Ex. 1.2: Eliminate needless recursion in `e/evlis`  when the input list contains only one expression.

;; I used `map` to simplify definition of `e/evlis` before.
;; Let's now reimplement it using recursion
;; note: tail recursion makes it a bit more complicated;
;; as an alternative, see https://github.com/Chouser/lisp-in-small-pieces-clj/blob/main/src/us/chouser/LISP/ch1a.clj#L68-L74
(defn evlis
  ([exps env]
   (evlis [] exps env))
  ([result exps env]
   (println "evlis called with exps:" exps)
   (if-let [s (seq exps)]
     (recur (conj result (e/evaluate (first exps) env))
            (rest s)
            env)
     result)))

;; check that it works:
(assert (= [1 '(2 3) [10 20] '(1 2 3)]
           (evlis '(1
                    (quote (2 3))
                    [10 20]
                    (cons 1 [2 3]))
                  e/env-global)))

;; does it still recur when evaluating only one item?
(evlis '((cons 1 [2 3]))
       e/env-global)
;; evlis called with exps: ((cons 1 [2 3]))
;; evlis called with exps: ()

;; eliminating unnecessary recursion
(defn evlis
  ([exps env] (evlis [] exps env))
  ([result exps env]
   (println "evlis called with exps:" exps)
   (let [s (seq exps)]
     (cond
       (not s) result

       (= 1 (bounded-count 2 s)) (conj result (e/evaluate (first exps) env))

       :else (recur (conj result (e/evaluate (first exps) env))
                    (rest s)
                    env)))))
(assert (= ['(1 2 3)]
           (evlis '((cons 1 [2 3]))
                  e/env-global)))
;; evlis called with exps: ((cons 1 [2 3]))



;;; Ex. 1.5: modify `<` to return a boolean value of the language being defined,
;;; instead of the implementation language (Clojure)
;;; => Abandon this exercise because it doesn't make much sense.
;;; When you look at the answers at the book, they simply define
;;; macro `defpredicate` used to define boolean functions.
;;; => Discussed on slack here https://lisp2022.slack.com/archives/C03C3NMCM7T/p1653104487398989
;;; - this approach imho doesn't bring any value so I'm not implementing it.
;;;   I still preserve my attempt below for the record.

;; so I want to return `t` or `f` but how do I do that?
(e/evaluate '(< 1 2) e/env-global)
;; => true

;; Fix it: TODO: how?
;; ... this only returns the symbol `t`
(e/evaluate '(< 1 2) (assoc e/env-global
                            '< (fn [[a b]] (if (< a b)
                                           't
                                           'f))))
;; => t

;; ... so maybe I need to tweak `invoke`?
(declare t-evaluate)
(defn t-invoke [f args env]
  (if (fn? f)
    ;; CHANGE: call evaluate on the result of the function application
    (t-evaluate (f args) env)
    (e/wrong "Not a function" f args)))

(defn t-evaluate [exp env]
  (if (e/atom? exp)
    (cond
      (= 't exp) true
      (= 'f exp) false
      (symbol? exp) (e/lookup exp env)
      ;; Notice that `keyword?` isn't here because keywords are Clojure's thing
      ;; and aren't present in the Lisp we are trying to implement
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (e/wrong "Cannot evaluate - unknown atomic expression?" exp))

    ;; we use `first` instead of `car`
    (case (first exp)
      quote (second exp)
      ;; (p.8) we gloss over the fact that in `(if pred)` we use boolean semantics
      ;; of the implementation language (Clojure - which means `nil` will be falsy);
      ;; more precisely, we should write `(if-not (= the-false-value (evaluate (second exp) env)))
      if (if (t-evaluate (second exp) env)
           (t-evaluate (nth exp 2) env)
           (t-evaluate (nth exp 3) env))
      begin (e/eprogn (rest exp) env)
      set! (e/update! (second exp) env (t-evaluate (nth exp 2) env))
      lambda (e/make-function (second exp) (nnext exp) env)
      ;; it's not a special form, just ordinary function => call it!
      (t-invoke (t-evaluate (first exp) env)
              (e/evlis (rest exp) env)
              env))))

(t-evaluate '(< 1 2) (assoc e/env-global
                            '< (fn [[a b]] (if (< a b)
                                             't
                                             'f))))
;; => true

;; sanity checks
(t-evaluate '(quote (1 2 3 )) e/env-global)
;; => (1 2 3)
;; TODO:
#_(t-evaluate '(cons 1 [2 3]) e/env-global)
;; Cannot evaluate - unknown atomic expression?
;; {:expression (1 2 3), :args nil}

;; ... would I have to update definitions of `cons` and some other primitives too?




;;; Ex. 1.6 Define function `list`

;; without proper definition, this doesn't work yet
;; (I have already defined `list` above, but only for `my-env`)
#_(e/evaluate '(((lambda (a)
                       (lambda (b) (list a b)))
               1)
              2)
            e/env-global)
;; No such binding
;; {:expression list, :args nil}

;; Define `list` and try again.
;; It looks really simple - just use `list`?
;; - but how do you I define proper arity? (it can be 0 or more arguments)
;; - and should I really return the list implementation offered by Clojure (the hosting language)?

;; here i cheat and say that you can use `list` only for 2 elements:
(e/defprimitive list list 2)
(assert (= '(1 2)
           (e/evaluate '(((lambda (a)
                                  (lambda (b) (list a b)))
                          1)
                         2)
                       e/env-global)))

;; ... but for other number of args you still get the error:
#_(e/evaluate '(list 1 2 3) e/env-global)
;; Incorrect ~arity
;; {:expression [#function[clojure.lang.PersistentList/Primordial] (1 2 3)],
;;  :extra-info ({:expected-arity 2, :actual-arity 3})}

;; ... so do I have to redefine `defprimitive` to support variadic functions?
;; one idea is to support `arity` arg that is a map with the following keys:
;; - `:min-arity`
;; - `:max-arity`
;; both of them would be optional and checked only if arity is a map
;; if it's an empty map, then it would simply accept any number of arguments

;; name `lift` inspired by Paul Bauer: https://github.com/pmbauer/LiSP-2022/blob/trunk/src/codes/bauer/LiSP/ch01.clj#L103
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
        (e/wrong "Incorrect arity" [f values] {:expected-arity arity :actual-arity val-count})))))

(defmacro defprimitive
  "Defines a primitive operation denoted by the symbol with given name,
  implemented as function f of given arity."
  [name f arity]
  `(e/definitial ~name (lift ~f ~arity)))

(defprimitive list list {:min-arity 0})


(assert (= '(1 2 3)
           (e/evaluate '(list 1 2 3) e/env-global)))
(type (e/evaluate '(list 1 2 3) e/env-global))
;; => clojure.lang.PersistentList

;; Note: alternatively, I could skip `defprimitive` and use `definitial` to define `list`.
;; This is how they did it in the book's answer.
(comment
  ;; Notice they don't use `list`'s implementation in Scheme either - they simply use anonymous
  ;; function to return the input args as they are. This is better approach!
  ;; notice we don't need to use `(fn [& values])` as with `defprimitive` below
  (e/definitial list (fn [values] values)))

;; so use an anonymous function instead of `list` directly
;; notice we have to use `(fn [& values])` because `defprimitive` uses `apply`
(defprimitive list (fn [& values] values) {:min-arity 0})
(assert (= '(1 2 3)
           (e/evaluate '(list 1 2 3) e/env-global)))
;; notice how type is now clojure.lang.Cons instead of clojure.lang.PersistentList
(type (e/evaluate '(list 1 2 3) e/env-global))
;; => clojure.lang.Cons

;; here it returns `nil` instead of `() which might be unexpected but maybe it's OK
;; since our language is Scheme?
;; or should we fix it?
(assert (= nil
           (e/evaluate '(list) e/env-global)))
(assert (= '(1)
           (e/evaluate '(list 1) e/env-global)))
(assert (= '(1 2 3 4 5 6 7 8 9 10)
           (e/evaluate '(list 1 2 3 4 5 6 7 8 9 10) e/env-global)))

;; I decided to fix the empty list too - see p.9 in the book where they say
;; that false, nil, and () are all different values

(defprimitive list (fn [& values] (or values ())) {:min-arity 0})

(assert (= ()
           (e/evaluate '(list) e/env-global)))
(assert (= '(1 2 3)
           (e/evaluate '(list 1 2 3) e/env-global)))



;;; Ex. 1.8 Define `apply`

;; the simplest implementation should just use Clojure's apply
(defprimitive apply (fn [f args] (apply f args)) 2)

;; ... but why it throws arity error??
#_(assert (= '(1 2 3)
           (e/evaluate '(apply list [1 2 3]) e/env-global)))
;; Wrong number of args (3) passed to: ch01-exercises/eval12973/fn--12974/fn--12975

;; the problem is that our `list` function really doesn't accept variable number of args
;; but instead takes only a collection...
((e/lookup 'list e/env-global)
 [1 2 3])
;; => (1 2 3)


;; ... that is because of how `defprimitive` already uses `apply`
;; to mitigate that, we need to either use `definitial` or wrap args again
;; here we wrap args with a vector so that defprimitive doesn't expand the inner arglist
(defprimitive apply (fn [f args] (apply f [args])) 2)
(assert (= '(1 2 3) (e/evaluate '(apply list [1 2 3]) e/env-global)))
(assert (= () (e/evaluate '(apply list []) e/env-global)))

;; finally, let's try with a lambda
(e/evaluate '(apply (lambda (a b c) (list a b c)) [1 2 3]) e/env-global)
;; => (1 2 3)


;; Now, what if Clojure didn't have `apply` function?
;; How we would implement apply ourselves?
;; ...
;; I think we would need to build a form with function at the first argument
;; and spread the arglist.
;; So to manually convert `(f [x y z ...])` into `(f x y z ...)`
;; Then evaluate the form - but notice I'm using Clojure's eval because `e/invoke`
;; also simply calls the target function via `invoke`
;; (it's not working through `e/evaluate` anymore because that would be a cycle)
(defprimitive apply (fn [f args] (e/invoke f args )) 2)

(assert (= '(1 2 3) (e/evaluate '(apply list [1 2 3]) e/env-global)))
(assert (= () (e/evaluate '(apply list []) e/env-global)))

;; Finally, in the answer to Ex. 1.8 they mention that apply is variadic!
;; - this doesn't work with my former definition
#_(assert (= '(1 2 3) (e/evaluate '(apply list 1 2 [3 4 5]) e/env-global)))
;; {:expression
;;  [#function[ch01-exercises/eval14010/fn--14011/fn--14012/fn--14015]
;;   (#function[ch01-exercises/eval13793/fn--13794/fn--13795] 1 2 [3 4 5])],
;;  :extra-info ({:expected-arity 2, :actual-arity 4})}

;; so try again - change the acceptable arity
(defprimitive apply (fn [f args] (e/invoke f args )) {:min-arity 2})
;; it doesn't work because our primitive still accepts only two args
#_(assert (= '(1 2 3) (e/evaluate '(apply list 1 2 [3 4 5]) e/env-global)))
;; 1. Unhandled clojure.lang.ArityException
;; Wrong number of args (4) passed to: ch01-exercises/eval14033/fn--14034/fn--14035/fn--14036

;; We change our primitive to accept variable number of args and use `apply concat` to flatten args.
(defprimitive apply (fn [f & args] (e/invoke f (apply concat args) )) {:min-arity 2})
(assert (= '(1 2 3) (e/evaluate '(apply list [1 2 3]) e/env-global)))
(assert (= () (e/evaluate '(apply list []) e/env-global)))


;;; Ex. 1.9 Define function `end` so you can exit cleanly from the interpreter (REPL) loop
;;; My idea is to let user return a special symbol `repl.exit`, detect it in the body
;;; of the `repl` function and exit cleanly.

(e/evaluate '(quote repl.exit) e/env-global)
;; => repl.exit

(defprimitive end (fn [] 'repl.exit) 0)

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
     (recur (e/repl1)))))

(comment
  (repl)
  .)



;;; Ex. 1.10 Performance benchmarking
;;; 1. Compare the speed of Scheme (Clojure) and `evaluate`
;;; 2. Then compare the speed of `evaluate` and `evaluate` interpreted by `evaluate`.


;; what kind of functions to benchmark?
;; perhaps factorial?
;; but recursion is only presented in section 2.6 ...
;; So we just evaluate a simple function over and over again.


;;; 1. Compare the speed of Scheme (Clojure) and `evaluate`
;; clojure:
(time (dotimes [i 100000]
        ((fn [x] (+ x x))
         i)))
"Elapsed time: 6.947866 msecs"

;; `evaluate`:
(time (dotimes [i 100000]
        (e/evaluate '((lambda (x) (+ x x))
                      1000)
                    e/env-global)))
"Elapsed time: 418.444609 msecs"
;; => evaluate is much slower than native Clojure, up to 100x
;; - over time it can be somewhare faster, but it's always been 300+ msecs



;;; 2. Then compare the speed of `evaluate` and `evaluate` interpreted by `evaluate`.
;;; How can I run `evaluate` interpreted by `evaluate`?
;;; Do I need to extend `evaluate` definition to recognize itself?
;;; or do I just need to use `defprimitive`?

(defprimitive evaluate (fn [exp env]
                         (e/evaluate exp env)) 2)

;; now benchmark evaluate inside evaluate
(time (dotimes [i 100000]
        (e/evaluate '(evaluate ((lambda (x) (+ x x))
                                1000)
                               ;; empty env is good enough here
                               '())
                    e/env-global)))
"Elapsed time: 422.944397 msecs"
;; => wrapping evaluate within another evaluate adds little overhead
;; it'a almost the same thing.

