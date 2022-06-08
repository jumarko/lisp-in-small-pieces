(ns ch01-jack)

;; Anything that's not a list is an atom
(def atom? (complement list?))

(atom? '(1 2 3))
;; => false
(atom? 1)
;; => true
(atom? '[1 2 3])
;; => true
(atom? "jack")
;; => true

;; Utility function to handle errors more helpfully
(defn wrong [msg exp & args]
  (throw (ex-info msg {:expression exp :rest-args args})))

;; AOT declarations needed for `evaluate` below
(declare lookup update! make-function invoke evlis eprogn)

;; Basic evaluate (eval) function - takes an expression `e` and environment `env`
;; This is really as much as we can do without digging into `eprogn` (see below)
;; and more complex environments (required by `make-function`).
(defn evaluate [e env]
  (if (atom? e)
    (cond
      ;; Symbols are simply looked up in the environment
      (symbol? e) (lookup e env)
      ;; Various other atomic values are _autoquoted_
      ;; ((some-fn number? string? char? boolean? vector?) exp) exp
      ((some-fn number? boolean? string? char? vector?) e) e
      :else (wrong "Unable to evaluate atom" e))
    ;; More complex forms: quote, if, begin (i.e. `do`...), set! and lambdas (functions)
    ;; We now know that `e` is not atomic, so we look at its first element to decide how to proceed.
    (case (first e)
      quote (second e)
      if (if (evaluate (second e) env)
           (evaluate (nth e 2) env)
           (evaluate (nth e 3) env))
      ;; `begin` is a country cousin of Clojure's `do`
      begin (eprogn (last e) env)
      ;; `set!` updates (mutates) the value of a variable in the environment
      ;; i.e. it associates the second element of the expression with the
      ;;      value obtained by evaluating the following element...
      set! (update! (second e) env (evaluate (nth e 2) env))
      ;; lambda means "make a function from this expression"
      ;; the second element of the expression is taken to be the name/symbol (?)
      ;; for the function we want to create/call (?), and we pass the rest of
      ;; the expression `e` as the function's args.
      lambda (make-function (second e) (nnext e) env)
      (invoke (evaluate (first e) env)
              (evlis (rest e) env)))))

(evaluate true {})
;; => true
(evaluate 1 {})
;; => 1
#_(evaluate 'sym {})
;; this doesn't work - need to define `lookup`
(evaluate '(quote (1 2 3)) {})
;; => (1 2 3)
(evaluate [1 2 3] {})
;; => [1 2 3]
(evaluate \c {})
;; => \c
(evaluate '(if true "true" "false") {})
;; => "true"
(evaluate '(if false "true" "false") {})
;; => "false"

;; 1.4.3 sequences + `eprogn`
(defn eprogn
  "Evaluates a sequence of expressions `exps` in an environment `env`."
  [exps env]
  (if (list? exps)
    (let [[fst & rst] exps
          ;; We always want to evaluate the fst expression
          ;; so that we know how to proceed with the rst
          fst-val (evaluate fst env)]
      (if (list? rst)
        (eprogn rst env)
        fst-val))
    ()))

(assert (= (eprogn '('a 'b 'c) {}) 'c))
(assert (= (eprogn '("foo" "bar" "baz") {}) "baz"))
;; Should return the value of the second if statement
(assert (= (eprogn '((if true "true" "false") (if false "true" "false")) {}) "false"))

;; Having defined `eprogn`, we can now evaluate statements with `begin`
(evaluate '(begin ((if true "true" "false") (if false "true" "false"))) {})
;; => "false"

;; 1.4.6 `evlis`
;; `evlis` is effectively just a mapping of `evaluate` over a list of `exps`.
(defn evlis
  "Takes a list of expressions `exps` and returns a list of their values."
  [exps env]
  (if (list? exps)
    (letfn [(evaluate' [exp] (evaluate exp env))]
      (map evaluate' exps))
    ()))

(evlis '((if false "true" "false")) {})
;; => ("false")
(evlis '((if false "true" "false") false) {})
;; => ("false" false)
(evlis '((if false "true" "fasle") "hey" false 21) {})
;; => ("fasle" "hey" false 21)
(evlis "foobar" {})
;; => ()
(evlis '(1 2 3) {})
;; => (1 2 3)


;; 1.4.6. `invoke`
;; `invoke` is repsonsible for applying functions to arguments - so I think
;; we can just use `apply` here?
(defn invoke [f args]
  (if (fn? f)
    (apply f args)
    (wrong "Not a function" f args)))

(invoke + '(1 2 3))
;; => 6
(invoke inc '(10))
;; => 11
;;
;; => using `apply` works out fine!

;; I think we're now ready to try the full `evaluate` function.
#_(evaluate '(+ (1 2 3)) {})
;; Oh, wait, we need to define `lookup` first - otherwise we don't know
;; what values (vars?) correspond to symbols.

;; ... `lookup` is treated in section 1.5 "The Environment".
;; This function does what it says on the tin: lookups the var associated
;; with a symbol in the environment.
(defn lookup [id env]
  (if-let [[k v] (find env id)]
    v
    (wrong "No such id in env" id env)))

#_(lookup "foo" {})
;; This doesn't and shouldn't work.
(lookup "foo" {"foo" +})
;; => #function[clojure.core/+]
(lookup 'evaluate {'evaluate evaluate})  ;; meta OooOOOooo
;; => #function[ch01-jack/evaluate]
(lookup 'jack {'jack {:name "jack" :age 28}})
;; => {:name "jack", :age 28}
(lookup 'john {'john nil})  ;; bit of an edge case (works with `find` but not with `get`)
;; => nil

;; Not 100% clear on the distinction between symbols and variables?
;; Some info in 1.3 on this - "When the expression is a symbol, the expression
;; represents a variable and its value is the one attributed by the environment."
;; => symbols -> variables -> values (by association in/with an env)
;; There's a lot more info about this on p.5
;; In particular, note that `evaluate` states an implicit conversion between
;; symbols and variables. A more exacting statement of the terms might be
;; something like ... (lookup (symbol->variable exp) env) ... where the
;; symbol->variable emphasises that the symbol must be converted into a variable
;; _before_ being looked up in the environment.
;; A _syntactic_ entity (the symbol) is converted into a _semantic_ entity (the variable)
;; (Corollary: variables don't exist, and can only be "got at" via their syntactic
;; counterparts).

(defn update! [id env value]
  (if (map? env)
    (assoc env id value)
    (wrong "Can't parse environment" env {:id id
                                          :env env
                                          :value value})))

(evaluate '(set! x 1) {})
;; => {x 1}
(evaluate (nth '(set! x 1) 2) {})
;; => 1
(nth '(set! x 1) 2)
;; => 1

;; 1.6 - functions
;; "Applying a function comes down to evaluating its body in an environment where
;; its variables are bound to values that they have assumed during the application."

;; This chapter starts with a minimal i.e. empty environment
(def env-init {})

;; We need a definition of `extend` - some way of associating variables with values
;; in the environment (this is actually from section 1.5)

(defn extend [env variables values]
  (if (= (count variables) (count values))
    (into env (zipmap variables values))
    (wrong "Number of variables must match number of values"
           [(count variables) (count values)]
           {:variables variables} {:values values})))

(extend env-init '(:a :b) '(+ -))
;; => {:a +, :b -}
(extend {:a + :b -} '(:c) '("foo"))
;; => {:a #function[clojure.core/+], :b #function[clojure.core/-], :c "foo"}
;; => {:c "foo"}  ;; This is wrong (initial implementation of `extend` without `into`)

;; Basically, initial attempts at an environment fail because `invoke` needs
;; access to the environment too: it's no good extending the global environment
;; if `invoke` can't see it, and thus cannot evaluate functions!

(defn make-function [variables body env]
  (fn [& values]
    (eprogn body (extend env variables values))))

((evaluate '(lambda (a b) a) {}) 1 2)

(evaluate '((lambda (a)
                    ;; `a` isn't present in the environment visible to this inner lambda
                    ((lambda (b) (list a b))
                     (+ 2 a)))
            1)
          ;; have to provide relevant host functions in env
          {'+ +
           'list list})
;; => (1 3)

;; Exercise 1.1 - modify evaluate to add tracing
(defn evaluate [e env]
  (if (atom? e)
    (cond
      ;; Symbols are simply looked up in the environment
      (symbol? e) (lookup e env)
      ;; Various other atomic values are _autoquoted_
      ;; ((some-fn number? string? char? boolean? vector?) exp) exp
      ((some-fn number? boolean? string? char? vector?) e) e
      :else (wrong "Unable to evaluate atom" e))
    ;; More complex forms: quote, if, begin (i.e. `do`...), set! and lambdas (functions)
    ;; We now know that `e` is not atomic, so we look at its first element to decide how to proceed.
    (case (first e)
      quote (second e)
      if (if (evaluate (second e) env)
           (evaluate (nth e 2) env)
           (evaluate (nth e 3) env))
      ;; `begin` is a country cousin of Clojure's `do`
      begin (eprogn (last e) env)
      ;; `set!` updates (mutates) the value of a variable in the environment
      ;; i.e. it associates the second element of the expression with the
      ;;      value obtained by evaluating the following element...
      set! (update! (second e) env (evaluate (nth e 2) env))
      ;; lambda means "make a function from this expression"
      ;; the second element of the expression is taken to be the name/symbol (?)
      ;; for the function we want to create/call (?), and we pass the rest of
      ;; the expression `e` as the function's args.
      lambda (make-function (second e) (nnext e) env)
      (let [fun (evaluate (first e) env)
            args (evlis (rest e) env)
            res (invoke fun args)]
        (println "Evaluate function " (first e) " With args " args " Result is " res)
        res))))

(evaluate 1 {})
;; => 1

(evaluate '(+ 1 2) {'+ +})
;; => nil
;; => nil
;; => 3

(list [1 2 3])
;; => ([1 2 3])

(list 1 2 3)
;; => (1 2 3)

(defn mi-list [& args]
  (apply list args))

(mi-list 1 2 3)
;; => (1 2 3)

(defn end []
  'repl-exit)

(def env-global {'+ +
                 'end end})

(defn repl1 []
  (let [res (evaluate (read) env-global)]
    res))

(defn repl []
  (let [res (repl1)]
    (if (= res 'repl-exit)
      (println "Bye!")
      (recur))))

(repl)

;; === CHAPTER 2 ===

(declare evaluate-application f-make-function f-evlis f-eprogn)

;; f-evaluate
(defn f-evaluate [e env fenv]
  (if (atom? e)
    (cond
      ;; Symbols are simply looked up in the environment
      (symbol? e) (lookup e env)
      ;; Various other atomic values are _autoquoted_
      ;; ((some-fn number? string? char? boolean? vector?) exp) exp
      ((some-fn number? boolean? string? char? vector?) e) e
      :else (wrong "Unable to evaluate atom" e))
    ;; More complex forms: quote, if, begin (i.e. `do`...), set! and lambdas (functions)
    ;; We now know that `e` is not atomic, so we look at its first element to decide how to proceed.
    (case (first e)
      quote (second e)
      if (if (f-evaluate (second e) env fenv)
           (f-evaluate (nth e 2) env fenv)
           (f-evaluate (nth e 3) env fenv))
      ;; `begin` is a country cousin of Clojure's `do`
      begin (f-eprogn (last e) env fenv)
      ;; `set!` updates (mutates) the value of a variable in the environment
      ;; i.e. it associates the second element of the expression with the
      ;;      value obtained by evaluating the following element...
      set! (update! (second e) env (f-evaluate (nth e 2) env fenv))
      ;; lambda means "make a function from this expression"
      ;; the second element of the expression is taken to be the name/symbol (?)
      ;; for the function we want to create/call (?), and we pass the rest of
      ;; the expression `e` as the function's args.
      lambda (f-make-function (second e) (nnext e) env fenv)
      ;; CHANGE: instead of calling `invoke` directly, call a new form `evaluate-application`.
      (evaluate-application (first e)
                            (f-evlis (rest e) env fenv)
                            env
                            fenv))))

;; evaluate non-functional expressions should still work as expected
(assert (= (f-evaluate 1 {} {}) 2))
;; => nil  :ta-da:

;; now define f-* variants of evaluator helpers (evlis, eprogn etc.)
(defn f-eprogn
  "Evaluates a sequence of expressions `exps` in an environment `env`."
  [exps env fenv]
  (if (list? exps)
    (let [[fst & rst] exps
          ;; We always want to evaluate the fst expression
          ;; so that we know how to proceed with the rst
          fst-val (f-evaluate fst env fenv)]
      (if (list? rst)
        (f-eprogn rst env fenv)
        fst-val))
    ()))

;; should work as before
(assert (= (f-eprogn '('a 'b 'c) {} {}) 'c))
;; => nil
(assert (= (f-eprogn '("foo" "bar" "baz") {} {}) "baz"))
;; => nil
;; Should return the value of the second if statement
(assert (= (f-eprogn '((if true "true" "false") (if false "true" "false")) {} {}) "false"))
;; => nil

(defn f-evlis
  "Takes a list of expressions `exps` and returns a list of their values."
  [exps env fenv]
  (if (list? exps)
    (letfn [(f-evaluate' [exp] (f-evaluate exp env fenv))]
      (map f-evaluate' exps))
    ()))

;; should work as before
(f-evlis '((if false "true" "false")) {} {})
;; => ("false")
(f-evlis '((if false "true" "false") false) {} {})
;; => ("false" false)
(f-evlis '((if false "true" "fasle") "hey" false 21) {} {})
;; => ("fasle" "hey" false 21)
(f-evlis "foobar" {} {})
;; => ()
(f-evlis '(1 2 3) {} {})
;; => (1 2 3)

(defn f-make-function [variables body env fenv]
  (fn [& values]
    (f-eprogn body (extend env variables values) fenv)))

((f-evaluate '(lambda (a b) a) {} {}) 1 2)
;; => 1
;; TODO should the above have worked without a definition of `evaluate-application`? Why?

(defn evaluate-application [fn args env fenv]
  (cond (symbol? fn)
        ;; symbol single-level function (no nesting) =>
        ;; bind functional symbol to var + invoke it
        (invoke (lookup fn fenv) args)
        ;; if it's a list i.e. contains another function
        ;; then we need to bind the variables in the child
        ;; list with a call to `eprogn`
        (and (list fn) (= (first fn) 'lambda))
        (f-eprogn (nnext fn)
                  (extend env (first (rest fn)) args)
                  fenv)
        :else (wrong "Incorrect functional term" fn {:fn fn
                                                     :args args
                                                     :env env
                                                     :fenv fenv})))

(f-evaluate '((lambda (a)
                    ;; `a` isn't present in the environment visible to this inner lambda
                    ((lambda (b) (list a b))
                     (+ 2 a)))
            1)
          ;; don't need to put anything in env
          {}
          ;; have to provide relevant host functions in fenv
          {'+ +
           'list list})
;; => (1 3)

;; In the book at this point there is some discussion regarding efficiencies
;; gained from separating out `env` and `f-env`; in particular around simplifying
;; `invoke` to no longer check if the `fn` parameter is indeed a `fn` (because
;; it must be). I haven't implemented this here as it isn't very interesting.
;;
;; Also, the efficiency gains really aren't so important here because we are
;; using a hashmap and no an association list.

;; comparing `f-evaluate` and `evaluate`
(f-evaluate '(if true (+ 1 2) (* 3 4)) {} {'+ + '* *})
;; => 3

(evaluate '(if true (+ 1 2) (* 3 4)) {'+ + '* *})
;; => 3

(evaluate '((if true + *) 3 4) {'+ + '* *})
;; => 7

;; this is the motivator for `funcall` - this example doesn't work with `f-evaluate`
(try
  (f-evaluate '((if true + *) 3 4) {} {'+ + '* *})
  (assert false "f-evaluate should fail with this expression")
  (catch clojure.lang.ExceptionInfo e
    (str "caught exception: " (.getMessage e))))
;; => "caught exception: Incorrect functional term"

;; LISP-2 avoids Clojurey/LISP-1 problems with names like `key` `name` etc. i.e. shadowed symbols

(defn funcall
  "Applies its first argument (which should be a function) to the rest of the args."
  [args]
  (fn [& args]
    (if (> (count args) 1)
      (invoke (first args) (rest args))
      (wrong "Incorrect arity" 'funcall {:args args}))))

;; this should work
(f-evaluate (funcall '((if true (function +) (function *)) 3 4)) {} {'+ + '* * 'funcall funcall})

;; ...but it doesn't. The expected result here is that `f-evaluate` evaluates the
;; expression and returns either + or * which then gets called by `funcall`.
;; I can't really see how this can work without adding a `case` in `f-evaluate` for `funcall`.
;; SOLUTION: pass `funcall` in the fenv (function environment).

(funcall '((if true (function +) (function *)) 3 4))
;; => #function[ch01-jack/funcall/fn--9477]

;; separately, also add `function` to `f-evaluate`
(f-evaluate '(function +) {} {'+ +})
;; => #function[clojure.core/+]
