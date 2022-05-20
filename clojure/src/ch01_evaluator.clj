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
(assert (true? (atom? 1)))
(assert (true? (atom? "ahoj")))
(assert (true? (atom? \c)))
(assert (false? (atom? '(1 2 3))))
;; TODO: notice that vectors are considered atoms!
(assert (true? (atom? [1 2 3])))
(assert (true? (atom? {1 2 10 20})))
;; while keywords are not really supported by our language's evaluate function we consider them atomic
(assert (true? (atom? :atomic)))

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

(assert (= 1 (evaluate 1 {})))
(assert (= "ahoj" (evaluate "ahoj" {})))
(assert (= [1 2 3] (evaluate [1 2 3] {})))

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
    (f args)
    (wrong "Not a function" f args)))


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

(assert (= '{x 1}
           (evaluate '(set! x 1) {})))

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
    (f args)
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
(assert (= 10
           (lookup 'a '{a 10 b 20})))

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
(assert (= 1
           (evaluate '((lambda (a b) a)
                       1 2)
                     {})))

;; Let's try again with `& values`
;; notice that `env` is unused which leads to the problem described below.
(defn make-function [variables body env]
  (fn [values]
    (eprogn body (extend env-init variables values))))
(evaluate '((lambda (a b) a)
            1 2)
          {})
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
  (fn [values]
    (eprogn body (extend env-global variables values))))

;; For demo, let's add the `+` to our global env.
;; Adding custom functions to the env is a bit tricky at this point
;; See how Chouser did it: https://github.com/Chouser/lisp-in-small-pieces-clj/blob/main/src/us/chouser/LISP/ch1a.clj#L151-L155
(def env-global (extend env-global
                  '[list +]
                  [identity ; list returns the sequence/list it receives as an argument
                   #(apply + %)]))

;; This is more relevant - we create a lambda that uses `car`.
(assert (= 50
           (evaluate '((lambda (a b) (+ a b))
                       30 20)
                     env-global)))

;; This is all great but we still have a  problem: nested lambdas don't work
;; The environment of the outer function isn't available to the inner function
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
;; This corresponds to a **dynamic Lisp**.
;; See also Chouser's solution for this section: https://github.com/Chouser/lisp-in-small-pieces-clj/blob/main/src/us/chouser/LISP/ch1a.clj#L177
;;
;; Our, quick and naive, solution is to just pass the current environment
;; to `invoke` which then passes it to the functions being invoked.
;; In both cases, the env is passed as the last argument.
;; We'll redefine `evaluate`, `make-function`, and `invoke`.
;; To avoid confusion with previous definitions we well use new names with `d-` prefix
;; (this is also used because the solution won't be the final one that we'll use)

;; `d-make-function` calls `eprogn` - they don't mention this in the book,
;; but I think we need to update `eprogn` too because it also calls `evaluate` - see below. 
(declare d-evaluate)
(defn d-eprogn
  "Evaluates sequence of expressions in given environment."
  [exps env]
  (if (list? exps)
    (let [[fst & rst] exps
          ;; CHANGE:
          fst-val (d-evaluate fst env)]
      (if (list? rst)
        ;; CHANGE:
        (d-eprogn rst env)
        fst-val))
    ()))

;; ... and `evlist` call in `d-evaluate` calls `evaluate`
;; so we need to redefine that too
(defn d-evlis [exps env]
  (if (list? exps)
    ;; CHANGE:
    (map #(d-evaluate % env) exps)
    ()))


;; `d-invoke`, unlike `invoke` accepts an environment as 3rd argument
(defn d-invoke [f args env]
  (if (fn? f)
    (f args env)
    (wrong "Not a function" f args)))

;; Notice how we have two environments here:
;; - `def-env` is the environment at the time the function is defined/created - it's not used here at all
;; - `current-env` is the environment at the time the function is called - this is the one we are using
(defn d-make-function [variables body def-env]
  ;; I'm wondering how does Scheme makes it work out of the box as in `(lambda (values current.env))`
  (fn [values current-env]
    (d-eprogn body (extend current-env variables values))))

;; Note: `d-make-function` calls `eprogn` - they don't mention this in the book,
;; but I think we need to update `eprogn` too because it also calls `evaluate` - see below. 

;; Inside `evaluate`, we only change the way `invoke` is called - adding `env` as an extra parameter
(defn d-evaluate [exp env]
  (if (atom? exp)
    (cond
      (symbol? exp) (lookup exp env)
      ((some-fn number? string? char? boolean? vector?) exp) exp
      :else (wrong "Cannot evaluate - unknown atomic expression?" exp))

    (case (first exp)
      quote (second exp)
      if (if (d-evaluate (second exp) env)
           (d-evaluate (nth exp 2) env)
           (d-evaluate (nth exp 3) env))
      begin (d-eprogn (rest exp) env)
      set! (update! (second exp) env (d-evaluate (nth exp 2) env))
      ;; CHANGE: call `d-make-function` instead of `make-function`
      lambda (d-make-function (second exp) (nnext exp) env)
      ;; CHANGE: call `d-invoke` instead of `invoke` and also `d-evaluate`
      (d-invoke (d-evaluate (first exp) env)
              (d-evlis (rest exp) env)
              ;; CHANGE: env is passed in as a new argument
              env))))

;; Adding custom functions to the env is a bit tricky at this point
;; See how Chouser did it: https://github.com/Chouser/lisp-in-small-pieces-clj/blob/main/src/us/chouser/LISP/ch1a.clj#L216-L220
(def d-env-global (extend env-global
                    '[list +]
                    [(fn [args env] args)
                     (fn [args env] (apply + args))]))

;; just check that a simple example still works
(assert (= 50
           (d-evaluate '((lambda (a b) (+ a b))
                         30 20)
                       d-env-global)))

;; BUT the main point is to verify that the example not working before
;; works now without problems:
(assert (= '(1 3)
           (d-evaluate '((lambda (a)
                                 ;; `a` isn't present in the environment visible to this inner lambda
                                 ((lambda (b) (list a b))
                                  (+ 2 a)))
                         1)
                       d-env-global)))

;; But we still have a problem - let's look at another example of nested lambdas:
;; Notice that the outer lambda now returns an inner function without applying it immediately
;; The issue is that at the time the inner lambda is called, `a` already felt out of scope
;; and is not present in `current-env`.
;; `a` is in the scope only when the outer lambda is "being constructed".
;;
;; To see this in action, you can try to debug `d-make-function`
(comment
  (d-evaluate '(((lambda (a)
                         (lambda (b) (list a b)))
                 1)
                2)
              d-env-global)
  ;; No such binding
  ;; {:expression a, :args nil}

  .)




;; iv) Fixing the Problem:
;; After the detour in iii), we finally get a proper implementation.
;; The idea is simple: use the `env` passed as an argument to `make-function`
;; and pass it further to `eprogn` executed when the defined lambda is called.
;; The only thing we need to update is `make-function`
(defn make-function [variables body env]
  (fn [values]
    ;; use `env`, not `env-global` here: 
    (eprogn body (extend env variables values))))

;; NOW IT WORKS!
(assert (= '(1 2)
           (evaluate '(((lambda (a)
                                (lambda (b) (list a b)))
                        1)
                       2)
                     env-global)))

;; also another form of nested lambdas that we fixed with iii)
(assert (= '(1 3)
           (evaluate '((lambda (a)
                                 ;; `a` isn't present in the environment visible to this inner lambda
                               ((lambda (b) (list a b))
                                (+ 2 a)))
                       1)
                     env-global)))

;; For better understanding, you can apply it only once and inspect the result
(def my-f (evaluate '((lambda (a)
                              (lambda (b) (list a b)))
                      1)
                    env-global))
;; with cider-inspect you will see this:
;; Class: ch01_evaluator$make_function$fn__13478
;; Value: "#function[ch01-evaluator/make-function/fn--13478]"
;; ---
;; Fields:
;;   "__methodImplCache" = nil
;;   "body" = ( ( list a b ) )
;;   "env" = { list clojure.lang.PersistentList$Primordial@5334cdf2, + ch01_evaluator$fn__13407@565b3603, a 1 }
;;   "variables" = ( b )
;; Static fields:
;;   "const__0" = #'ch01-evaluator/eprogn
;;   "const__1" = #'ch01-evaluator/extend



;;; 1.6.1 Dynamic and Lexical Binding (p.19-23
;;; In the previous section, we have seen a *dynamic* lisp
;;; in the subsection "Improving the patch".
;;; A dynamic Lisp, uses the current environment of the function being applied
;;; to determine the values of variables - the outer function,
;;; must extend its current execution environment with the values of its variables
;;; _at the time of application_.
;;; On the other hand, a *lexical* Lisp extends the environment passed in
;;; _at the time of definition_ (i.e. when the function is being created/defined).

;; This example works only in a dynamic Lisp!
;; `foo` wouldn't normally see the value of `y` but it can see it when executed from within `bar`
(def y 0)
(defn foo [x] (list x y))
(defn bar [y] (foo 1991))
;; You can see that Clojure is a lexical Lisp
(list (bar 100) (foo 3))
;; => ((1991 0) (3 0))
;; In a dynamic Lisp, the value of y visible from within `foo` would be 100,
;; so it would return ((1991 100) (3 0))


;; Historic note: Lisp 1.0 was defined as a dynamic Lisp,
;; but early on, John McCarthy realized that the following expression
;; should return (2 3) but instead it returned (1 3)
;; because a was 1 in current-env at the time of the function application
(comment
  (let ((a 1))
    ;; after this block completed, the a->2 binding fell out of scope,
    ;; so a was 1 and thus the inner lambda returned (1 3)
    ((let ((a 2))
       (lambda  (b) (list a b))))
    3)
.)
;; to mitigate this, they introduced a new special form `function`,
;; whose argument is `lambda` and it returns a proper closure,
;; that is "a function associated with its definition environment".

;; In the book, on p.21-22 they show how to extend `d-evaluate`
;; with `d-make-closure` but I'm not adding  a fully-working example here
;; because it's not something we'll need
;; Here I only include `d-make-closure` definition and a fragment of its usage inside hypothetical `d-evaluate`
(defn d-make-closure [fun env]
  ;; notice how `current-env` isn't used at all and instead it uses the definition environment
  (fn [values current-env]
    (fun values env)))
;; how it could be used inside `d-evaluate`
(comment
  (defn d-evaluate [exp env]
    (case (first exp)
      ,,,
      function (let [[_ [[args body]]] (second exp)
                     fun (d-make-function args body)]
                 (d-make-closure fun env))
      lambda (d-make-function (second exp) (nnext exp) env)))
  
  .)



;;; 1.7 Global Environment (p.27)
;;; We define some basic functions to form our "standard library"
;;; - things like `cons`, `car`, etc.

(def env-global env-init)

;; First define two helper macros that will "push" the definitions into the global environment
;; Macros have to be used because the names we are about to define can't be resolved
;; until they are defined :).

;; Here's what I'm trying to achieve - `(definitial car first)`
;; should add 'car to global-env so it looks like this: {'car first}
;; and the lookup can then work:
((lookup 'car {'car first} ) [1 2 3])
;; => 1

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

(comment
  (definitial foo1)
  ;; macroexpands to a call of itself:
  (definitial foo1 :ch01-evaluator/void)
  ;; ... that macroexpands further: 
  (alter-var-root #'env-global #(extend % ['foo1] [:ch01-evaluator/void]))
  ;; => {foo1 :ch01-evaluator/void}

  ;; check that foo1 symbol is indeed in the global env
  (lookup 'foo1 env-global)
  ;; => :ch01-evaluator/void

  ;; trying to call 'foo1 doesn't make sense yet because it has no value/function associated with it
  ;; - it should fail but returns nil:
  ((lookup 'foo1 env-global) [1 2 3])
;; => nil
  .)


(defmacro defprimitive
  "Defines a primitive operation denoted by the symbol with given name,
  implemented as function f of given arity."
  [name f arity]
  `(definitial
     ~name
     (fn [~'values]
       (if (= ~arity (count ~'values))
         (apply ~f ~'values)
         (wrong "Incorrect ~arity" [~f ~'values])))))

(comment

  (def env-global env-init)

  ;; when foo1 isn't defined yet:
  #_((lookup 'foo1 env-global) '([1 2 3]))
  ;; No such binding
  ;; {:expression foo1, :args nil}

  (defprimitive foo1 first 1)
  ;; calling macroexpand-all on the above form:
  (alter-var-root
   #'env-global
   #(extend
        %
      ['foo1]
      [(fn*
        ([values]
         (if (= 1 (count values))
           (apply first values)
           (wrong "Incorrect ~arity" [first values]))))]))

  ;; After foo1 is defined, it should return the first item of the collection
  ;; notice we wrap the input argument within a list to simulate what `evaluate` does
  ((lookup 'foo1 env-global) '([1 2 3]))
  ;; => 1

.)


;; can `definitial` and `defprimive` be simple macros?
;; => NO - the name symbol would be evaluated prior the function call.
(comment
  (defn definitial
    ([name]
     ;; (prn name) ; the value of name
     ;; (prn (type name)) ; clojure.lang.Symbol
     ;; since definitial is a macro, simply ~name will do the job
     (definitial name :ch01-evaluator/void))
    ([name value]
     ;; notice how we use `extend` instead of relying on internal env structure
     (alter-var-root #'env-global #(extend % [name] [value]))))

  (definitial foo1 first)
  ;; Throws:
  ;;   Unable to resolve symbol: foo1 in this context

  ;; so the user would have to quote the name explicitly which isn't idiomatic
  (definitial 'foo1 first)
  ((lookup 'foo1 env-global) [1 2 3])
  ;; => 1


  )


;; p18 - we now define  a few useful constants
(defn define-constants []
  (definitial t true)
  (definitial f false)
  (definitial nil nil))
env-global
;; => {}
(define-constants)
env-global
;; => {t true, f false, nil nil}

(evaluate '(if t 1 0) env-global)
;; => 1

;; bindings such as `t` and `f` representing basic constants are good
;; but it can lead to subtle errors - shadowing such bindings it's very easy.
;; Consider this example:
(evaluate '((lambda (t) (if t 1 0))
            false)
           env-global)
;; `t` is false?!
;; => 0

;; Btw. does Clojure allows shadowing true, false et al?
;; => No!
#_((fn [true] true)
 false)
;;    Call to clojure.core/fn did not conform to spec.

;; ... so another approach is to lock immutability of t and f in the interpreter
(defn evaluate [exp env]
  (if (atom? exp)
    (cond
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

;; ... now the true is true regardless of what local binding we use
;; - not saying that can't be confusing either
(evaluate '((lambda (t) (if t 1 0))
            false)
           env-global)
;; => 1

