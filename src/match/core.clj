(ns match.core
  (:use [clojure.core.logic.unify :only [unifier unifier* binding-map
                                         binding-map* prep replace-lvar
                                         lvarq-sym? rem-?]]
        [clojure.walk :only [postwalk]])
  (:require [clojure.core.logic.minikanren :as mk]))

(defmacro cond-let [[name value] & body]
    `(let [~name ~value]
       ~((reduce
          (fn [fun [condition expr]]
             (fn [else]
               (fun
                `(if-let [~name ~condition]
                   ~expr
                   ~else))))
           identity
           (partition-all 2 body)) nil)))

(defn emittable [expr]
  (postwalk
   (fn [expr]
     (cond
      (mk/lvar? expr)
      `(clojure.core.logic.minikanren.LVar.
        ~(.name expr)
        ~(.hash expr)
        ~(.cs expr)
        ~(.meta expr))
      (symbol? expr)
      `(quote ~expr)
      (seq? expr)
      `(list ~@expr)
      (mk/lcons? expr)
      `(mk/lcons
        ~(emittable (.a expr))
        ~(emittable (.d expr)))
      :else expr))
   expr))

(defmacro cond-m [value & matches]
  (let [value-name (gensym)
        match-name (gensym)]
    `(let [~value-name ~value]
       (cond-let [~match-name nil]
                 ~@(for [m (partition-all 2 matches)
                         :let [[pattern body] m
                               lvars (keys (:lvars (meta (prep pattern))))
                               matcher `(binding-map* ~(let [p (prep pattern)]
                                                         `(with-meta
                                                            ~(emittable p)
                                                            ~(emittable
                                                              (meta p))))
                                                      ~value-name)
                               body `(let [~(zipmap
                                             (->> lvars
                                                  (map name)
                                                  (map (partial drop 1))
                                                  (map (partial apply str))
                                                  (map symbol))
                                             (map #(list 'quote %) lvars))
                                           ~match-name]
                                       ~body)]
                         x [matcher body]]
                     x)))))

;; TODO: do branching, e.g. instead of building a vector and trying to
;; unify try and unify the first arg, then the second, then the third,
;; merge the binding maps, may require changes to cond-m instead of
;; nm2
;; maybe cond-m-tree
(defmacro mn
  "creates a fn that runs a body based on pattern matching"
  [name-or-first-clause & bodies]
  (let [fn-name (if (symbol? name-or-first-clause)
                  name-or-first-clause
                  (gensym "fn"))
        bodies (if-not (symbol? name-or-first-clause)
                 (conj bodies name-or-first-clause)
                 bodies)
        m (for [[arity clauses] (group-by
                                 #(count (first %)) (partition-all 2 bodies))]
            (let [args (vec (take arity (repeatedly (partial gensym 'arg))))]
              `(~args (cond-m ~args
                              ~@(apply concat clauses)
                              ?# (throw
                                  (IllegalArgumentException.
                                   "no matching clause"))))))]
    `(fn* ~fn-name ~@m)))

(defmacro defmn [name doc-string? & body]
  (let [[doc-string body] (if (string? doc-string?)
                            [doc-string? body]
                            [nil (cons doc-string? body)])]
    `(def ~(vary-meta name assoc :doc doc-string) (mn ~@body))))

(defmn secd
  "executes a primitve secd machine, takes a data stack, environment map,
  control stack, and dump stack"
  [(?v . nil) ?e' () ()]
  v

  [(?v . nil) ?e' () ([?s ?e ?c] . ?d)]
  (recur (cons v s) e c d)

  [?s ?e ([:term [:lit ?n]] . ?c) ?d]
  (recur (cons [:int n] s) e c d)

  [?s ?e ([:term [:var ?x]] . ?c) ?d]
  (recur (cons (get e x) s) e c d)

  [?s ?e ([:term [:lam [?x ?t]]] . ?c) ?d]
  (recur (cons [:closure [e x t]] s) e c d)

  [?s ?e ([:term [:app [?t0 ?t1]]] . ?c) ?d]
  (recur s e (list* [:term t1] [:term t0] :apply c) d)

  [(:succ [:int ?n] . ?s) ?e (:apply . ?c) ?d]
  (recur (cons [:int (inc n)] s) e c d)

  [([:closure [?e' ?x ?t]] ?v . ?s) ?e (:apply . ?c) ?d]
  (recur () (assoc e' x v) (list [:term t]) (cons [s e c] d)))


(comment

  ((mn
    (1 . ?a) (println a))
   '[1 2])

  (secd () ; data stack
        '{inc :succ} ; environment
        '([:term [:lit 1]] [:term [:var inc]] :apply) ; control stack
        () ; dump stack
        )
  ;; => [:int 2]

  ((mn
    [?a] (println a)
    [?a ?b] (println a b))
   1 2)

  (cond-let [name init]
            som)

  (defmacro cond-let [[name value] & body]
    `(let [~name ~value]
       ~((reduce
          (fn [fun [condition expr]]
             (fn [else]
               (fun
                `(if-let [~name ~condition]
                   ~expr
                   ~else))))
           identity
           (partition-all 2 body)) nil)))

  )
