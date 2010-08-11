(ns arkham.core
  (:use [clojure.contrib.macro-utils :only [mexpand-all]]))

(declare dot ctor get-var)

(defrecord SpecialFrame [tag args])

(defn special-frame? [frame]
  (= SpecialFrame (type frame)))

(defmethod print-method SpecialFrame [o w]
  (.write w (str "#" (name (:tag o)))))

(defmulti meval (comp type second))

(defmethod meval :default [[stack exp]]
  [stack exp])

(defmethod meval clojure.lang.Symbol [[stack exp]]
  [stack
   (if (= exp '*STACK*)
     stack
     (if-let [bound  (->> stack
                          (remove special-frame?)
                          (filter #(contains? % exp))
                          first)]
       (get bound exp)
       @(get-var (ns-resolve *ns* exp) exp)))])

(defmulti eval-seq (fn [[stack [first & _]]] first))

(defmethod meval clojure.lang.ISeq [[state exp]]
  (trampoline eval-seq [state exp]))

(defmethod eval-seq :default [[state [op & args]]]
  (let [op (second (meval [state op]))
        args (doall (map (fn [x] (second (meval [state x]))) args))]
    (fn []
      [state (apply op args)])))

(defmethod eval-seq 'let* [[stack [_ bindings & body]]]
  [stack
   (second
    (let [stack1 (conj stack
                       (reduce
                        (fn [locals [name value]]
                          (assoc locals
                            name (second (meval [(conj stack locals) value]))))
                        {}
                        (partition 2 bindings)))]
      (meval
       [stack1 (cons 'do body)])))])

(defmethod eval-seq 'do [[stack [_ & body]]]
  [stack
   (second
    (last
     (doall
      (map (fn [e]
             (meval [stack e]))
           body))))])

(defmethod eval-seq 'quote [[stack [_ name]]] [stack name])

(defmethod eval-seq 'var [[stack [_ sym]]]
  ;; this seems off, may still be possible to get a var that you
  ;; aren't supposed to have access to.
  [stack (if-let [v (ns-resolve *ns* sym)]
           (get-var v sym)
           (clojure.lang.Var/intern
            (or (namespace sym) *ns*)
            (symbol (name sym))))])

(defmethod eval-seq 'if [[stack [_ a b c]]]
  [stack
   (if (second (meval [stack a]))
     (second (meval [stack b]))
     (second (meval [stack c])))])

(defmethod eval-seq '. [[stack [_ target name- & args]]]
  (let [target (second (meval [stack target]))]
    [stack
     (if (class? target)
       (throw (Exception. "huh?"))
       (dot target name- args stack))]))

(defmethod eval-seq 'new [[stack [_ class & args]]]
  (let [class (second (meval [stack (list 'var class)]))]
    [stack (ctor class stack args)]))

(defmethod eval-seq 'throw [[stack [_ exception]]]
  (throw (second (meval [stack exception]))))

(defmethod eval-seq 'loop* [[stack [_ bindings & bodies]]]
  [stack
   (second
    (meval
     [(conj stack (SpecialFrame. :loop [(take-nth 2 bindings) bodies]))
      (conj bodies bindings 'let*)]))])

(defmethod eval-seq 'recur [[stack [_ & args]]]
  (fn []
    (let [stack1 (loop [[s & ss] stack]
                   (if (not (and (special-frame? s)
                                 (= :loop (:tag s))))
                     (recur ss)
                     (conj ss s)))
          {tag :tag [bindings bodies] :args} (first stack1)
          args (->> args
                    (map #(meval [stack %]))
                    (map second)
                    (map (partial list 'quote)))
          bindings (vec (mapcat list bindings args))]
      (meval
       [(rest stack1) (conj bodies bindings 'loop*)]))))

(defmethod eval-seq 'fn* [[stack [_ & args]]]
  (let [fn-name (when (symbol? (first args))
                  (first args))
        args (if fn-name
               (rest args)
               args)
        bodies (if (vector? (first args))
                 (list args)
                 args)
        max-args (if-let [m (->> bodies
                                 (map first)
                                 (map #(.contains % '&))
                                 (some true?))]
                   :&
                   (apply max (map count (map first bodies))))]
    [stack (fn thisfn [& args]
             (when (and (number? max-args)
                        (> (count args) max-args))
               (throw (Exception.
                       (format "to many arguments to %s (%s)"
                               (or fn-name thisfn)
                               max-args))))
             (cond
              (and (not= max-args :&)
                   (= 1 (count bodies)))
              (let [[params & body] (first bodies)
                    locals (zipmap params args)
                    stack1 (conj stack locals)
                    body (conj body 'do)]
                (second (meval [stack1 body])))
              (not= max-args :&)
              :foo))]))

(defmethod eval-seq 'def [[stack [_ name value]]]
  (let [evalue (second (meval [stack value]))]
    (alter-meta!
     (clojure.lang.Var/intern *ns* name evalue true)
     (constantly (meta name)))))

(defn evil [exp]
  (second (meval [() (mexpand-all exp)])))

(defn evil* [exp]
  (meval [() (mexpand-all exp)]))

(defmulti ctor (comp first list))

(defmethod ctor :default [class stack args]
  (clojure.lang.Reflector/invokeConstructor
   class
   (into-array Object
               (map #(second (meval [stack %])) args))))

(defmethod ctor Thread [& _]
  (throw (Exception. "DENIED")))


(defmulti dot (fn [target method args stack]
                [(class target) method]))

(defmethod dot :default [target method args stack]
  (clojure.lang.Reflector/invokeInstanceMethod
   target
   (name method)
   (into-array Object
               (map #(second (meval [stack %])) args))))

(defmethod dot [clojure.lang.Var 'invoke] [& _]
  (throw (Exception. "DENIED")))

(defmulti get-var (comp first list))

(defmethod get-var :default [var sym] var)

(defmethod get-var nil [_ sym]
  (throw
   (Exception.
    (format "Unable to resolve symbol: %s in this context" (name sym)))))

(defmethod get-var #'clojure.core/eval [_ _]  #'arkham.core/evil)
