(ns arkham.core
  (:use [clojure.contrib.macro-utils :only [mexpand-all]]))

(declare dot ctor get-var dot-static)

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
                          (map #(if (instance? clojure.lang.IDeref %)
                                  @%
                                  %))
                          (filter #(contains? % exp))
                          first)]
       (get bound exp)
       @(get-var (ns-resolve *ns* exp) exp)))])

(defmulti eval-seq (fn [[stack [first & _]]] first))

(defmethod meval clojure.lang.ISeq [[state exp]]
  (trampoline eval-seq [state exp]))

(defmethod meval clojure.lang.IPersistentVector [[stack exp]]
  [stack
   (->> exp
        (map #(second (meval [stack %])))
        vec)])

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
      (meval [stack1 (cons 'do body)])))])

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
  (let [n (ns-resolve *ns* target)]
    [stack
     (if (class? n)
       (dot-static n name- args stack)
       (dot (second (meval [stack target])) name- args stack))]))

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

(defn max-args [bodies]
  (if-let [m (->> bodies
                  (map first)
                  (map #(.contains % '&))
                  (some true?))]
    (->> bodies
         (map first)
         (filter #(.contains % '&))
         first
         (take-while #(not= '& %))
         count
         vector)
    (apply max (map count (map first bodies)))))

(defn max-non-varg [bodies]
  (->> bodies
       (remove #(.contains (first %) '&))
       (map first)
       (map count)
       (cons 0)
       (apply max)))

(defn fn-no-varg-eval [bodies stack args fn-name]
  (let [[params & body] (->> bodies
                             (filter #(= (count args)
                                         (count (first %))))
                             first)
        stack1 (conj stack
                     (SpecialFrame. :loop [params body])
                     (zipmap params args))
        body (conj body 'do)]
    (second (meval [stack1 body]))))

(defn fn-varg-eval [bodies stack args fn-name]
  (let [[params & body] (->> bodies
                             (filter #(.contains (first %) '&))
                             first)
        normal-params (take-while #(not= '& %) params)
        varg-param (first (drop (inc (count normal-params)) params))
        normal-args (take (count normal-params) args)
        vargs (drop (count normal-args) args)
        stack1 (conj stack
                     (SpecialFrame. :loop [(concat normal-params
                                                   [varg-param])
                                           body])
                     (assoc (zipmap normal-params normal-args)
                       varg-param vargs))
        body (conj body 'do)]
    (second (meval [stack1 body]))))

(defmethod eval-seq 'fn* [[stack [_ & args]]]
  (let [fn-name (when (symbol? (first args))
                  (first args))
        args (if fn-name
               (rest args)
               args)
        bodies (if (vector? (first args))
                 (list args)
                 args)
        max-args (max-args bodies)
        max-non-varg (max-non-varg bodies)]
    [stack (fn thisfn [& args]
             (let [stack (if fn-name (conj stack {fn-name thisfn}) stack)]
               (cond
                (and (number? max-args)
                     (> (count args) max-args))
                (throw (Exception.
                        (format "to many arguments to %s (%s)"
                                (or fn-name thisfn)
                                max-args)))
                (>= max-non-varg (count args))
                (fn-no-varg-eval bodies stack args fn-name)
                (vector? max-args)
                (fn-varg-eval bodies stack args fn-name)
                :else
                (throw (Exception. "... the hell?")))))]))

(defmethod eval-seq 'try [[stack [_ & forms]]]
  (let [body (take-while #(or (not (seq? %))
                              (and (not= (first %) 'catch)
                                   (not= (first %) 'finally)))
                         forms)
        catch (->> forms (drop (count body))
                   (filter #(= 'catch (first %))))
        finally (first (drop (+ (count catch) (count body)) forms))]
    (try
      (meval [stack (conj body 'do)])
      (catch Throwable t
        (let [body (->> catch
                        (filter #(isa? (type t) (ns-resolve *ns* (second %))))
                        first)]
          (if-not (seq body)
            (throw t)
            (let [[_ class name & body] body
                  body (conj body 'do)
                  stack1 (conj stack {name t})]
              [stack (second (meval [stack1 body]))]))))
      (finally
       (meval [stack (conj (rest finally) 'do)])))))

(defmethod eval-seq 'def [[stack [_ name value]]]
  (let [evalue (second (meval [stack value]))]
    (alter-meta!
     (clojure.lang.Var/intern *ns* name evalue true)
     (constantly (meta name)))))

(defmethod eval-seq 'letfn* [[stack [_ bindings & body]]]
  (let [stack1 (conj stack (promise))
        table (zipmap (take-nth 2 bindings)
                     (map
                      #(second (meval [stack1 %]))
                      (take-nth 2 (rest bindings))))]
    (deliver (first stack1) table)
    (meval [stack1 (cons 'do body)])))


(defn evil [exp]
  (second (meval [() (mexpand-all exp)])))

(defn evil* [exp]
  (meval [() (mexpand-all exp)]))

(defmulti ctor (comp first list))

(defn ctor-default [class stack args]
  (clojure.lang.Reflector/invokeConstructor class
   (into-array Object (map #(second (meval [stack %])) args))))

(defmethod ctor :default [class stack args] (ctor-default class stack args))

(defmulti dot (fn [target method args stack] [(class target) method]))

(defn dot-default [target method args stack]
  (clojure.lang.Reflector/invokeInstanceMethod
   target (name method)
   (into-array Object (map #(second (meval [stack %])) args))))

(defmethod dot :default [target method args stack]
  (dot-default target method args stack))

(defmulti get-var (comp first list))

(defn get-var-default [var sym] var)

(defmethod get-var :default [var sym] (get-var-default var sym))

(defmulti dot-static (fn [target method args stack] [target method]))

(defn dot-static-default [target method args stack]
  (clojure.lang.Reflector/invokeStaticMethod (.getName target) (name method)
   (into-array Object (map #(second (meval [stack %])) args))))

(defmethod dot-static :default [target method args stack]
  (dot-static-default target method args stack))
