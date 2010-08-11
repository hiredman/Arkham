(ns arkham.core
  (:use [clojure.contrib.macro-utils :only [mexpand-all]]))

(defrecord SpecialFrame [tag args])

(defmethod print-method SpecialFrame [o w]
  (.write w "#=<>"))

(defmulti meval (comp type second))

(defmethod meval :default [[stack exp]]
  [stack exp])

(defmethod meval clojure.lang.Symbol [[stack exp]]
  [stack
   (if (= exp '*STACK*)
     stack
     (if-let [bound  (->> stack
                          (remove #(= SpecialFrame (type %)))
                          (filter #(contains? % exp))
                          first)]
       (get bound exp)
       (if-let [v (ns-resolve *ns* exp)]
         @v
         (throw
          (Exception.
           (format
            "Unable to resolve symbol: %s in this context" (name exp)))))))])

(defmulti eval-seq (fn [[stack [first & _]]] first))

(defmethod meval clojure.lang.ISeq [[state exp]]
  (trampoline eval-seq [state exp]))

(defmethod eval-seq :default [[state [op & args]]]
  (let [op (second (meval [state op]))
        args (doall (map (fn [x] (second (meval [state x]))) args))]
    [state (apply op args)]))

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

(defmethod eval-seq 'quote [[stack [_ name]]]
  [stack name])

(defmethod eval-seq 'var [[stack [_ & body]]]
  [stack (ns-resolve *ns* (first body))])

(defmethod eval-seq 'if [[stack [_ a b c]]]
  [stack
   (if (second (meval [stack a]))
     (second (meval [stack b]))
     (second (meval [stack c])))])

(defmethod eval-seq '. [[stack [_ target name- & args]]]
  [stack
   (if (class? target)
     (throw (Exception. "huh?"))
     (clojure.lang.Reflector/invokeInstanceMethod
      (second (meval [stack target]))
      (name name-)
      (into-array Object
                  (map #(second (meval [stack %])) args))))])

(defmethod eval-seq 'new [[stack [_ class & args]]]
  [stack
   (clojure.lang.Reflector/invokeConstructor
    (second (meval [stack (list 'var class)]))
    (into-array Object
                (map #(second (meval [stack %])) args)))])

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
                  (if (not (and (= SpecialFrame (type s))
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

(defmethod eval-seq 'fn* [[stack [_ & bodies]]]
  [stack
   (let [max-args (if-let [m (->> bodies
                                  (map first)
                                  (map #(.contains % '&))
                                  (some true?))]
                    m
                    (apply max (map count (map first bodies))))]
     max-args)]
  #_(fn [& args]
      (second
       (meval
        [(conj stack (into {} (map vector bindings args)))
         (cons 'do body)]))))

(defmethod eval-seq 'def [[stack [_ name value]]]
  (let [evalue (second (meval [stack value]))]
    (alter-meta!
     (clojure.lang.Var/intern *ns* name evalue true)
     (constantly (meta name)))))

(defn evil [exp]
  (second (meval [() (mexpand-all exp)])))

(defn evil* [exp]
  (meval [() (mexpand-all exp)]))
