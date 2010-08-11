(ns arkham.core
  (:use [clojure.contrib.macro-utils :only [mexpand-all]]))

(defmulti meval (comp type second))

(defmethod meval :default [[stack exp]]
  [stack exp])

(defmethod meval clojure.lang.Symbol [[[top & _ :as stack] exp]]
  [stack
   (if (contains? top exp)
     (get top exp)
     (if-let [v (ns-resolve *ns* exp)]
       @v
       (throw
        (Exception.
         (format
          "Unable to resolve symbol: %s in this context" (name exp))))))])

(defmulti eval-seq (fn [[stack [first & _]]] first))

(defmethod meval clojure.lang.ISeq [[state exp]]
  [state (eval-seq [state exp])])

(defmethod eval-seq :default [[state [op & args]]]
  (let [op (second (meval [state op]))
        args (doall (map (fn [x] (second (meval [state x]))) args))]
    (apply op args)))

(defmethod eval-seq 'let* [[stack [_ bindings & body]]]
  (second
   (let [stack1 (conj stack (into {} (map vec (partition 2 bindings))))]
     (meval
      [stack1 (cons 'do body)]))))

(defmethod eval-seq 'do [[stack [_ & body]]]
  (second
   (last
    (doall
     (map (fn [e]
            (meval [stack e]))
          body)))))

(defmethod eval-seq 'quote [[stack [_ & body]]]
  (first body))

(defmethod eval-seq 'var [[stack [_ & body]]]
  (ns-resolve *ns* (first body)))

(defmethod eval-seq 'fn* [[stack [_ & bodies]]]
  (println stack bodies)
  (let [max-args (if-let [m (->> bodies
                                 (map first)
                                 (map #(.contains % '&))
                                 (some true?))]
                   m
                   (apply max (map count (map first bodies))))]
    max-args)
  #_(fn [& args]
    (second
     (meval
      [(conj stack (into {} (map vector bindings args)))
       (cons 'do body)]))))

#_(defmethod eval-seq 'loop [[stack [_ bindings & body]]]
  )

(defn evil [exp]
  (second (meval [() (mexpand-all exp)])))
