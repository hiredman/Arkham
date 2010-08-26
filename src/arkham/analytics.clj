(ns arkham.analytics
  (:use [arkham.core]
        [clojure.java.io :only [reader]]))

(defrecord Mock [payload])

(def *report* nil)

(defmethod eval-seq 'new [[stack [_ class & args]]]
  (let [class1 (ns-resolve *ns* class)]
    (doall (map (fn [x] (second (meval [stack x]))) args))
    (when-not (some #{class} (@*report* :deftype))
      (swap! *report* update-in [:class] conj class))
    [stack (Mock. class)]))

(defmethod eval-seq '. [[stack [_ target name- & args]]]
  (let [target (second (meval [stack target]))
        args (doall (map (fn [x] (second (meval [stack x]))) args))]
    (swap! *report* update-in [:method] conj [name- target])
    [stack (Mock. (:payload target))]))

(defmethod eval-seq 'deftype* [[stack exp]]
  (swap! *report* update-in [:deftype] conj (second exp))
  [stack (Mock. nil)])

(defmethod eval-seq 'defrecord [[stack exp]]
  (swap! *report* update-in [:deftype] conj (second exp))
  [stack (Mock. nil)])

(defmethod eval-seq :default [[state [op & args]]]
  (let [op (second (meval [state op]))
        args (doall (map (fn [x] (second (meval [state x]))) args))]
    (fn []
      [state (Mock. op)])))

(defmethod eval-seq 'def [[stack [_ name value]]]
  (swap! *report* update-in [:global] conj name)
  (meval [stack value])
  [stack (Mock. nil)])

(defmethod eval-seq 'recur [[stack [_ & args]]]
  (doall (map (fn [x] (second (meval [stack x]))) args))
  (Mock. :recur))

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
    (doseq [[args & body] bodies]
      (meval [(conj stack (zipmap args (repeat (Mock. :local))))
              (cons 'do body)]))
    [stack (Mock. :fn)]))

(defmethod eval-seq 'if [[stack [_ a b c]]]
  (second (meval [stack a]))
  (second (meval [stack b]))
  (second (meval [stack c]))
  [stack (Mock. :if)])

(defmethod meval clojure.lang.Symbol [[stack exp]]
  [stack
   (cond
    (= exp '*STACK*)
    stack
    (find-frame stack exp)
    (get (find-frame stack exp) exp)
    (some #{exp} (@*report* :global))
    (Mock. exp)
    (ns-resolve *ns* exp)
    (do
      (swap! *report* update-in [:global] conj exp)
      (Mock. exp))
    (and (namespace exp) (name exp))
    (do
      (swap! *report* update-in [:stactic-field] conj exp)
      (Mock. exp))
    :else
    (do
      (when-not (some #{exp} (@*report* :deftype))
        (swap! *report* update-in [:class] conj exp))
      (Mock. exp)))])

(defmethod eval-seq 'throw [[stack _]]
  [stack (Mock. :throw)])

(def packages #{"java.io" "java.util" "java.util.concurrent" "clojure.lang"})

(defn find-class [class loaded]
  (let [[short full] (->> loaded
                          (filter #(= class (first %)))
                          first)
        [short full] (if (nil? short)
                       (->> packages
                            (filter
                             #(try
                                (Class/forName
                                 (str % "."
                                      (name class)))
                                (catch Exception _)))
                            (map
                             (fn [p]
                               [class (Class/forName
                                       (str p "."
                                            (name class)))]))
                            first)
                       [short full])]
    (when short
      {(symbol
        (.replaceAll (.getName full) (str "\\."(name short)) ""))
       [short]})))

(defn resolve-classes [classes]
  (let [loaded (sort-by
                (comp (memfn getName) second)
                (distinct
                 (mapcat #(try (ns-imports %) (catch Exception _)) (all-ns))))
        classes (distinct classes)]
    (cons :import
          (map
           (fn [[k value]]
             (vec (cons k (seq value))))
           (apply merge-with concat
                  (for [class classes]
                    (find-class class loaded)))))))

(defn generate-imports [file]
  (binding [*report* (atom {:global ['*ns*]})]
    (with-open [rdr (java.io.PushbackReader. (reader file))]
      (let [eof (Object.)]
        (doseq [form (take-while #(not= eof %)
                                 (repeatedly #(read rdr false eof)))]
          (evil form))))
    (-> *report*
        deref
        :class
        resolve-classes)))

(comment

  (generate-imports "/Users/hiredman/src/Arkham/foo.clj")

  )

