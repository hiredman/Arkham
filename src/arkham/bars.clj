(ns arkham.bars
  (:use [arkham.core]))

(defmethod ctor Thread [& _]
  (throw (Exception. "DENIED")))

(defmethod dot [clojure.lang.Var 'invoke] [& _]
  (throw (Exception. "DENIED")))

(defmethod get-var nil [_ sym]
  (throw
   (Exception.
    (format "Unable to resolve symbol: %s in this context" (name sym)))))

(defmethod get-var #'clojure.core/eval [_ _]  #'arkham.core/evil)

(defmethod get-var #'clojure.core/future-call [_ _]
  (throw (Exception. "DENIED")))

(defmethod get-var #'clojure.core/agent [_ _]
  (throw (Exception. "DENIED")))

(defmethod ctor clojure.lang.Agent [& _]
  (throw (Exception. "DENIED")))

(defmethod get-var #'clojure.core/alter-var-root [_ _]
  (throw (Exception. "DENIED")))

(defmethod dot [clojure.lang.Var 'alter] [& _]
  (throw (Exception. "DENIED")))

(defmethod dot [clojure.lang.Var 'alterRoot] [& _]
  (throw (Exception. "DENIED")))
