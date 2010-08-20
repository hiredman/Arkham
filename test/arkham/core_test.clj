(ns arkham.core-test
  (:use [arkham.core]
        [clojure.test])
  (:require [arkham.bars]))

(deftest test-evil
  (testing "symbol resolution"
    (is (thrown? Exception (evil (gensym)))))
  (testing "function calling"
    (is (= 3 (evil '(+ 1 2)))))
  (testing "binding"
    (is (= 3 (evil '(binding [* +] (* 1 2))))))
  (testing "special forms"
    ;; TODO:
    ;; set!
    ;; deftype* case* reify*
    ;; DONE:
    ;; . if let* do quote var new throw loop/recur def fn*
    ;; try/catch/finally letfn*
    (testing "do"
      (is (= 1 (evil '(do 2 1))))
      (is (= 2 (evil '(do 1 (+ 1 1))))))
    (testing "quote"
      (is (= 'a (evil '(quote a)))))
    (testing "var"
      (is (= #'+ (evil '(var +)))))
    (testing "let*"
      (is (= 5 (evil '(let* [x (+ 1 2) y 2] (+ x y))))))
    (testing "if"
      (is (= 4 (evil '(if (zero? (- 1 2)) 1 4)))))
    (testing "."
      (is (= 4 (evil '(.invoke + 2 2)))))
    (testing "throw"
      (is (thrown? IllegalArgumentException
                   (evil '(throw (IllegalArgumentException. "foo"))))))
    (testing "def"
      (let [s (gensym)
            ns (create-ns s)]
        (binding [*ns* ns]
          (evil (list 'def s 1)))
        (is (= @(ns-resolve ns s) 1)))))
  (testing "get-var"
    (is (= evil (evil 'eval))))
  (testing "ctor and dot"
    (is (thrown? Exception (evil '(Thread.))))
    (is (thrown? Exception (evil '(.invoke (var +) 1 2))))
    (is (string? (evil '(System/getenv "USER")))))
  (testing "leftfn*"
    (is (= 1 (evil '(letfn [(foo [x] x) (bar [x] (foo x))] (bar 1)))))))

(deftest test-loop-recur
  (testing "loop/recur"
    (is (= :end (evil '(loop [x 1] (if (= x 2) :end (recur (inc x)))))))
    (is (= [2 2 2 2]
             (evil '(loop [x 0 s []]
                      (if (= x 4)
                        s
                        (recur (inc x) (conj s (count *STACK*)))))))
        "loop/recur uses a constant stack space")
    (is (= 1 (evil '(loop [[x & xs] [3 2 1]] (if (= 1 x) x (recur xs))))))))

(deftest test-fn*
  (testing "fn*"
    (is (= 1 (evil '((let [x 1] (fn [] x)))))
        "fn* closes over lexical scope")
    (is (= [1 2]
             (evil '(let [x (fn ([x] x) ([x y] y))]
                      [(x 1) (x 1 2)]))))
    (is (= 5 (evil '((fn [x]
                       (if (= x 5)
                         x
                         (recur (inc x)))) 0))))
    (is (= [1 2 3 4]
             (evil
              '((fn [a b c & xs] (vector a b c (first xs))) 1 2 3 4))))
    (is (= '+ (evil '((fn [x] x) '+))) "no double eviling")))

(deftest test-try-catch-finally
  (testing "try/catch/finally"
    (is (= 1 (evil '(try 1 (catch Exception _)))))
    (is (= 2 (evil '(try (throw (Exception. "foo")) (catch Exception _ 2)))))
    (is (thrown? Exception (evil'(try
                                   (throw (Exception. "foo"))
                                   (catch ArithmeticException _
                                     2)))))
    (is (= [1 2] (evil '(let [x (java.util.HashMap.)]
                          (try
                            (throw (Exception. "foo"))
                            (catch Exception _
                              (.put x :a 1))
                            (finally
                             (.put x :b 2)))
                          [(.get x :a) (.get x :b)]))))))
