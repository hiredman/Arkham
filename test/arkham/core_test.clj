(ns arkham.core-test
  (:use [arkham.core]
        [clojure.test]))

(deftest test-evil
  (testing "symbol resolution"
    (is (thrown? Exception (evil 'f))))
  (testing "function calling"
    (is (= 3
           (evil '(+ 1 2)))))
  (testing "special forms"
    ;; TODO:
    ;; letfn* fn* set! try
    ;; catch finally deftype* case* reify*
    ;; DONE:
    ;; . if let* do quote var new throw loop/recur def
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
        (is (= @(ns-resolve ns s) 1))))
    (testing "fn*"
      (is (= 1 (evil '((let [x 1] (fn [] x))))))))
  (testing "get-var"
    (is (= evil (evil 'eval))))
  (testing "ctor and dot"
    (is (thrown? Exception (evil '(Thread.))))
    (is (thrown? Exception (evil '(.invoke (var +) 1 2))))))

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
