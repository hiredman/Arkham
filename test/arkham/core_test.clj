(ns arkham.core-test
  (:use [arkham.core])
  (:use [clojure.test]))

(deftest test-evil
  (testing "symbol resolution"
    (is (thrown? Exception (evil 'f))))
  (testing "function calling"
    (is (= 3 (evil '(+ 1 2)))))
  (testing "special forms"
    ;; . def loop* recur if let* letfn* do fn* quote var set! try
    ;; catch finally throw deftype* case* new reify*
    (is (= 1 (evil '(do 2 1))))
    (is (= 'a (evil '(quote a))))
    (is (= #'+ (evil '(var +))))
    (is (= 3 (evil '(let [x 1 y 2] (+ x y)))))))
