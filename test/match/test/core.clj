(ns match.test.core
  (:use [match.core])
  (:use [clojure.test]))

(deftest test-mn
  (is (= 1 ((mn [?a] a) 1)))
  (is (= [:int 2] (secd ()
                        '{inc :succ}
                        '[[:term [:lit 1]]
                          [:term [:var inc]]
                          :apply]
                        ()))))
