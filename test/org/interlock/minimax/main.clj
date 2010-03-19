(ns org.interlock.minimax.test.main
  (:use [org.interlock.minimax.main])
  (:use [clojure.test]))

(defmacro with-private-vars [[ns vars] & tests]
  "Refers private fns from ns and runs tests in context."
  `(let ~(reduce #(conj %1 %2 `@(ns-resolve '~ns '~%2)) [] vars)
     ~@tests))

(with-private-vars [org.interlock.minimax.main [int->letter letter->int to-int]]
  (deftest test-int->letter
    (is (= \A (int->letter 0)))
    (is (= \Z (int->letter 25))))
  (deftest test-letter->int
    (is (= 0 (letter->int \A)))
    (is (= 25 (letter->int \Z))))
  (deftest to-int
    (is (= nil (to-int (Object.))))
    (is (= 10 (to-int "10")))))

(deftest test-in-a-row?
  (is (= true (in-a-row? :Z 3 [:a 3 :Z :Z :Z 4]))))

(deftest test-opposite
  (is (= :x (opposite :o))))

(def testboard [:e :e :e
                :x :o :x
                :x :o :o]) ;TODO: improve this (maybe)

(def testboard-x-wins [:x :e :e
                       :x :o :x
		       :x :o :o])

(def testboard-o-wins [:e :o :e
		       :x :o :x
		       :x :o :o])

(deftest test-render-board
  (is (= "    A B C\n 1  . . .\n 2  X O X\n 3  X O O\n" (render-board testboard))))

(deftest test-winner?
  (is (= true (winner? testboard-x-wins :x)))
  (is (= true (winner? testboard-o-wins :o)))
  (is (nil? (winner? testboard :x))))