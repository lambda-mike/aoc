(ns day13.core-test
  (:require [clojure.test :refer :all]
            [day13.core :refer :all]))

(deftest solve-b-test
  (testing "solve-b"
    (testing "1068781"
      (is (= 1068781 (solve-b 1000 '([0 7] [1 13] [4 59] [6 31] [7 19])))))
    (testing "3417"
      (is (= 3417 (solve-b 1000 '([0 17] [2 13] [3 19])))))
    (testing "754018"
      (is (= 754018 (solve-b 1000 '([0 67] [1 7] [2 59] [3 61])))))
    (testing "779210"
      (is (= 779210 (solve-b 1000 '([0 67] [2 7] [3 59] [4 61])))))
    (testing "1261476"
      (is (= 1261476 (solve-b 1000 '([0 67] [1 7] [3 59] [4 61])))))
    (testing "1202161486"
      (is (= 1202161486 (solve-b 1000 '([0 1789] [1 37] [2 47] [3 1889])))))))
