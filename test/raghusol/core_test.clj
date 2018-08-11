(ns raghusol.core-test
  (:require [clojure.test :refer :all]
            [raghusol.core :refer :all]))

(deftest process-data-test
  (testing "FIXME, I fail."
  	(println (for [x (process-data )] 
              (str "\n Q: " (:question x) "\n A: " 
                  (str (:txt (first (:matching-txt x))) "\n\n")))))
  true)


