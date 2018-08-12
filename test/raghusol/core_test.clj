(ns raghusol.core-test
  (:require [clojure.test :refer :all]
            [raghusol.core :refer :all]))
(def stop-words (list "What" "Which" "Where" "and" "the" "of" "is" "to" "zebra" "Zebras"))

(deftest process-data-test
  (testing "FIXME, I fail."
    (println "Choosing sample stop words -> " (str stop-words))
  	(println (for [x (process-data stop-words)] 
              (str "\n Q: " (:question x) "\n A: " 
                  (str (:txt (first (:matching-txt x))) "\n\n")))))
  true)


