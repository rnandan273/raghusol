(ns raghusol.core-test
  (:require [clojure.test :refer :all]
            [raghusol.core :refer :all]))
(def stop-words (list "What" "Which" "Where" "and" "the" "of" "is" "to" "zebra" "Zebras"))
(def data-file "./data.txt")

(deftest process-data-test
  (testing "FIXME, I fail."
    (println "Sample stop words -> " (str stop-words))
    (println "Sample data file -> " data-file)
  	(println (for [x (process-data stop-words data-file)] 
              (str "\n Q: " (:question x) "\n A: " 
                  (str (:txt (first (:matching-txt x))) "\n\n")))))
  true)


