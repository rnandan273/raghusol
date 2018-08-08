(ns raghusol.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))



(def stop-words (list "What" "Which" "Where" "and" "the" "of" "is" "to" "zebra" "Zebras"))

(defn do-score [token item]
	(->> (map #(clojure.string/includes? token %) 
		       (clojure.set/difference 
                 (into #{} (clojure.string/split (str item) #" "))
                 (into #{} stop-words))) 
    	 (filter true?)
    	 (count)))


(defn score-questions [token txt]
	(let [])
	{:question token
    	         :matching-txt 
    	         (->> (map (fn [x] 
		                {:txt x
		                 :score (do-score token x)}) txt))})

(defn filter-by-hints [txt hints]
	(let [matches  (map (fn [x] 
					         {:txt (:txt txt) 
					          :score (+ (:score txt) (do-score (:txt txt) x))}) 
	                hints)
	     max-score (apply max (map :score  (sort-by :score matches)))]
         (filter #(= max-score (:score %)) matches)))

(defn score-hints [token hints]
	{:question (:question token)
     :matching-txt (filter-by-hints (first (:matching-txt token)) hints)})


(defn filter-questions-by-max-score [items]
     (let [matches (:matching-txt items)
     	   max-score (apply max (map :score (sort-by :score matches)))]
     {:question (:question items) :matching-txt (filter #(= max-score (:score %)) matches)}))


(defn investigate []
	(let [data (-> (slurp (io/file "./data.txt"))
		           (clojure.string/split #"\n"))
	      tokens (into [] (reverse data))
		  txt (clojure.string/split (nth (into [] (reverse data)) 0) #"\. ")
		  questions (conj [] (nth tokens 1) (nth tokens 2) (nth tokens 3) (nth tokens 4) (nth tokens 5))
		  hints (clojure.string/split (nth tokens 6) #";")]
          (->> (->> (map #(score-questions % txt) questions) 
 	  		   		(map filter-questions-by-max-score))
               (map #(score-hints % hints)))))


(defn -main
  "The main function"
  [& args]
  (println "Do you choose to investigate the input data (Yes/No)\nIf Yes : Run the solution \nIf No : Exit")
  (case (read-line)
  	"Yes" (println (investigate))
  	"No"  (println "Have a good day")
  	(println "Have a good day")))





