(ns raghusol.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

(def stop-words (list "What" "Which" "Where" "and" "the" "of" "is" "to" "zebra" "Zebras"))

(defn do-similarity-score 
	"Do the similarity score"
	[token item]
	(->> (map #(clojure.string/includes? token %) 
		       (clojure.set/difference 
                 (into #{} (clojure.string/split (str item) #" "))
                 (into #{} stop-words))) 
    	 (filter true?)
    	 (count)))


(defn score-questions 
	"Scores the questions to match the text tokens"
	[token txt]
	{:question token
    	         :matching-txt 
    	         (->> (map (fn [x] 
		                {:txt x
		                 :score (do-similarity-score token x)}) txt))})

(defn filter-by-hints 
	"Filters hints by highest score"
	[txt hints]
	(let [matches  (map (fn [x] 
					         {:txt (:txt txt) 
					          :score (+ (:score txt) (do-similarity-score (:txt txt) x))}) 
	                hints)
	     max-score (apply max (map :score  (sort-by :score matches)))]
         (filter #(= max-score (:score %)) matches)))

(defn score-hints 
	"Scores the hints to match the text tokens"
	[token hints]
	{:question (:question token)
     :matching-txt (filter-by-hints (first (:matching-txt token)) hints)})


(defn filter-questions-by-max-score 
	"Filters questions by highest score"
	[items]
     (let [matches (:matching-txt items)
     	   max-score (apply max (map :score (sort-by :score matches)))]
     {:question (:question items) :matching-txt (filter #(= max-score (:score %)) matches)}))


(defn process-data 
	"Runs the algorithm, parses the file and instantiates the data structures for the pipeline processing"
	[]
	(let [data (-> (slurp (io/file "./data.txt"))
		           (clojure.string/split #"\n"))
	      tokens (into [] (reverse data))
		  txt (clojure.string/split (nth (into [] (reverse data)) 6) #"\. ")
		  questions (conj [] (nth tokens 1) (nth tokens 2) (nth tokens 3) (nth tokens 4) (nth tokens 5))
		  hints (clojure.string/split (nth tokens 0) #";")]
          (->> (->> (map #(score-questions % txt) questions) 
 	  		   		(map filter-questions-by-max-score))
               (map #(score-hints % hints)))))


(defn -main
  "The main function"
  [& args]
  (println "Do you choose to process the data (Yes/No)\nIf Yes : Run the solution \nIf No : Exit")
  (case (read-line)
  	"Yes" (println (for [x (process-data )] 
              (str "\n Q: " (:question x) "\n A: " 
                  (str (:txt (first (:matching-txt x))) "\n\n"))))
  	"No"  (println "Have a good day")
  	(println "Have a good day")))





