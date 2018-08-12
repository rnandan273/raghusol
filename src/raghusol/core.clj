(ns raghusol.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

(defn do-similarity-score 
	"Do the similarity score"
	[stop-words token item]
	(->> (map #(clojure.string/includes? token %) 
		       (clojure.set/difference 
                 (into #{} (clojure.string/split (str item) #" "))
                 (into #{} stop-words))) 
    	 (filter true?)
    	 (count)))


(defn score-questions 
	"Scores the questions to match the text tokens"
	[stop-words token txt]
	{:question token
    	         :matching-txt 
    	         (->> (map (fn [x] 
		                {:txt x
		                 :score (do-similarity-score stop-words token x)}) txt))})

(defn filter-by-hints 
	"Filters hints by highest score"
	[stop-words txt hints]
	(let [matches  (map (fn [x] 
					         {:txt (:txt txt) 
					          :score (+ (:score txt) (do-similarity-score stop-words (:txt txt) x))}) 
	                hints)
	     max-score (apply max (map :score  (sort-by :score matches)))]
         (filter #(= max-score (:score %)) matches)))

(defn score-hints 
	"Scores the hints to match the text tokens"
	[stop-words token hints]
	{:question (:question token)
     :matching-txt (filter-by-hints stop-words (first (:matching-txt token)) hints)})


(defn filter-questions-by-max-score 
	"Filters questions by highest score"
	[items]
     (let [matches (:matching-txt items)
     	   max-score (apply max (map :score (sort-by :score matches)))]
     {:question (:question items) :matching-txt (filter #(= max-score (:score %)) matches)}))


(defn process-data 
	"Runs the algorithm, parses the file and instantiates the data structures for the pipeline processing"
	[stop-words]
	(let [data (-> (slurp (io/file "./data.txt"))
		           (clojure.string/split #"\n"))
	      tokens (into [] (reverse data))
		  txt (clojure.string/split (nth (into [] (reverse data)) 6) #"\. ")
		  questions (conj [] (nth tokens 1) (nth tokens 2) (nth tokens 3) (nth tokens 4) (nth tokens 5))
		  hints (clojure.string/split (nth tokens 0) #";")]
          (->> (->> (map #(score-questions stop-words % txt) questions) 
 	  		   		      (map filter-questions-by-max-score))
               (map #(score-hints stop-words % hints)))))


(defn -main
  "The main function"
  [& args]
  (println "Do you choose to process the data (Yes/No)\nIf Yes : Run the solution \nIf No : Exit")

  (case (read-line)
  	"Yes" (do (println "Please provide comma separated stop words to do the similarity scoring \n Here is a sample 
                       \"What\",\"Which\",\"Where\",\"and\",\"the\",\"of\", \"is\",\"to\", \"zebra\",\"Zebras\"")
  	      (let [stop-words (-> (read-line)
                              (clojure.string/split #","))]
  	      	(println "Stop chosen are => " stop-words)
  	      	
  	       (println (for [x (process-data stop-words)] 
              (str "\n Q: " (:question x) "\n A: " 
                  (str (:txt (first (:matching-txt x))) "\n\n"))))))
  	"No"  (println "Have a good day")
  	(println "Have a good day")))





