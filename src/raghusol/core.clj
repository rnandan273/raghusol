(ns raghusol.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

(def input-json (json/read-str (slurp (io/file "./sample-input.json.txt")) :key-fn keyword))

(def agents (ref #{}))

(def busy-agents (ref #{}))

(def job-list (ref #{}))

(def output-log (ref #{}))

(defn job-processor []
	(let [input-chan (chan)
		  job-map (atom {})]
		(go (while true 
			    (let [job-spec (<! input-chan)]
			    	 (println  "Registering Job" job-spec)
			    	 (dosync (alter job-list conj job-spec)))))
		input-chan))

(defn agent-processor []
	(let [input-chan (chan)
		  job-map (atom {})]
	(go (while true 
		    (let [agent-spec (<! input-chan)
		    	  available-jobs @job-list]
		    	(println  "Registering Agents" agent-spec)
		    	(dosync (alter agents conj agent-spec)))))
	input-chan))

(defn job-allocator []
	(let []
		(go (while true 
			(Thread/sleep (rand 10000))
			;(println (str "Job Allocator ***************" @agents))

			(loop [jobs @job-list]
		    (let [size (count jobs)]
		      (when (> size 0)
		        (let [job (first jobs)
		        	  job-type (:type job)
		        	  available-agents (remove (fn [y] (some (fn [x] (= (:agent-id x) (:id y))) @busy-agents)) @agents)

		        	  selected-agent-primary (first (filter (fn [y] 
				    	  	                                   (some (fn [x] (= x job-type)) (:primary_skillset y))) available-agents))
		        	  selected-agent (if (= nil selected-agent-primary)
				    	  	              (first 
				    	  	              	(filter (fn [y] 
				    	  	                           (some (fn [x] (= x job-type)) (:secondary_skillset y))) available-agents))
				    	  	              selected-agent-primary)
		        	  job-spec-mod (conj job {:agent-id (:id selected-agent)})]

				    (if (not= nil selected-agent)
				    	(do
					        (println "Checking agent for Job : " job-type)
					        (println "Agent Allocated : " selected-agent)
						    (dosync 
						    		(alter busy-agents conj job-spec-mod)
						    		(alter job-list disj job)))
				    	 (do (println "No free agent available for " job-type)))
					(recur (rest jobs))))))

			 ;(println (str "Job Allocator ###############"))
			 ))))


(defn job-request-processor [output-chan]
	(let [input-chan (chan)
		  job-map (atom {})]

		(go (while true 
			    (let [job-req-spec (<! input-chan)
			    	  selected-agent-spec (first (dosync (filter #(= (:agent_id job-req-spec) (:agent-id %)) @busy-agents)))]
			    	(>! output-chan {:job-assigned {:id (:id selected-agent-spec) :agent_id (:agent-id selected-agent-spec)}})
			    	;(println "OUTPUT ==> \n" {:job-assigned {:id (:id selected-agent-spec) :agent_id (:agent-id selected-agent-spec)}})
			    	(dosync 
			    		(alter busy-agents disj selected-agent-spec)
			    		(alter output-log conj {:job-assigned {:id (:id selected-agent-spec) :agent_id (:agent-id selected-agent-spec)}})
			    		))))
		input-chan))

(def agentpr (agent-processor))

(def jobpr (job-processor))

(def outpr (chan))

(def jobreqpr (job-request-processor outpr))

(def job-allocator (job-allocator))


(defn process-agents [feed-json]
	(loop [items feed-json]
	    (let [size (count items)]
	      (when (> size 0)
	        (let [item (first items)]
			    (go 
			    	(>! agentpr item)))
				(recur (rest items))))))

(defn process-jobs [feed-json]
	(loop [items feed-json]
	    (let [size (count items)]
	      (when (> size 0)
	        (let [item (first items)]
	        	  (go 
	  	           	 (>! jobpr item))
				  (recur (rest items)))))))


(defn process-job-requests [feed-json]
	(loop [items feed-json]
	    (let [size (count items)]
	      (when (> size 0)
	        (let [item (first items)]
	            (go 
	  				(Thread/sleep (rand 50000))
	  				(println "Adding Job completion -> " item)
	  	            (>! jobreqpr item)))
				(recur (rest items))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Please type the json feed data")
  (try
    ; (def json-input (json/read-str (read-line) :key-fn keyword))
    (do
     	(def input-json (json/read-str (slurp (io/file "./sample-input.json.txt")) :key-fn keyword))
     	;(println (str "Your input is " input-json))
        (let [agents-list (map :new_agent (filter :new_agent input-json))
        	  jobs-list (sort-by #(= false (:urgent %)) (map :new_job (filter :new_job input-json)))
        	  jobs-request-list (map :job_request (filter :job_request input-json))]
        	(process-agents agents-list)
        	(process-jobs jobs-list)
        	(process-job-requests jobs-request-list)))
       
        (while true 
        	  (do 
        	  	(Thread/sleep (rand 100000))
        	    (println (str "OUTPUT PRINT" (<!! outpr)))))
     
     (catch Exception e (str "caught exception: " (.getMessage e) "\nPlease provide a valid JSON"))
     (finally
     	(do
     		(Thread/sleep (rand 15000))
     		(println (str "Your output is " @output-log))))))


