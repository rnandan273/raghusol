(ns raghusol.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

(def input-json-test (json/read-str (slurp (io/file "./sample-input.json.txt")) :key-fn keyword))

(def agents (ref #{}))

(def busy-agents (ref #{}))

(def job-list (ref #{}))

(def job-req-list (ref #{}))

(def output-log (ref #{}))

(defn job-processor 
	"Registers each new job in the working job list"
	[]
	(let [input-chan (chan)
		  job-map (atom {})]
		(go (while true 
			    (let [job-spec (<! input-chan)]
			    	 (println  "Registering Job" job-spec)
			    	 (dosync (alter job-list conj job-spec)))))
		input-chan))

(defn job-request-processor
   "Registers each new job request in the working job request list"
    []
	(let [input-chan (chan)
		  job-map (atom {})]
		(go (while true 
			    (let [job-req-spec (<! input-chan)]
			    	 (println  "Registering Job Request" job-req-spec)
			    	 (dosync (alter job-req-list conj job-req-spec)))))
		input-chan))

(defn agent-processor 
	"Registers each new agent in the agent list"
	[]
	(let [input-chan (chan)
		  job-map (atom {})]
	(go (while true 
		    (let [agent-spec (<! input-chan)
		    	  available-jobs @job-list]
		    	(println  "Registering Agents" agent-spec)
		    	(dosync (alter agents conj agent-spec)))))
	input-chan))

(defn job-allocator 
	"Allocates each job by matching them against the rules first check primary skills, if not available look at secondary skills"
	[]
	(let []
		(go (while true 
			(Thread/sleep (rand 15000))
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
					        ;(println "Agent Allocated for Job : "  job-type " is " selected-agent)
						    (dosync 
						    		(alter busy-agents conj job-spec-mod)
						    		(alter job-list disj job)))
				    	 (do (println "Waiting for next available agent to process " job-type)))
					(recur (rest jobs))))))
			 ))))

(defn job-request-allocator
   "The last job allocated is done, hence frees up the agent and signals job done"
    [output-chan]
	(let []
		(go (while true 
			(Thread/sleep (rand 15000))

			(loop [jobs @job-req-list]
		    (let [size (count jobs)]
		      (when (> size 0)
		        (let [job (first jobs)
		        	  selected-agent-spec (first (dosync (filter #(= (:agent_id job) (:agent-id %)) @busy-agents)))]
                   ;(println "In Job request allocator " job "-" selected-agent-spec)

                   (if (not= nil selected-agent-spec)
				    	(do
					        ;(println "Agent handling : "  job " is " selected-agent-spec)
						    (>! output-chan {:job-assigned {:job_id (:id selected-agent-spec) :agent_id (:agent-id selected-agent-spec)}})
					    	(dosync 
					    		(alter busy-agents disj selected-agent-spec)
					    		(alter output-log conj {:job-assigned {:job_id (:id selected-agent-spec) :agent_id (:agent-id selected-agent-spec)}})
					    		)
						    ))
				   
					(recur (rest jobs))))))))))

;; Channel handlers

(def agentpr (agent-processor))

(def jobpr (job-processor))

(def jobreqpr (job-request-processor))

(def outpr (chan))

(def job-allocator (job-allocator))

(def job-request-allocator (job-request-allocator outpr))

;; Main function
(defn process-agents 
	"Reads the input and pushes to the agent channel"
	[feed-json]
	(loop [items feed-json]
	    (let [size (count items)]
	      (when (> size 0)
	        (let [item (first items)]
			    (go 
			    	(>! agentpr item)))
				(recur (rest items))))))

(defn process-jobs 
	"Reads the input and pushes to the jobs channel"
	[feed-json]
	(loop [items feed-json]
	    (let [size (count items)]
	      (when (> size 0)
	        (let [item (first items)]
	        	  (go 
	  	           	 (>! jobpr item))
				  (recur (rest items)))))))


(defn process-job-requests 
	"Reads the input and pushes to the job requests channel"
	[feed-json]
	(loop [items feed-json]
	    (let [size (count items)]
	      (when (> size 0)
	        (let [item (first items)]
	            (go 
	  	            (>! jobreqpr item)))
				(recur (rest items))))))

(defn -main
  "The main function, which reads the input json string containing the agents, jobs and job requests.
  Registers them to the system and the system will pick them up"
  [& args]
  (println "Do you choose to provide the feed data (Yes/No)\nIf Yes : Provide json data \nIf No : Allow the system to process sample json")
  (try
  	(def input-json (case (read-line)
  		              "Yes" (do (println "Provide a json data")
  		              	        (read-line))
  		              "No" (json/read-str (slurp (io/file "./sample-input.json.txt")) :key-fn keyword)
  	                  "Please answer Yes or No"))
    (do
     	(println (str "Processing data \n" input-json))
        (let [agents-list (map :new_agent (filter :new_agent input-json))
        	  ;sort by urgency level
        	  jobs-list (sort-by #(= false (:urgent %)) (map :new_job (filter :new_job input-json)))
        	  jobs-request-list (map :job_request (filter :job_request input-json))]
        	(process-agents agents-list)
        	(process-jobs jobs-list)
        	(process-job-requests jobs-request-list)))
       
        (while true 
        	  (do 
        	  	(Thread/sleep (rand 10000))
        	    (println (str "<====== CONSOLE OUTPUT ======>\n" (<!! outpr)))))
     
     (catch Exception e (str "caught exception: " (.getMessage e) "\nPlease provide a valid JSON"))
     (finally)))


