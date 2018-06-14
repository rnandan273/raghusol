(ns raghusol.core-test
  (:require [clojure.test :refer :all]
            [raghusol.core :refer :all]))

(def agents-list (map :new_agent (filter :new_agent input-json-test)))
(def jobs-list (sort-by #(= false (:urgent %)) (map :new_job (filter :new_job input-json-test))))
(def jobs-request-list (map :job_request (filter :job_request input-json-test)))

(deftest agent-registration-test
  (testing "FIXME, I fail."
  	(process-agents agents-list))
  true)

(deftest agent-registration-count-test
  (testing "FIXME, I fail."
    (> (count @agents) 0))
  true)

(deftest job-registration-test
  (testing "FIXME, I fail."
     (process-jobs jobs-list))
      true)

(deftest agent-registration-jobs-test
  (testing "FIXME, I fail."
    (> (count @job-list) 0))
  true)

(deftest job-requests-test
  (testing "FIXME, I fail."
     (process-job-requests jobs-request-list))
      true)



