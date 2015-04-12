(ns cterm.core
  (use [clojure.string :only [replace-first 
                              split 
                              trim 
                              trim-newline]])
  (require [clj-commons-exec :as exec]))


(defn get-home []
  (trim-newline (:out @(exec/sh ["printenv" "HOME"]))))

(defn replace-home [directory]
  (trim-newline (replace-first directory (get-home) "~")))

(defn print-prompt [directory]
  (printf "\n%s\t%s" (replace-home directory) (:out @(exec/sh ["date" "+%X"]))) 
  (printf "$ ")
  (flush))

(defn process-input [input] 
  (let [input-vector (split input #"\s+")
        pipe?       (boolean (re-find #"\|" input))]

    {:input-vector input-vector
     
     ;cd
     :dir (when (= "cd" (first input-vector)) (second input-vector))
    
     ;pipes 
     :pipe? pipe?   
     :pipe-input-1 (when pipe? (split (trim (first  (split input #"\|"))) #"\s+"))
     :pipe-input-2 (when pipe? (split (trim (second (split input #"\|"))) #"\s+")) 
     
     ;history

     ;new process 
     :process? ()}))

(defn execute-input [input] 
  (cond
    (input :pipe?) @(last (exec/sh-pipe (input :pipe-input-1) (input :pipe-input-2)))
    :else @(exec/sh (input :input-vector))))

(defn print-output [output-map]
  (when (not (zero? (output-map :exit))) (printf "error code: %s\n" (output-map :exit)))
  (when (not (nil? (output-map :exc))) (println (output-map :exc)))
  (when (not (nil? (output-map :err))) (println (output-map :err)))
  (when (not (nil? (output-map :out))) (println (output-map :out))))

(defn print-history [history]
  (doseq [i (range 0 (count history))]
    (println i (get history i))))

(defn -main 
  []
  (def history [])
  (print-prompt (get @(exec/sh ["pwd"]) :out)) 
  (doseq  
    [line (take-while (partial not= "quit") (repeatedly read-line))]
     (cond
       (boolean (re-find #"history" line)) (print-history history)
       :else 
        (->> (process-input line) 
             (execute-input)
             (print-output)))

     ;(println (process-input line))
     (def history (conj history (vector line)))  
     (print-prompt (get @(exec/sh ["pwd"]) :out))))
