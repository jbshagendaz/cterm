(ns cterm.core
  (use [clojure.string :only [replace-first 
                              split 
                              trim 
                              trim-newline]])
  (use [clj-commons-exec :as exec]))


;;Variables
(def last-dir (:out @(exec/sh ["pwd"])))
(def curr-dir (:out @(exec/sh ["pwd"])))
(def history [])

;;Misc Functions
(defn get-home []
  (trim-newline (:out @(exec/sh ["printenv" "HOME"]))))

(defn replace-home [dir]
  (trim-newline (replace-first dir (get-home) "~")))

(defn print-prompt []
  (printf "\n%s\t%s" (replace-home curr-dir) (:out @(exec/sh ["date" "+%X"]))) 
  (printf "$ ")
  (flush))

(defn print-history []
  (doseq [i (range 0 (count history))]
    (println i (get history i))))


;;User Input Functions
(defn process-input [input] 
  "Takes an input line and outputs a map of the analyzed text"
  (let [input-vector (split input #"\s+")
        pipe?        (boolean (re-find #"\|" input))
        cd?          (= "cd" (first input-vector))]
    {:input-vector input-vector
     
     ;cd
     :cd? cd?
     :rel?  (when cd? (not= \/ (get (second input-vector) 0)))
     :abs?  (when cd? (= \/ (get (second input-vector) 0)))
   
     ;history
     :history? (boolean (re-find #"history" input))
     
     ;pipes 
     :pipe? pipe?   
     :pipe-input-1 (when pipe? (split (trim (first  (split input #"\|"))) #"\s+"))
     :pipe-input-2 (when pipe? (split (trim (second (split input #"\|"))) #"\s+")) 
     
     ;new process 
     :process? ()}))

(defn execute-input [input-map]
  "Takes in a map of analyzed input and executes the command"
  (def last-dir curr-dir) 
  (when (input-map :rel?) (def curr-dir (str curr-dir "/" (second (input-map :input-vector)))))
  (when (input-map :abs?) (def curr-dir (second (input-map :input-vector))))
  
  (cond
    (input-map :cd?) (let [output-map @(exec/sh ["cd" curr-dir])]
                       (when (not (nil? (:err output-map))) (do (def curr-dir last-dir)))
                       output-map)

    (input-map :history?) (let [] 
                            (print-history)
                            {:exit 0})

    (input-map :pipe?) @(last (exec/sh-pipe (input-map :pipe-input-1) 
                                            (input-map :pipe-input-2) 
                                            {:dir curr-dir}))
    
    :else @(exec/sh (input-map :input-vector) {:dir curr-dir})))

(defn print-output [output-map]
  "Takes in a map of output and conditionally prints the results"
  (when (not (zero? (output-map :exit))) (printf "error code: %s\n" (output-map :exit)))
  (when (not (nil? (output-map :exc))) (println (output-map :exc)))
  (when (not (nil? (output-map :err))) (println (output-map :err)))
  (when (not (nil? (output-map :out))) (println (output-map :out))))


;;Main Function
(defn -main 
  []
  (print-prompt) 
  (doseq  
    [line (take-while (partial not= "quit") (repeatedly read-line))] 
    
    (->> (process-input line) 
         (execute-input)
         (print-output))
    
    (def history (conj history (vector line)))  
    (print-prompt)))
