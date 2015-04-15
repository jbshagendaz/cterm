(ns cterm.core
  (use [clojure.string :only [replace-first 
                              split 
                              trim 
                              trim-newline]])
  (require [clj-commons-exec :as exec]))

(def last-dir (:out @(exec/sh ["pwd"])))
(def curr-dir (:out @(exec/sh ["pwd"])))

(defn get-home []
  (trim-newline (:out @(exec/sh ["printenv" "HOME"]))))

(defn replace-home [dir]
  (trim-newline (replace-first dir (get-home) "~")))

(defn print-prompt []
  (printf "\n%s\t%s" (replace-home curr-dir) (:out @(exec/sh ["date" "+%X"]))) 
  (printf "$ ")
  (flush))

(defn process-input [input] 
  (let [input-vector (split input #"\s+")
        pipe?        (boolean (re-find #"\|" input))
        cd?          (= "cd" (first input-vector))]
    {:input-vector input-vector
     
     ;cd
     :cd? cd?
     :rel?  (when cd? (not= \/ (get (second input-vector) 0)))
     :abs?  (when cd? (= \/ (get (second input-vector) 0)))
    
     ;pipes 
     :pipe? pipe?   
     :pipe-input-1 (when pipe? (split (trim (first  (split input #"\|"))) #"\s+"))
     :pipe-input-2 (when pipe? (split (trim (second (split input #"\|"))) #"\s+")) 
     
     ;new process 
     :process? ()}))

(defn execute-input [input]
  (def last-dir curr-dir) 
  (when (input :rel?) (def curr-dir (str curr-dir "/" (second (input :input-vector)))))
  (when (input :abs?) (def curr-dir (second (input :input-vector))))
  
  (cond
    (input :cd?) (let [output-map @(exec/sh ["cd" curr-dir])]
                   (when (not (nil? (:err output-map))) 
                     (do (def curr-dir last-dir)))
                   output-map)

    (input :pipe?) @(last (exec/sh-pipe (input :pipe-input-1) (input :pipe-input-2) {:dir curr-dir}))
    :else @(exec/sh (input :input-vector) {:dir curr-dir})))

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
  (print-prompt) 
  (doseq  
    [line (take-while (partial not= "quit") (repeatedly read-line))]
     (cond
       (boolean (re-find #"history" line)) (print-history history)
       :else 
        (->> (process-input line) 
             (execute-input)
             (print-output)))

     (def history (conj history (vector line)))  
     (print-prompt)))
