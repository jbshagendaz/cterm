(ns cterm.core
  (use [clojure.string :only [join
                              replace-first 
                              split 
                              trim 
                              trim-newline]])
  (use [clj-commons-exec :as exec]))


;;Variables
(def last-dir (trim-newline (:out @(exec/sh ["pwd"]))))
(def curr-dir (trim-newline (:out @(exec/sh ["pwd"]))))
(def history [])

;;Misc Functions
(defn count-substring [string sub]
  "counts occurrences of sub in string"
  (count (re-seq (re-pattern sub) string)))

(defn get-home []
  "returns $HOME"
  (trim-newline (:out @(exec/sh ["printenv" "HOME"]))))

(defn replace-home [dir]
  "changes $HOME to ~ to mimic normal shell
   example: /home/luckystrike ---> ~"
  (trim-newline (replace-first dir (get-home) "~")))

(defn print-prompt []
  "prints the standard user prompt"
  (printf "\n%s\t%s" (replace-home (:out @(exec/sh ["pwd"] {:dir curr-dir}))) (:out @(exec/sh ["date" "+%X"]))) 
  (printf "$ ")
  (flush))

(defn print-history []
  "function to print history implemented so each cmd printed on newline"
  (doseq [i (range 0 (count history))]
    (println i (get history i))))

(defn go-back [x]
  "on 'cd ..' or variation will return altered path"
  (cond
    (pos? x) (join "/" (take (+ x 1) (split curr-dir #"/")))
    :else "/"))

(defn change-dir [dir]
  "changes curr-dir variable"
  (cond
    (nil? dir) (alter-var-root #'curr-dir (constantly (get-home)))
    :else (alter-var-root #'curr-dir (constantly dir))))

(defn change-last-dir [dir]
  "changes last-dir variable"
  (cond
    (nil? dir) (alter-var-root #'last-dir (constantly (get-home)))
    :else (alter-var-root #'last-dir (constantly dir))))


;;User Input Functions
(defn process-input [input] 
  "Takes an input line and outputs a map of the analyzed text"
  (let [input-vector (split input #"\s+")
        pipe?        (boolean (re-find #"\|" input))
        cd?          (= "cd" (first input-vector))
        second-nil?  (nil? (second input-vector))]

    {:input-vector input-vector
     
     ;cd
     :cd? cd?
     :second-nil? second-nil?
     :rel?  (when (and cd? (not second-nil?)) (not= \/ (get (second input-vector) 0)))
     :abs?  (when (and cd? (not second-nil?)) (= \/ (get (second input-vector) 0)))
     :back? (when (and cd? (not second-nil?)) (boolean (re-find #"\.." (second input-vector))))
  
     ;history
     :history? (boolean (re-find #"history" input))
     
     ;pipes 
     :pipe? pipe?   
     :pipe-input-1 (when pipe? (split (trim (first  (split input #"\|"))) #"\s+"))
     :pipe-input-2 (when pipe? (split (trim (second (split input #"\|"))) #"\s+"))}))

(defn execute-input [input-map]
  "Takes in a map of analyzed input and executes the command"
  (change-last-dir curr-dir) 
 
  (when (input-map :cd?)  
    (cond
      (input-map :second-nil?) (change-dir nil)
      (input-map :back?) (change-dir (go-back (- (count-substring curr-dir "/") (count-substring (second (input-map :input-vector)) "..")))) 
      (input-map :abs?) (change-dir (second (input-map :input-vector)))
      :else (change-dir (str curr-dir "/" (second (input-map :input-vector))))))
  
  (cond
    (input-map :cd?) (let [output-map @(exec/sh ["cd" curr-dir])]
                       (when (not (nil? (:err output-map))) (do (change-dir last-dir)))
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
    (when (< 0 (count line))
      (do
        (->> (process-input line) 
             (execute-input)
             (print-output))
        (alter-var-root #'history (constantly (conj history (vector line)))))) 
    (print-prompt)))
