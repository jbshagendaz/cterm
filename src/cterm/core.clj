(ns cterm.core
  (use [clojure.string :only [split]])
  (require [clj-commons-exec :as exec]))

(defn print-prompt
  []
  ;(printf (str (get @(exec/sh ["pwd"]) :out))) 
  (printf "$ ")
  (flush))

(defn -main 
  []
  (print-prompt)
  (doseq  
    [line (take-while (partial not= "exit") (repeatedly read-line))]
     (println  @(exec/sh (split line #" ")))
     (print-prompt)))
