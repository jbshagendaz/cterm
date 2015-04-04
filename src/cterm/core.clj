(ns cterm.core
  (use [clojure.java.shell :only [sh]]))

(defn -main 
  []
   (println "Enter text:")
    
   (loop  [input  (read-line)]
     (when-not  (= ":done" input)
       (println  (str "You entered: >>" input "<<"))
       (recur  (read-line))))
    
   (println "End")
   )
