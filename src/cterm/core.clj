(ns cterm.core
  (use [clojure.java.shell :only [sh]]))
  (require '[lanterna.terminal :as terminal])

(defn -main 
  "" 
  []
  (def term (terminal/get-terminal :swing))
  (terminal/start term)
  (while (pos? 2) 
    (terminal/put-character term (terminal/get-key-blocking term))))
