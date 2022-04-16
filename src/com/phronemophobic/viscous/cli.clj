(ns com.phronemophobic.viscous.cli
  (:require [com.phronemophobic.viscous :as viscous]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import java.io.PushbackReader
           java.io.StringReader))

(defn read-edn [rdr]
  (with-open [rdr (java.io.PushbackReader. rdr)]
    (edn/read {:default tagged-literal} rdr)))


(defn read-json [rdr]
  (with-open [rdr rdr]
    ((requiring-resolve 'clojure.data.json/read) rdr)))

(def help-text
  "Open a viscous data inspector.

Usage:

Read edn from stdin:
cat data.edn | clojure -X:viscous :file -

Read edn from filename:
clojure -X:viscous :file data.edn

Read json from stdin:
cat data.json | clojure -X:viscous :json-file -

Read json from filename:
clojure -X:viscous :json-file data.edn
")

(defn main [{:keys [file edn json-file json] :as opts}]
  (let [obj
        (cond
          (= (str file) "-") (read-edn (io/reader *in*)) 
          file (read-edn (io/reader (str file)))
          edn (read-edn (StringReader. (str edn)))

          (= (str json-file) "-") (read-json (io/reader *in*)) 
          json-file (read-json (io/reader (str json-file)))
          json (read-json (StringReader. (str json)))
          
          :else ::print-help)]
    (if (= obj ::print-help)
      (println help-text)
      (viscous/inspect obj
                       (merge
                        (select-keys opts [:width :height :show-context?])
                        {:sync? true})))))
