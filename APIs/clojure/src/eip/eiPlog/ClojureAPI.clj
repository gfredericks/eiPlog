(ns eip.eiPlog.ClojureAPI
  (:use clojure.contrib.json.read) 
  (:use clojure.contrib.json.write)
  (:use eip.eiPlog.http)
  (:gen-class
    :methods [#^{:static true} [applications [] java.util.List]
              #^{:static true} [events [String] java.util.List]
              #^{:static true} [log [String String String String] void]]))

(defn -applications []
  (read-json
    (get-something "/applications")))

(defn -events [app-name]
  (read-json
    (get-something (format "/events/%s" app-name))))

(defn -log [app-name event-name context details]
  (post-something
    (format "/logs/%s/%s" app-name event-name)
    (json-str
      {:context context
       :details details})))
