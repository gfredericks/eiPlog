(ns #^{:doc "Basic http functions"}
  eip.eiPlog.http
  (:import [java.net URLEncoder])
  (:import [java.io File FileInputStream]) 
  (:require [clojure.contrib.http.connection :as c])
  (:require [clojure.contrib.str-utils2 :as s])
  (:require [clojure.contrib.http.agent :as ag]))

(. (System/getProperties) load (FileInputStream. (File. "properties")))

(def http-path
  (let [prop (System/getProperties)]
    (format "http://%s:%s"
      (.getProperty prop "eiPlog_host")
      (.getProperty prop "eiPlog_port"))))

(defn post-something
  [path body]
  (let [a
        (ag/http-agent
          (str
            http-path
            path)
          :body body
          :headers {"Content-Type" "application/json"
                    "Accept" "application/json"
                    "Connection" "close"}
          :method "POST")]
    (ag/string a)
    (ag/status a)))

(defn put-something
  [path body]
  (ag/http-agent
    (str
      http-path
      path)
    :body body
    :headers {"Content-Type" "text/plain"
              "Accept" "application/json"}
    :method "PUT"))

(defn get-something
  ([path params]
    (ag/string
      (ag/http-agent
        (str
          http-path
          path
          "?"
          (s/join "&" 
                  (map
                    (fn [[k v]]
                      (format "%s=%s"
                        k
                        (URLEncoder/encode v)))
                    params)))
        :headers
          {"Accept" "application/json"}
        )))
  ([path]
    (ag/string
      (ag/http-agent
        (str
          http-path
          path)
        :headers
          {"Accept" "application/json"}
        ))))

(defn do-it-yourself [context details]
  (let [conn (c/http-connection "http://log.eip.sc.gov:8000/logs/gary/remote")]
    (.setRequestMethod conn "POST")
    (doseq [[name value] {"Content-Type" "application/json" "Accept" "application/json"}]
      (.setRequestProperty conn name value))
    (c/start-http-connection conn (format "{\"context\": \"%s\", \"details\": \"%s\"}" context details))
    (let [rc (.getResponseCode conn)]
      (.close (.getContent conn))
      rc)))

