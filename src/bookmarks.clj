(ns bookmarks
  (:require
   [babashka.fs :as fs]
   [babashka.http-client :as http]
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [medley.core :as xtra]
   [taoensso.timbre :as log]))

(defn extract-bookmarks
  [bookmarks]
  (loop [to-process [bookmarks] result []]
    (let [[{:keys [children]
            :as bookmark} & remaining] to-process
          next-to-process (concat children remaining)
          updated-result (if-not (or (seq children) (nil? (:uri bookmark)))
                           (conj result bookmark)
                           result)]
      (log/trace :extract-bookmarks :remaining (count remaining))
      (if (seq next-to-process)
        (recur next-to-process updated-result)
        updated-result))))

(def TIMEOUT 10000)

(defn still-reachable?
  [url]
  (log/debug :still-reachable? :url url)
  (let [client (http/client (assoc http/default-client-opts :connect-timeout TIMEOUT))]
    (try (some? (http/head url {:client client}))
         (catch Exception e false))))

(def ^:dynamic *still-reachable?* still-reachable?)

(defn check-reachability
  [bookmarks]
  (->> bookmarks
       (xtra/distinct-by :uri)
       (mapv #(future (assoc % :still-reachable (*still-reachable?* (:uri %)))))
       (mapv deref)))

(defn retrieve-api-key
  []
  (:OPENAI_API_KEY (clojure.edn/read-string (slurp (str (fs/path (fs/home) ".keys" "sgpt.edn"))))))

(def SYSTEM_PROMPT "You will be provided with a json array of objects. Each object contains a \"title\" attribute and \"uri\" attribute. You task is to classify each \"uri\" using the \"title\" to have more context. You will return a json array of objects where you will have added to each object an attribute \"group\" with the result of the classification for that object.")

(def RESPONSE_FORMAT {:type "json_schema"
                      :json_schema {:name "classification_schema"
                                    :schema {:type "object"
                                             :properties {:result
                                                          {:type "array"
                                                           :items {:type "object"
                                                                   :properties {:uri {:type "string"}
                                                                                :title {:type "string"}
                                                                                :group {:type "string"}}
                                                                   :additionalProperties false}}}}}})

(defn ask-gpt-to-classify
  [extracted-json]
  (let [openai-api-key (retrieve-api-key)
        body {:model "gpt-4o"
              :response_format RESPONSE_FORMAT
              :messages [{:role "system"
                          :content [{:type "text"
                                     :text SYSTEM_PROMPT}]}
                         {:role "user"
                          :content [{:type "text"
                                     :text extracted-json}]}]}
        resp (http/post "https://api.openai.com/v1/chat/completions"
                        {:headers {:content-type "application/json"
                                   :authorization (format "Bearer %s" openai-api-key)}
                         :body (json/encode body)
                         :throw false})]
    (->> (json/parse-string (:body resp) true)
         :choices
         (mapv (comp :content :message)))))

(def ^:dynamic *ask-gpt-to-classify* ask-gpt-to-classify)

(defn classify-urls
  [urls]
  (log/debug :classify-urls :count (count urls))
  (let [to-classify (json/encode urls)]
    (-> (*ask-gpt-to-classify* to-classify)
        first
        (json/decode keyword)
        :result)))

(def CHUNK 5)

(defn classify-bookmarks
  [bookmarks]
  (->> bookmarks
       (map #(select-keys % [:uri :title]))
       (partition CHUNK)
       (mapv classify-urls)))

(def EMPTY-BOOKMARKS
  {:guid "root________"
   :title ""
   :index 0
   :dateAdded 1651102848726000
   :lastModified 1728388462499000
   :id 1
   :typeCode 2
   :type "text/x-moz-place-container"
   :root "placesRoot"
   :children [{:guid "menu________"
               :title "menu"
               :index 0
               :dateAdded 1651102848726000
               :lastModified 1682955611419000
               :id 2
               :typeCode 2
               :type "text/x-moz-place-container"
               :root "bookmarksMenuFolder"}]})

(defn render-bookmarks
  [classified-bookmarks]
  (let [results-bookmarks (reduce-kv (fn create-group-node
                                       [result group bookmarks]
                                       (let [group-node {:guid (.toString (java.util.UUID/randomUUID))
                                                         :title group
                                                         :typeCode 2
                                                         :type "text/x-moz-place-container"}
                                             children (mapv (fn create-bookmark-node
                                                              [bookmark]
                                                              (dissoc bookmark :group :index)) bookmarks)]
                                         (conj result (assoc group-node :children children))))
                                     [] classified-bookmarks)]
    (assoc-in EMPTY-BOOKMARKS [:children 0 :children] results-bookmarks)))

(comment

  (def memoized-still-reachable? (memoize still-reachable?))
  (def memoized-ask-gpt-to-classify (memoize ask-gpt-to-classify))

  (def read-bookmarks (json/decode (slurp "bookmarks-2024-10-13.json") keyword))
  (def extracted-bookmarks (->> (extract-bookmarks read-bookmarks)
                                (xtra/distinct-by :uri)))
  (count extracted-bookmarks)

  (def checked-bookmarks
    (binding [*ask-gpt-to-classify* memoized-ask-gpt-to-classify
              *still-reachable?* memoized-still-reachable?]
      (->> extracted-bookmarks
              ;(take 10)
           (mapv #(future (assoc % :still-reachable (*still-reachable?* (:uri %)))))
           (mapv deref))))

  (->> checked-bookmarks
       (filter :still-reachable)
       count)

  (->> checked-bookmarks
       (filter (complement :still-reachable))
       count)

  (binding [*ask-gpt-to-classify* memoized-ask-gpt-to-classify
            *still-reachable?* memoized-still-reachable?]
    (let [bookmarks (json/decode (slurp (io/resource "bookmarks-2024-10-13.json")) keyword)
          extracted (extract-bookmarks bookmarks)
          checked (check-reachability extracted)
          {still-reachables true
           unreachables false
           :as splitted-by-reachability} (group-by :still-reachable checked)
          classified-reachables (classify-bookmarks still-reachables)
          classified-unreachables (classify-bookmarks unreachables)
          classified-by-uri (into {} (for [classification (->> (concat classified-reachables classified-unreachables)
                                                               (apply concat))]
                                       [(:uri classification) classification]))]
      (->> checked
           (map #(assoc % :group (:group (get classified-by-uri (:uri %) {:group "Unknown"}))))
           (group-by :group)
           (spit (io/resource "intermediate_result.edn")))))

  (let [classified-results (clojure.edn/read-string (slurp (io/resource "intermediate_result.edn")))]
    (first classified-results))
  ; ["Moving Services" [{:group "Moving Services", :index 18, :typeCode 1, :type "text/x-moz-place", :still-reachable false, :title "Déménagement :: Lettre déménagement :: Lettre de déménagement :: Préavis demenagement | Officiel du déménagement", :lastModified 1654827084000000, :id 14200, :dateAdded 1428795674000000, :uri "http://www.officiel-demenagement.com/aide/demenagement-formalite+lettre+changement+adresse.php", :guid "sTkkqUhgIvSg"}]]

  (let [classified-results (clojure.edn/read-string (slurp (io/resource "intermediate_result.edn"))) ]
    (->> (render-bookmarks classified-results)
         (json/encode)
         #_(spit "resources/final_result.json")))

  (comment))
