(ns datomic.codeq.examples.storm
  (:require [datomic.api :as d :refer [q]]
            [clojure.pprint :refer [pprint]]))

(def methods-by-class
  '[:find ?method-name
    :in $ ?class-name
    :where
    [?class-codename :code/name ?class-name]
    [?class-codeq :java/class ?class-codename]
    [?method-codeq :codeq/parent ?class-codeq]
    [?method-codeq :java/method ?method-codename]
    [?method-codename :code/name ?method-name]])

(def method-churn-by-class
  '[:find ?name (count ?sha)
    :in $ ?class-name
    :where
    [?class-codename :code/name ?class-name]
    [?codeq :java/class ?class-codename]
    [?mcodeq :codeq/parent ?codeq]
    [?mcodeq :java/method ?codename]
    [?codename :code/name ?name]
    [?mcodeq :codeq/code ?code]
    [?code :code/sha ?sha]])

(def method-churn
  '[:find ?name (count ?sha)
    :where
    [?mcodeq :java/method ?codename]
    [?codename :code/name ?name]
    [?mcodeq :codeq/code ?code]
    [?code :code/sha ?sha]])

(defn -main [& [database-uri]]
  (assert database-uri)
  (println "Running storm examples with database" database-uri)
  (println)
  (let [db (-> database-uri d/connect d/db)
        class-name "backtype.storm.utils.ShellProcess"]
    (println "## Past & present methods of" class-name)
    (pprint 
     (seq (q methods-by-class
             db
             class-name)))

    (println)
    (println "## Method churn for class" class-name)
    (->> (q method-churn-by-class db class-name)
         (sort-by second)
         reverse
         clojure.pprint/pprint)

    (println)
    (println "## Method churn across codebase")
    (->> (q method-churn db)
         (sort-by second)
         reverse
         (take 10)
         clojure.pprint/pprint)
    (System/exit 0)))

(comment 
  (-main "datomic:free://localhost:4334/storm")

)