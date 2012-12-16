(ns datomic.codeq.analyzers.java
  (:require [datomic.api :as d]
            [datomic.codeq.analyzer :as az]
            [datomic.codeq.util :as util]
            [datomic.codeq.analyzers.java.parser :refer [parse-tree]]))

(defmacro ? [expr]
  `(let [e# ~expr]
     (clojure.pprint/pprint e#)
     e#))

(defmulti tx-data (fn [db fid ast ctx] (:node ast)))

(defmethod tx-data :compilation-unit [db fid ast ctx]
  (let [package (:package ast)
        types (:types ast)]
    (mapcat #(tx-data db fid % (assoc ctx :package package)) types)))

(defmethod tx-data :type-declaration
  [db fid
   {:keys [sha name loc src interface?] :as ast}
   {:keys [sha->id codename->id package] :as ctx}]
  (let [codename (format "%s.%s" package name)
        codeid (sha->id sha)

        codetx (if (util/tempid? codeid)
                 {:db/id codeid
                  :code/sha sha
                  :code/text src})

        codeqid (or (ffirst (d/q '[:find ?e :in $ ?f ?loc
                                   :where
                                   [?e :codeq/file ?f] 
                                   [?e :codeq/loc ?loc]]
                                 db fid loc))
                    (d/tempid :db.part/user))

        codeqtx (if (util/tempid? codeqid)
                  {:db/id codeqid
                   :codeq/file fid
                   :codeq/loc (apply pr-str loc)
                   :codeq/code codeid}
                  {:db/id codeqid})

        nameid (codename->id codename)

        nametx (if (util/tempid? nameid)
                 {:db/id nameid
                  :code/name codename})

        codeqtx (assoc codeqtx
                  (if interface? :java/interface :java/class)
                  nameid)]
    
    (remove nil? [codetx codeqtx nametx])))

(defmethod tx-data :enum-declaration [db fid ast ctx]
  (let [codename (format "ED %s.%s" (:package ctx) (:name ast))]
    (println codename)))

(defmethod tx-data :annotation-type-declaration [db fid ast ctx]
  (let [codename (format "ATD %s.%s" (:package ctx) (:name ast))]
    (println codename)))

(defn analyze [db fid src]
  (let [ast (parse-tree src)]
    (tx-data db fid ast {:sha->id (util/index->id-fn db :code/sha)
                         :codename->id (util/index->id-fn db :code/name)})))
  
(defn schemas []
  {1 [{:db/id #db/id[:db.part/db]
       :db/ident :java/class
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/one
       :db/doc "codename defined by a class definition"
       :db.install/_attribute :db.part/db}
      {:db/id #db/id[:db.part/db]
       :db/ident :java.class/methods
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/many
       :db/doc "The methods defined by a class"
       :db.install/_attribute :db.part/db}
      {:db/id #db/id[:db.part/db]
       :db/ident :java/interface
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/one
       :db/doc "codename defined by an interface definition"
       :db.install/_attribute :db.part/db}
      {:db/id #db/id[:db.part/db]
       :db/ident :java.interface/methods
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/many
       :db/doc "The methods defined in an interface"
       :db.install/_attribute :db.part/db}]})

(deftype JavaAnalyzer []
  az/Analyzer
  (keyname [a] :java)
  (revision [a] 1)
  (extensions [a] [".java"])
  (schemas [a] (schemas))
  (analyze [a db fid src] (analyze db fid src)))

(defn impl [] (JavaAnalyzer.))

(comment
  (def uri "datomic:free://localhost:4334/git")
  (def conn (d/connect uri))
  (def db (d/db conn))
  
  (clojure.pprint/pprint
   (sort
    (d/q '[:find ?fname ?n ; ?commit
           :where
           [?cq :java/class ?c]
           [?c :code/name ?n]
           [?cq :codeq/file ?f]
           [?node :node/object ?f]
           [?node :node/filename ?filename]
           [?filename :file/name ?fname]]
         db)))

  (d/delete-database uri)

  )