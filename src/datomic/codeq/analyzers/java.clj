(ns datomic.codeq.analyzers.java
  (:require [datomic.api :as d]
            [datomic.codeq.analyzer :as az]
            [datomic.codeq.util :as util]
            [datomic.codeq.analyzers.java.parser :refer [parse-tree]]))

(defmulti emit-tx-data (fn [db ast fid src ctx] (:node ast)))

(defmethod emit-tx-data :compilation-unit [db ast fid src ctx]
  (let [package (-> ast :package :name)
        types (:types ast)]
    (mapcat #(emit-tx-data db % fid src (assoc ctx :package package)) types)))

(defmethod emit-tx-data :type-declaration [db ast fid src ctx]
  (let [codename->id (:codename->id ctx)
        sha->id (:sha->id ctx)
        codename (format "%s.%s" (:package ctx) (:name ast))
        codeid (codename->id codename)
        codetx (if (util/tempid? codeid)
                 {:db/id codeid
                  :code/sha (:sha ast)
                  :code/text (:src ast)})
        codeqid (or (ffirst (d/q '[:find ?e :in $ ?f ?loc
                                   :where
                                   [?e :codeq/file ?f] 
                                   [?e :codeq/loc ?loc]]
                                 db fid loc))
                    (d/tempid :db.part/user))
        codeqtx (if (util/tempid? codeqid)
                  {:db/id codeqid
                   :codeq/file fid
                   :codeq/loc loc
                   :codeq/code codeid})]))

(defmethod emit-tx-data :enum-declaration [db ast fid src ctx]
  (let [codename (format "ED %s.%s" (:package ctx) (:name ast))]
    (println codeq-name)))

(defmethod emit-tx-data :annotation-type-declaration [db ast fid src ctx]
  (let [codename (format "ATD %s.%s" (:package ctx) (:name ast))]
    (println codeq-name)))

(defn analyze [db fid src]
  (let [ast (parse-tree src)]
    (emit-tx-data db ast fid src {:sha->id (util/index->id-fn db :code/sha)
                                  :codename->id (util/index->id-fn db :code/name)})))
  
(defn schemas []
  {1 [{:db/id #db/id[:db.part/db]
       :db/ident :java/class
       :db/valueType :db.cardinality/one
       :db/doc "codename defined by a class definition"
       :db.install/_attribute :db.part/db}
      {:db/id #db/id[:db.part/db]
       :db/ident :java.class/methods
       :db/valueType :db.cardinality/many
       :db/doc "The methods defined by a class"
       :db.install/_attribute :db.part/db}]}
      {:db/id #db/id[:db.part/db]
       :db/ident :java/interface
       :db/valueType :db.cardinality/one
       :db/doc "codename defined by an interface definition"
       :db.install/_attribute :db.part/db}
      {:db/id #db/id[:db.part/db]
       :db/ident :java.interface/methods
       :db/valueType :db.cardinality/many
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
