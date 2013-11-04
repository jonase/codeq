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
   {:keys [sha name loc src interface? methods] :as ast}
   {:keys [sha->id codename->id loc->codeqid package] :as ctx}]
  (let [loc-str (apply pr-str loc)
        codename (format "%s.%s" package name)
        codeid (sha->id sha)

        codetx (if (util/tempid? codeid)
                 {:db/id codeid
                  :code/sha sha
                  :code/text src})

        codeqid (loc->codeqid loc-str)

        codeqtx (if (util/tempid? codeqid)
                  {:db/id codeqid
                   :codeq/file fid
                   :codeq/loc loc-str
                   :codeq/code codeid}
                  {:db/id codeqid})

        nameid (codename->id codename)

        nametx (if (util/tempid? nameid)
                 {:db/id nameid
                  :code/name codename})

        codeqtx (assoc codeqtx
                  :java/class nameid
                  :java/interface? interface?
                  :java/package-name package
                  :java/class-name name)

        methodtxs (mapcat #(tx-data db fid % (assoc ctx :typename codename :parent codeqid)) methods)]
    
    (remove nil? (concat [codetx codeqtx nametx] methodtxs))))

(defmethod tx-data :enum-declaration [db fid ast ctx]
  (let [codename (format "ED %s.%s" (:package ctx) (:name ast))]
    (println codename)))

(defmethod tx-data :annotation-type-declaration [db fid ast ctx]
  (let [codename (format "ATD %s.%s" (:package ctx) (:name ast))]
    (println codename)))

(defmethod tx-data :method-declaration
  [db fid
   {:keys [name parameters loc sha src] :as ast}
   {:keys [sha->id codename->id loc->codeqid typename parent] :as ctx}]
  (let [loc-str (apply pr-str loc)
        codename (format "%s/%s"
                         typename
                         name)

        codeid (sha->id sha)

        codetx (when (util/tempid? codeid)
                 {:db/id codeid
                  :code/sha sha
                  :code/text src})

        codeqid (loc->codeqid loc)

        codeqtx (if (util/tempid? codeqid)
                  {:db/id codeqid
                   :codeq/file fid
                   :codeq/loc loc-str
                   :codeq/code codeid
                   :codeq/parent parent}
                  {:db/id codeqid})
        nameid (codename->id codename)
        nametx (if (util/tempid? nameid)
                 {:db/id nameid
                  :code/name codename})
        codeqtx (assoc codeqtx
                  :java/method nameid
                  :java/method-name name)]
    (remove nil? [codetx codeqtx nametx])))

(defn analyze [db fid src]
  (let [ast (parse-tree src)]
    (tx-data db fid ast {:sha->id (util/index->id-fn db :code/sha)
                         :codename->id (util/index->id-fn db :code/name)
                         :loc->codeqid #(or (ffirst (d/q '[:find ?e :in $ ?f ?loc
                                                           :where
                                                           [?e :codeq/file ?f]
                                                           [?e :codeq/loc ?loc]]
                                                         db fid %))
                                            (d/tempid :db.part/user))})))
  
(defn schemas []
  {1 [{:db/id #db/id[:db.part/db]
       :db/ident :java/class
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/one
       :db/doc "codename defined by a class definition"
       :db.install/_attribute :db.part/db}

      {:db/id #db/id[:db.part/db]
       :db/ident :java/method
       :db/valueType :db.type/ref
       :db/cardinality :db.cardinality/one
       :db/doc "Java method"
       :db.install/_attribute :db.part/db}

      {:db/id #db/id[:db.part/db]
       :db/ident :java/package-name
       :db/valueType :db.type/string
       :db/cardinality :db.cardinality/one
       :db/doc ""
       :db.install/_attribute :db.part/db}

      {:db/id #db/id[:db.part/db]
       :db/ident :java/class-name
       :db/valueType :db.type/string
       :db/cardinality :db.cardinality/one
       :db/doc ""
       :db.install/_attribute :db.part/db}

      {:db/id #db/id[:db.part/db]
       :db/ident :java/interface?
       :db/valueType :db.type/boolean
       :db/cardinality :db.cardinality/one
       :db/doc ""
       :db.install/_attribute :db.part/db}

      {:db/id #db/id[:db.part/db]
       :db/ident :java/method-name
       :db/valueType :db.type/string
       :db/cardinality :db.cardinality/one
       :db/doc ""
       :db.install/_attribute :db.part/db}
      ]})


(deftype JavaAnalyzer []
  az/Analyzer
  (keyname [a] :java)
  (revision [a] 1)
  (extensions [a] [".java"])
  (schemas [a] (schemas))
  (analyze [a db fid src] (analyze db fid src)))

(defn impl [] (JavaAnalyzer.))
