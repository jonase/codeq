(ns datomic.codeq.examples
  (:require [datomic.api :refer [db q] :as d]))

(def uri "datomic:free://localhost:4334/git")

(defn codeq-db []
  (-> uri d/connect db))

;; committers
(q '[:find ?email
     :where
     [_ :commit/committer ?u]
     [?u :email/address ?email]]
   (codeq-db))

;; authors
(q '[:find ?email
     :where
     [_ :commit/author ?e]
     [?u :email/address ?email]]
   (codeq-db))

;; Initial commit date
(q '[:find (min ?date)
     :where
     [_ :commit/committedAt ?date]]
   (codeq-db))

;; How many commits in total
(q '[:find (count ?c)
     :where
     [?c :git/type :commit]]
   (codeq-db))

;; Top 3 committers
(->> (q '[:find ?email (count ?commit)
          :where
          [?commit :commit/author ?author]
          [?author :email/address ?email]]
        (codeq-db))
     (sort-by second)
     reverse
     (take 3))

;; First & latest commit date by author email
(q '[:find ?email (min ?date) (max ?date)
     :in $ ?email
     :where
     [?e :commit/committedAt ?date]
     [?e :commit/author ?u]
     [?u :email/address ?email]]
   (codeq-db) "your-email-here")

;; Rules from http://blog.datomic.com/2012/10/codeq.html
(def rules
  '[[(node-files ?n ?f)
     [?n :node/object ?f]
     [?f :git/type :blob]]
    [(node-files ?n ?f)
     [?n :node/object ?t]
     [?t :git/type :tree] 
     [?t :tree/nodes ?n2]
     (node-files ?n2 ?f)]
    
    [(object-nodes ?o ?n)
     [?n :node/object ?o]]
    [(object-nodes ?o ?n)
     [?n2 :node/object ?o]
     [?t :tree/nodes ?n2]
     (object-nodes ?t ?n)]

    [(commit-files ?c ?f)
     [?c :commit/tree ?root]
     (node-files ?root ?f)]

    [(commit-codeqs ?c ?cq)
     (commit-files ?c ?f)
     [?cq :codeq/file ?f]]

    [(file-commits ?f ?c)
     (object-nodes ?f ?n)
     [?c :commit/tree ?n]]

    [(codeq-commits ?cq ?c)
     [?cq :codeq/file ?f]
     (file-commits ?f ?c)]])

;; Find authors who has had part in the evolution of a function
(defn commit-dates [name]
  (map first
       (q '[:find (min ?date) ?sha
            :in $ % ?name
            :where
            [?n :code/name ?name]
            [?cq :clj/def ?n]
            [?cq :codeq/code ?c]
            [?c :code/sha ?sha]
            (codeq-commits ?cq ?commit)
            [?commit :commit/committedAt ?date]]
          (codeq-db)
          rules
          name)))

(defn committer-by-insts [insts]
  (q '[:find ?email
       :in $ [?inst ...]
       :where
       [?commit :commit/committedAt ?inst]
       [?commit :commit/committer ?u]
       [?u :email/address ?email]]
     (codeq-db)
     insts))

(-> "clojure.core.reducers/fold" commit-dates committer-by-insts)

;; What function definition has been part of the most commits
(->> (q '[:find ?name (count ?date)
          :where
          [_ :code/name ?name]
          [(datomic.codeq.examples/commit-dates ?name) [?date ...]]]
        (codeq-db))
     (sort-by second)
     reverse
     (take 5))
;; (["clojure.test-clojure/test-names" 19]
;;  ["clojure.main/repl" 17]
;;  ["clojure.core/defn" 14]
;;  ["clojure.core/promise" 10]
;;  ["clojure.core/future-call" 9])



