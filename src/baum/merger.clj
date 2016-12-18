(ns baum.merger
  (:require [clojure.set :as set]
            [baum.util :as u]))

;; Borrowed the idea that controlling the merging strategy based on metadata
;; from Leiningen

(def ^:private +priority-exactly+ 2)
(def ^:private +priority-any+ 1)

(defn- matcher-priority [matcher]
  (::priority (meta matcher)))

(defn- with-matcher-priority [matcher p]
  (with-meta matcher {::priority p}))

(defn- compare-matchers [a b]
  (let [[alp arp] (matcher-priority a)
        [blp brp] (matcher-priority b)
        ret       (compare (+ alp arp)
                           (+ blp brp))]
    (if (zero? ret)
      ;; right wins
      (compare arp brp)
      ret)))

(defn- ->sub-matcher [matcher]
  (cond (keyword? matcher)
        (with-matcher-priority
          #(-> % meta matcher)
          +priority-exactly+)

        (vector? matcher)
        (with-matcher-priority
          (->> matcher
               (map ->sub-matcher)
               (apply every-pred))
          (* +priority-exactly+ (count matcher)))

        (= matcher '_)
        (with-matcher-priority
          (constantly true)
          +priority-any+)

        (fn? matcher)
        (with-matcher-priority
          matcher
          +priority-exactly+)

        :else
        (throw (ex-info "Found an incorrect matcher" {:matcher matcher}))))

(defn- ->matcher [matcher]
  (cond (and (vector? matcher) (= 2 (count matcher)))
        (let [[left-matcher right-matcher] (map ->sub-matcher matcher)]
          (with-matcher-priority
            (fn [left right]
              (and (left-matcher left)
                   (right-matcher right)))
            [(matcher-priority left-matcher)
             (matcher-priority right-matcher)]))

        (= matcher '_)
        (with-matcher-priority
          (constantly true)
          [+priority-any+ +priority-any+])

        (fn? matcher)
        (with-matcher-priority
          matcher
          [+priority-exactly+ +priority-exactly+])

        :else
        (throw (ex-info "Found an incorrect matcher" {:matcher matcher}))))

(defn- dispatch-matcher [multimethod-var left right]
  (let [candidates (remove #{:default} (keys (methods (deref multimethod-var))))]
    (->> candidates
         (filter #((->matcher %) left right))
         (sort-by ->matcher compare-matchers)
         reverse
         first)))

(defn need-to-apply-matchers? [multimethod-var left right]
  (if-let [matcher (dispatch-matcher multimethod-var left right)]
    (not= :default matcher)
    false))

;; Using multimethods to implement the merging strategies might be not good
;; idea since that makes it harder to control the priority of rules. While I
;; would like to keep this for the better customizability, I might rethink in
;; the feature.
(defmulti prioritized-merge (partial dispatch-matcher #'prioritized-merge))

(defmulti merge-colls (partial dispatch-matcher #'merge-colls))

(defn merge-tree
  ([] nil)
  ([left] left)
  ([left right]
   (cond
     (need-to-apply-matchers? #'prioritized-merge left right)
     (prioritized-merge left right)

     (need-to-apply-matchers? #'merge-colls left right)
     (merge-colls left right)

     (and (map? left) (map? right))
     (merge-with merge-tree left right)

     :else
     right))
  ([left right & more]
   (reduce merge-tree (into [left right] more))))


;;
;; Rules
;;

(defmethod prioritized-merge [:displace '_] [left right]
  right)

(defmethod prioritized-merge ['_ :displace] [left right]
  (u/with-meta-safe left (merge (meta left) (meta right))))

(defmethod prioritized-merge ['_ :replace] [left right]
  right)

(defmethod merge-colls [coll? [coll? :append]] [left right]
  (into (empty right) (concat left right)))

(defmethod merge-colls [coll? [coll? :prepend]] [left right]
  (into (empty right) (concat right left)))
