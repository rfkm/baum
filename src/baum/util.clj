(ns baum.util
  (:require [me.raynes.fs :as fs]))

(defn deep-merge
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-every-nth [f n coll]
  (map-indexed (fn [i v]
                 (if (zero? (mod (inc i) n))
                   (f v)
                   v))
               coll))

(defn some+
  "Like `some` but handles :baum/nil as nil."
  [pred coll]
  (some (fn [v]
          (pred (when-not (= v :baum/nil) v))) coll))

(defn vectorize [v]
  (if (vector? v) v [v]))

(defn alias-key [m [original new]]
  (if (contains? m original)
    (assoc m new (get m original))
    m))

(defn alias-keys [map aliases]
  (reduce alias-key map aliases))

(defn expand-home [path]
  (if (string? path)
    (fs/expand-home path)
    path))
