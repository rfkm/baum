(ns baum.util
  (:import java.io.FileNotFoundException))

(defn deep-merge
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(defn map-every-nth [f n coll]
  (map-indexed (fn [i v]
                 (if (zero? (mod (inc i) n))
                   (f v)
                   v))
               coll))

(defn vectorize [v]
  (if (vector? v) v [v]))

(defn alias-key [m [original new]]
  (if (contains? m original)
    (assoc m new (get m original))
    m))

(defn alias-keys [map aliases]
  (reduce alias-key map aliases))

(defn- parse-ns-var! [ns-var]
  (if-let [ns (namespace (symbol ns-var))]
    [(symbol ns) (symbol (name ns-var))]
    (throw (IllegalArgumentException.
            (str "Invalid format:\n\n\t"
                 ns-var
                 "\n\nns-var must be of the form: '<namespace>/<var-name>'.")))))

(defn- resolve-ns-var! [ns-sym var-sym]
  (try (require ns-sym :reload)
       (catch FileNotFoundException e
         (throw (IllegalArgumentException.
                 (format "Unable to load ns: %s/%s" ns-sym var-sym)))))
  (or (ns-resolve ns-sym var-sym)
      (throw (IllegalArgumentException.
              (format "Unable to load var: %s/%s" ns-sym var-sym)))))

(defn resolve-var! [sym]
  (apply resolve-ns-var! (parse-ns-var! sym)))
