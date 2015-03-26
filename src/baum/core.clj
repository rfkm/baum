(ns baum.core
  (:refer-clojure :exclude [read-string])
  (:require [baum.util :as u]
            [clojure.core.match :as m]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.tools.reader :as r]
            [clojure.tools.reader.edn :as edn]
            [clojure.walk :as w]
            [environ.core :refer [env]]))

(def ^:dynamic ^:private *context*
  "Holds variables bound by `baum/let`."
  {})

(defn- delegate-to-reducer [reducer-key name args & body]
  `(let [f# (fn ~name ~args ~@body)]
     (defn ~name [v#]
       {~reducer-key [f# v#]})))

(defmacro defreader [name args & body]
  (apply delegate-to-reducer ::invoke name args body))

(defmacro deflazyreader [name args & body]
  (apply delegate-to-reducer ::lazy-invoke name args body))

;;;
;;; Readers
;;;

(declare reduction)

(deflazyreader inspect-reader [v opts]
  (let [reduced (reduction v opts)
        before (with-out-str (pprint v))
        after (with-out-str (pprint reduced))]
    (printf "\n%s\n↓ ↓ ↓\n\n%s\n" before after)
    reduced))

(defreader env-reader [v opts]
  (let [vs (if-not (vector? v) [v nil] v)] ; Add nil as fallback
    (or (some env (butlast vs))
        (last vs))))

(defreader resource-reader [v opts]
  (io/resource v))

(defreader file-reader [v opts]
  (io/file v))

(defreader files-reader [v opts]
  (let [[path & [re]] (u/vectorize v)
        re (or re #"")
        re (if (string? re) (re-pattern re) re)]
    (->> path
         io/file
         file-seq
         (filter #(.isFile ^java.io.File %))
         (filter #(re-find re (str %)))
         sort
         vec)))

(defreader str-reader [v opts]
  (apply str v))

(defreader regex-reader [v opts]
  (re-pattern v))

(defreader eval-reader [v opts]
  (when-not r/*read-eval*
    (throw (ex-info "eval-reader not allowed when *read-eval* is false"
                    {:value v})))
  (eval v))

(deflazyreader some-reader [v opts]
  (let [vs (u/vectorize v)]
    (u/some+ #(reduction % opts) vs)))

(deflazyreader match-reader [[vars & clauses] opts]
  (let [cs        (partition 2 clauses)
        saved     (reduce (fn [acc [_ v]]
                            (assoc acc (keyword (gensym)) v))
                          {}
                          cs)
        saved-inv (set/map-invert saved)
        clauses   (mapcat (fn [[c a]]
                            [c (get saved-inv a)])
                          cs)]
    (get saved (eval `(m/match ~(reduction vars opts) ~@clauses)))))

(declare refer-global-variable)

(defreader ref-reader [v opts]
  {:pre ((symbol? v))}
  (if (contains? *context* v)
    (*context* v)
    (refer-global-variable v)))

(declare read-config safe-read-config)

(defn- import-multiple [v importer]
  (->> v
       u/vectorize
       (map importer)
       (remove nil?)
       (reduce u/deep-merge)))

(defn- import-config [v opts]
  (import-multiple v #(if ((some-fn map? nil?) %)
                        %
                        (read-config % opts))))

(defn- import-config* [v opts]
  (import-multiple v #(if ((some-fn map? nil?) %)
                        %
                        (safe-read-config % opts nil))))

(defreader import-reader [v opts]
  (import-config v opts))

(defreader import-reader* [v opts]
  (import-config* v opts))


;;;
;;; Reduction
;;;

(defn- apply-reducers [reducers m get-reducer opts]
  (let [key-set (set (keys reducers))]
    (reduce (fn [acc-m k]
              (if-let [f (get-reducer reducers k)]
                (f (dissoc acc-m k) (acc-m k) opts)
                acc-m))
            m
            (or (and (map? m)
                     (seq (filter key-set (keys m))))
                []))))

(defn- apply-lazy-reducer [reducers m opts]
  (apply-reducers reducers
                  m
                  (fn [hs k]
                    (let [h (k hs)]
                      (and (map? h)
                           (:lazy h))))
                  opts))

(defn- apply-eager-reducer [reducers m opts]
  (apply-reducers reducers
                  m
                  (fn [hs k]
                    (let [h (k hs)]
                      (or (and (map? h)
                               (:eager h))
                          (and (fn? h)
                               h))))
                  opts))

(defn reduction
  [m opts]
  (let [reducers (:reducers opts)
        new-m    (apply-lazy-reducer reducers m opts)]

    ;; XXX: Lexical scope is better? There is no function call in the
    ;;      static config file world (except for `eval`), so there may
    ;;      be no difference between lexical scope and dynamic scope
    ;;      in almost all cases. However, it may be a problem that
    ;;      scopes are visible from other imported files.
    (binding [*context* (merge *context*
                               (::context (meta new-m) {}))]
      (w/walk #(reduction % opts)
              #(apply-eager-reducer reducers % opts)
              new-m))))


;;;
;;; Reducers
;;;

(defn- reduce-invoke [m [f v] opts]
  (f v opts))

(defn- reduce-include [m v opts]
  (import-config (conj (u/vectorize v) m) opts))

(defn- reduce-include* [m v opts]
  (import-config* (conj (u/vectorize v) m) opts))

(defn- reduce-override [m v opts]
  (import-config (into [m] (u/vectorize v)) opts))

(defn- reduce-override* [m v opts]
  (import-config* (into [m] (u/vectorize v)) opts))

(defn- eval-bindings [bindings]
  (let [syms (vec (take-nth 2 bindings))
        qs   (mapv (fn [sym] `(~'quote ~sym)) syms)]
    (eval `(let ~bindings
             (interleave ~qs ~syms)))))


(defn- reduce-bindings [bindings opts unsafe]
  (reduce (fn [acc [k v]]
            (binding [*context* (merge *context*
                                       (apply hash-map acc))]
              (let [v (if (unsafe v)
                        (list 'quote (reduction v opts))
                        v)]
                (conj acc k (eval `(let ~acc ~v))))))
          []
          (partition 2 bindings)))

(defn- create-context [bindings opts]
  (let [unsafe (set (take-nth 2 (rest bindings)))]
    (->> bindings
         destructure
         (#(reduce-bindings % opts unsafe))
         (apply hash-map))))

(defn- reduce-let [m v opts]
  (with-meta m {::context (create-context v opts)}))


;;;
;;; Global Variables
;;;

(defmulti refer-global-variable identity)

(defmethod refer-global-variable :default [sym]
  (throw (ex-info (format "Unable to resolve symbol: %s in this context" sym)
                  {:context *context*})))

(defmethod refer-global-variable 'HOSTNAME [_]
  (.. java.net.InetAddress getLocalHost getHostName))

(defmethod refer-global-variable 'HOSTADDRESS [_]
  (.. java.net.InetAddress getLocalHost getHostAddress))

;;; --

(defn- apply-transformers [m opts]
  (reduce (fn [acc f]
            (f acc opts))
          m
          (:transformers opts)))

(defn wrap-nil-safe-reader
  "Returns nil-safe version of a given reader function. If a reader
  function returns falsy value, replace it with a pseudo
  nil, :baum/nil. Otherwise the EDN reader will throw an exception."
  [f]
  (fn [& args]
    (or (apply f args)
        :baum/nil)))

(defn replace-pseudo-nils
  "Replace all pseudo nils(:baum/nil) in m with real nils."
  [m opts]
  (w/postwalk-replace {:baum/nil nil} m))

(defn default-readers [& [opts]]
  {'baum/env       env-reader
   'baum/str       str-reader
   'baum/regex     regex-reader
   'baum/match     match-reader
   'baum/resource  resource-reader
   'baum/file      file-reader
   'baum/files     files-reader
   'baum/import    import-reader
   'baum/import*   import-reader*
   'baum/some      some-reader
   'baum/eval      eval-reader
   'baum/ref       ref-reader
   'baum/inspect   inspect-reader})

(defn default-reducers [& [opts]]
  {::invoke        reduce-invoke         ; internal
   ::lazy-invoke   {:lazy reduce-invoke} ; internal
   :baum/let       {:lazy reduce-let}
   :baum/include   reduce-include
   :baum/include*  reduce-include*
   :baum/override  reduce-override
   :baum/override* reduce-override*})

(def default-aliases
  {'baum/env       'env
   'baum/str       'str
   'baum/regex     'regex
   'baum/match     'match
   'baum/resource  'resource
   'baum/file      'file
   'baum/files     'files
   'baum/import    'import
   'baum/import*   'import*
   'baum/some      'some
   'baum/eval      '=
   'baum/ref       '-
   'baum/inspect   'inspect
   :baum/let       '$let
   :baum/include   '$include
   :baum/include*  '$include*
   :baum/override  '$override
   :baum/override* '$override*})

(defn default-transformers [& [opts]]
  [replace-pseudo-nils
   reduction])

(defn default-options [opts]
  {:readers      (merge (default-readers opts)
                        *data-readers*)
   :reducers     (default-reducers opts)
   :transformers []
   :aliases      {}
   :shorthand?  false
   :edn?        false})

(defn apply-aliases [{:keys [aliases] :as opts}]
  (-> opts
      (update-in [:reducers] u/alias-keys aliases)
      (update-in [:readers] u/alias-keys aliases)))

(defn- apply-default-aliases [{:keys [shorthand?] :as opts}]
  (if (:shorthand? opts)
    (update-in opts [:aliases] #(u/deep-merge default-aliases %))
    opts))

(defn update-options [opts]
  (-> (default-options opts)
      (u/deep-merge (or opts {}))
      (update-in [:readers] #(u/fmap wrap-nil-safe-reader %))
      (update-in [:transformers] concat (default-transformers opts))
      apply-default-aliases
      apply-aliases))

;;;
;;; API
;;;

(defn read-string
  ([s]
   (read-string {:eof nil} s))
  ([opts s]
   (let [opts (update-options opts)]
     (if (:edn? opts)
       (apply-transformers (edn/read-string opts s) opts)
       (binding [r/*data-readers* (merge r/*data-readers* (opts :readers))]
         (apply-transformers (r/read-string s) opts))))))

(defn read-config
  ([file]
   (read-config file {}))
  ([file opts]
   (read-string opts (slurp (u/expand-home file)))))

(defn safe-read-config
  "Same as `read-config`, but returns alt when an error occured."
  {:arglists
   '([file alt] [file opts alt])}
  [& args]
  (try
    (apply read-config (butlast args))
    (catch Exception e
      (last args))))
