(ns baum.core
  (:refer-clojure :exclude [read-string])
  (:require [baum.resolver :as resolver]
            [baum.util :as u]
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

(defreader resolve-reader [v opts]
  (u/resolve-var! v))

(deflazyreader some-reader [v opts]
  (let [vs (u/vectorize v)]
    (some #(reduction % opts) vs)))

(deflazyreader if-reader [[test then & [else]] opts]
  (if (reduction test opts) then else))

(deflazyreader match-reader [[vars & clauses] opts]
  (let [protect (partial u/map-every-nth #(list 'quote %) 2)]
    (eval `(m/match ~(reduction vars opts) ~@(protect clauses)))))

(declare refer-global-variable)

(defreader ref-reader [v opts]
  {:pre ((symbol? v))}
  (if (contains? *context* v)
    (*context* v)
    (refer-global-variable v)))

(declare read-file safe-read-file)

(defn- import-multiple [v importer]
  (->> v
       u/vectorize
       (map importer)
       (remove nil?)
       (reduce u/deep-merge)))

(defn- import-file [v opts]
  (import-multiple v #(if ((some-fn map? nil?) %)
                        %
                        (read-file % opts))))

(defn- import-file* [v opts]
  (import-multiple v #(if ((some-fn map? nil?) %)
                        %
                        (safe-read-file % opts nil))))

(defreader import-reader [v opts]
  (import-file v opts))

(defreader import-reader* [v opts]
  (import-file* v opts))

(declare read-string)

(defreader read-reader [v opts]
  (read-string opts v))

(defreader read-env-reader [v opts]
  (let [vs (if-not (vector? v) [v nil] v)] ; Add nil as fallback
    (or (read-string (some env (butlast vs)))
        (last vs))))


;;;
;;; Reduction
;;;

(defn- apply-reducers [reducers m get-reducer opts]
  (let [key-set (set (keys reducers))]
    (reduce (fn [acc-m k]
              (if-let [f (get-reducer reducers k)]
                (let [reduced (f (dissoc acc-m k) (acc-m k) opts)]
                  (if (not= reduced acc-m)
                    (reduction reduced opts)
                    reduced))
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

(defn reduction [m opts]
  (let [reducers (:reducers opts)]
    (w/walk #(reduction % opts)
            #(apply-eager-reducer reducers % opts)
            (apply-lazy-reducer reducers m opts))))


;;;
;;; Reducers
;;;

(defn reduce-invoke [m [f v] opts]
  (f v opts))

(defn reduce-include [m v opts]
  (import-file (conj (u/vectorize v) m) opts))

(defn reduce-include* [m v opts]
  (import-file* (conj (u/vectorize v) m) opts))

(defn reduce-override [m v opts]
  (import-file (into [m] (u/vectorize v)) opts))

(defn reduce-override* [m v opts]
  (import-file* (into [m] (u/vectorize v)) opts))

(defn- reduce-bindings [bindings opts]
  (reduce (fn [acc [k v]]
            (binding [*context* (merge *context*
                                       (apply hash-map acc))]
              (conj acc k (eval `(let ~acc ~(reduction v opts))))))
          []
          (partition 2 bindings)))

(defn- create-context [bindings opts]
  (let [protect (partial u/map-every-nth #(list 'quote %) 2)]
    (-> bindings
        protect
        destructure
        (reduce-bindings opts)
        (->> (apply hash-map)))))

(defn reduce-let [m v opts]
  ;; XXX: Lexical scope is better? There is no function call in the
  ;;      static file world (except for `eval`), so there may
  ;;      be no difference between lexical scope and dynamic scope
  ;;      in almost all cases. However, it may be a problem that
  ;;      scopes are visible from other imported files.
  (binding [*context* (merge *context*
                             (create-context v opts))]
    (reduction m opts)))

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

(defn default-readers [& [opts]]
  {'baum/env      env-reader
   'baum/str      str-reader
   'baum/regex    regex-reader
   'baum/if       if-reader
   'baum/match    match-reader
   'baum/resource resource-reader
   'baum/file     file-reader
   'baum/files    files-reader
   'baum/read     read-reader
   'baum/read-env read-env-reader
   'baum/import   import-reader
   'baum/import*  import-reader*
   'baum/some     some-reader
   'baum/resolve  resolve-reader
   'baum/eval     eval-reader
   'baum/ref      ref-reader
   'baum/inspect  inspect-reader})

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
   'baum/if        'if
   'baum/match     'match
   'baum/resource  'resource
   'baum/file      'file
   'baum/files     'files
   'baum/read      'read
   'baum/read-env  'read-env
   'baum/import    'import
   'baum/import*   'import*
   'baum/some      'some
   'baum/resolve   'resolve
   'baum/eval      'eval
   'baum/ref       '-
   'baum/inspect   'inspect
   :baum/let       '$let
   :baum/include   '$include
   :baum/include*  '$include*
   :baum/override  '$override
   :baum/override* '$override*})

(defn default-transformers [& [opts]]
  [reduction])

(defn default-options [opts]
  {:readers      (merge (default-readers opts)
                        *data-readers*)
   :reducers     (default-reducers opts)
   :transformers []
   :aliases      {}
   :shorthand?  true
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
      (update-in [:transformers] concat (default-transformers opts))
      apply-default-aliases
      apply-aliases))

;;;
;;; API
;;;

(defn read-string
  "Read the given string as a Baum-formatted string. The acceptable options are
  the following:

  :readers - A map of readers. See `default-readers`.
  :reducers - A map of reducers. See `default-reducers`.
  :aliases - A map of aliases. See `default-aliases`.
  :shorthand? - Whether to enable the shorthand notation. In other words,
  whether to enable the default alias settings.
  :edn? - Whether to enable the EDN-only reader."
  ([s]
   (read-string {:eof nil} s))
  ([opts s]
   (let [opts (update-options opts)]
     (if (:edn? opts)
       (apply-transformers (edn/read-string opts s) opts)
       (binding [r/*data-readers* (merge r/*data-readers* (opts :readers))]
         (apply-transformers (r/read-string s) opts))))))

(defn read-file
  "Read a Baum-formatted file. See `read-string` for the list of available
  options."
  ([file]
   (read-file file {}))
  ([file opts]
   (resolver/with-resolved [target file]
     (read-string opts (slurp target)))))

(defn safe-read-file
  "Same as `read-file`, but returns alt when file doesn't exist."
  {:arglists
   '([file alt] [file opts alt])}
  [& args]
  (try
    (apply read-file (butlast args))
    (catch java.io.FileNotFoundException e
      (last args))))

(defn read-config
  "DEPRECATED: Use 'read-file' instead.
  Read a Baum-formatted file. See `read-string` for the list of available
  options."
  {:deprecated "0.4.0"
   :arglists
   '([file] [file opts])}
  [& args]
  (println "Warning: read-config is deprecated; use read-file")
  (apply read-file args))

(defn safe-read-config
  "DEPRECATED: Use 'safe-read-file' instead.
  Same as `read-config`, but returns alt when file doesn't exist."
  {:deprecated "0.4.0"
   :arglists
   '([file alt] [file opts alt])}
  [& args]
  (println "Warning: safe-read-config is deprecated; use safe-read-file")
  (apply safe-read-file args))
