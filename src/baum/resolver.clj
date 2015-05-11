(ns baum.resolver
  (:refer-clojure :exclude [resolve])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [me.raynes.fs :as fs])
  (:import [java.io File]
           [java.net URI URL MalformedURLException]))

(def ^:dynamic *current-file*
  "A current file being parsed"
  nil)

(defprotocol Resolver
  (resolve [parent target]))

(defn- sep->slash [s]
  (str/replace s File/separator "/"))

(defn- relative-path? [path]
  (and (string? path)
       (or (.startsWith ^String path "./")
           (.startsWith ^String path "../"))))

(defn- absolute-path? [path]
  (and (string? path)
       (.startsWith ^String path "/")))

(defmulti resolve-uri
  (fn [^URI parent ^String target]
    (keyword "baum.resolver" (.getScheme parent))))

(defmethod resolve-uri ::jar [^URI parent ^String target]
  (if (absolute-path? target)
    target
    (let [target (if (relative-path? target)
                   target
                   (str "/" target))
          path   (.getSchemeSpecificPart parent)
          idx    (.lastIndexOf path "!/")
          local  (subs path (+ 1 idx))
          ctx    (subs path 0 idx)]
      (str "jar:" ctx "!"
           (sep->slash (resolve (io/file local) target))))))

(defmethod resolve-uri ::http [^URI parent ^String target]
  (if (absolute-path? target)
    target
    (str (.resolve parent (if (relative-path? target)
                            target
                            (str "/" target))))))

(derive ::https ::http)
(derive ::ftp ::http)

(defmethod resolve-uri :default [^URI parent ^String target]
  (if (relative-path? target)
    (str (.resolve parent (if (relative-path? target)
                            target
                            (str "/" target))))
    target))


;;;
;;; Default impls
;;;

(extend-protocol Resolver
  String
  (resolve [parent target]
    (try
      (resolve (URL. parent) target)
      (catch MalformedURLException e
        (resolve (File. parent) target))))

  File
  (resolve [parent target]
    (if (relative-path? target)
      (.getCanonicalPath (io/file (.getParentFile parent) target))
      target))

  URL
  (resolve [parent target]
    (resolve (.toURI parent) target))

  URI
  (resolve [parent target]
    (resolve-uri parent target))

  nil
  (resolve [parent target]
    target)

  Object
  (resolve [parent target]
    target))

(defn expand-path [path]
  (if (string? path)
    (->> (fs/expand-home path)
         (resolve *current-file*))
    path))

(defmacro with-resolved [[sym target] & body]
  `(let [target# (expand-path ~target)]
     (binding [*current-file* target#]
       (let [~sym target#]
         ~@body))))
