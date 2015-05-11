(ns baum.resolver-test
  (:require [baum.resolver :as resolver]
            [clojure.java.io :as io]
            [midje.sweet :refer :all])
  (:import [java.net URL]))


(facts "#resolve"
  (facts "parent = File"
    (fact "Normally a given path will be resolved as a relative path to the
    project root."
      (resolver/resolve (io/file "foo/bar.edn") "bar.edn") => "bar.edn")

    (fact "A path starting with ./ or ../ will be resolved as a relative path to
    its parent."
      (resolver/resolve (io/file "foo/bar.edn") "./bar.edn")
      => (.getCanonicalPath (io/file "foo/bar.edn"))

      (resolver/resolve (io/file "foo/bar.edn") "../baz/../bar.edn")
      => (.getCanonicalPath (io/file "bar.edn")))

    (fact "If an absolute path is given, just returns it as it is."
      (resolver/resolve (io/file "foo/bar.edn") "/foo.edn")
      => "/foo.edn")

    (fact "just returns a given path as it is"))

  (facts "parent = URL(jar)"
    (fact "Normally a given path will be resolved as a relative path to root in
    a parent jar"
      (resolver/resolve (str (io/resource "midje/sweet.clj")) "project.clj")
      => #"^jar:.*midje[^/]*\.jar!/project.clj")

    (fact "A path starting with ./ or ../ will be resolved as a relative path to
    its parent."
      (resolver/resolve (io/resource "clojure/core.clj") "./string.clj")
      => (str (io/resource "clojure/string.clj"))

      (resolver/resolve (io/resource "clojure/java/io.clj") "../foo/../core.clj")
      => (str (io/resource "clojure/core.clj")))

    (fact "If an absolute path is given, just returns it as it is."
      (resolver/resolve (io/resource "clojure/java/io.clj") "/foo.edn")
      => "/foo.edn"))

  (facts "parent = URL(http/https)"
    (fact "Normally a given path will be resolved as a relative path to the root
    URL."
      (resolver/resolve (URL. "http://example.com/foo/bar.edn") "bar.edn")
      => "http://example.com/bar.edn"

      (resolver/resolve (URL. "https://example.com/foo/bar.edn") "bar.edn")
      => "https://example.com/bar.edn")

    (fact "A path starting with ./ or ../ will be resolved as a relative path to
    its parent."
      (resolver/resolve (URL. "http://example.com/foo/bar.edn") "./foo.edn")
      => "http://example.com/foo/foo.edn"

      (resolver/resolve (URL. "https://example.com/foo/bar.edn") "../bar/../foo.edn")
      => "https://example.com/foo.edn")

    (fact "If an absolute path is given, just returns it as it is."
      (resolver/resolve (URL. "https://example.com/foo/bar.edn") "/foo.edn")
      => "/foo.edn"))

  (facts "parent = URL(file)"
    (fact "Normally a given path will be resolved as a relative path to the
    project root."
      (resolver/resolve (io/as-url (io/file "foo/bar.edn")) "bar.edn")
      => "bar.edn")

    (fact "A path starting with ./ or ../ will be resolved as a relative path to
    its parent."
      (resolver/resolve (io/as-url (io/file "foo/bar.edn")) "./bar.edn")
      => (str (io/as-url (io/file "foo/bar.edn")))

      (resolver/resolve (io/as-url (io/file "foo/bar.edn")) "../baz/../bar.edn")
      => (str (io/as-url (io/file "bar.edn"))))

    (fact "If an absolute path is given, just returns it as it is."
      (resolver/resolve (io/as-url (io/file "foo/bar.edn")) "/foo.edn")
      => "/foo.edn"))

  (facts "parent = unknown"
    (fact "Always just returns a given path as it is"
      (resolver/resolve nil "foo.edn") => "foo.edn"
      (resolver/resolve nil "./foo.edn") => "./foo.edn"
      (resolver/resolve nil "../foo.edn") => "../foo.edn"
      (resolver/resolve :foo "../foo.edn") => "../foo.edn")))
