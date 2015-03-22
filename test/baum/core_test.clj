(ns baum.core-test
  (:require [baum.core :as c]
            [clojure.java.io :as io]
            [clojure.tools.reader :as r]
            [environ.core :refer [env]]
            [midje.sweet :refer :all]))

;;;
;;; prep
;;;

(defn rs
  ([s] (rs {} s))
  ([opts s]
   (let [[a b] (map #(c/read-string (merge opts %) s)
                    [{:edn? true} {:edn? false}])]
     (fact a => b)
     a)))

;;;
;;; Tests
;;;

(facts "can read a simple map"
  (let [test-conf-path "dev-resources/test.edn"
        ret {:foo :bar}]
    (fact "from a string path"
      (c/read-config test-conf-path) => ret)

    (fact "from a file object"
      (c/read-config (io/file test-conf-path)) => ret)

    (fact "from a url object"
      (c/read-config (io/as-url (io/file test-conf-path))) => ret)))


(facts "can read config files with reader macros"
  (fact "inst"
    (rs "{:a #inst \"1989-10-29\"}") => {:a #inst "1989-10-29"})

  (fact "env"
    (rs "{:a #baum/env :user}") => {:a (env :user)})

  (fact "env from project.clj"
    (rs "{:a #baum/env :env}") => {:a "dev"})

  (fact "env with fallback"
    (rs "#baum/env [:non-existent-env \"foo\"]") => "foo"
    (rs "#baum/env [:non-existent-env :user \"foo\"]") => (env :user)
    (rs "#baum/env [\"foo\"]") => "foo"
    (rs "#baum/env []") => nil)

  (fact "str"
    (rs "#baum/str [\"foo\" \"bar\"]") => "foobar")

  (fact "match"
    (let [s "{:a #baum/match [[#baum/env :env 0]
                             [\"dev\" 0]  {:a :a}
                             [\"prod\" _] {:b :b}
                             :else  #baum/eval (throw (Exception. \"unknown\"))]}"]
      (rs s) => {:a {:a :a}}
      (provided (env :env) => "dev")

      (rs s) => {:a {:b :b}}
      (provided (env :env) => "prod")

      (rs s) => (throws "unknown")
      (provided (env :env) => "unknown")

      (rs "{:a #baum/match [#baum/env :env]}") => (throws #"No matching clause")))

  (fact "resource"
    (rs "{:a #baum/resource \"test.edn\"}") => {:a (io/resource "test.edn")})

  (fact "file"
    (rs "{:a #baum/file \"dev-resources/test.edn\"}")
    => {:a (io/file "dev-resources/test.edn")})

  (fact "files"
    (rs "#baum/files [\"src\" \"clj$\"]")
    => [(io/file "src/baum/core.clj")
        (io/file "src/baum/util.clj")])

  (fact "files + filter"
    (rs "#baum/files [\"src\" \"core.*clj$\"]")
    => [(io/file "src/baum/core.clj")]

    (rs "#baum/files [\"src\" #baum/regex \"core.*clj$\"]")
    => [(io/file "src/baum/core.clj")])

  (fact "some"
    (rs "{:a #baum/some [nil nil 1 nil]}") => {:a 1}
    (rs "{:a #baum/some [#baum/env :non-existent-env
                         #baum/env :user]}")
    => {:a (env :user)}

    ;; If lazy evaluation works correctly, no exception will occurs.
    (rs "#baum/some [#baum/env :user
                     #baum/eval (throw (Exception. \"foo\"))]")
    => "rkworks"
    (provided (env :user) => "rkworks"))


  (fact "eval"
    (rs "{:a #baum/eval (+ 1 2)}") => {:a 3})

  (fact "*read-eval*"
    (binding [r/*read-eval* false]
      (rs "{:a #baum/eval (+ 1 2)}")) => (throws #"not allowed"))

  (fact "regex"
    (rs "#baum/regex \"^foo*$\" ") => #"^foo*$")


  (fact "import"
    (rs "{:parent #baum/import \"child.edn\"}") => {:parent {:a :b}}
    (provided
      (slurp "child.edn") => "{:a :b}"))

  (fact "nested import"
    (rs "{:parent #baum/import \"child.edn\"}") => {:parent {:a {:a :b}}}
    (provided
      (slurp "child.edn")  => "{:a #baum/import \"child2.edn\"}"
      (slurp "child2.edn") => "{:a :b}"))

  (fact "multiple import"
    (rs "{:parent #baum/import [\"child.edn\" \"child2.edn\"]}")
    => {:parent {:a {:a :b2} :c :d}}
    (provided
      (slurp "child.edn")  => "{:a {:a :b} :c :d}"
      (slurp "child2.edn") => "{:a {:a :b2}}"))

  (fact "Throws an exception when trying to import non-existent files"
    (rs "{:parent #baum/import \"child.edn\"}") => (throws java.io.FileNotFoundException))

  (fact "Returns an empty map when trying to import* non-existent files"
    (rs "{:parent #baum/import* \"child.edn\"}") => {:parent nil})

  (fact "inspect"
    (let [res "\n{:baum/include {:a :b}, :a :c}\n\n↓ ↓ ↓\n\n{:a :c}\n\n"]
      (with-out-str (rs "#baum/inspect {:baum/include {:a :b} :a :c}"))
      => (str res res))

    (binding [*out* (new java.io.StringWriter)]
      (rs "#baum/inspect {:baum/include {:a :b} :a :c}"))
    => {:a :c})

  (fact "custom reader"
    (rs {:readers {'foo (constantly :foo)}}
        "{:a #foo :b}")
    => {:a :foo})

  (fact "custom reader crated by defreader"
    (c/defreader constantly-foo [v opts]
      :foo)
    (rs {:readers {'foo constantly-foo}}
        "{:a #foo :b}")
    => {:a :foo})

  (fact "custom reader via *data-readers*"
    (binding [*data-readers* {'foo (constantly :foo)}]
      (rs "{:a #foo :b}"))
    => {:a :foo})

  (fact "import with custom reader"
    (rs {:readers {'foo (constantly :foo)}}
        "{:parent #baum/import \"dev-resources/foo.edn\"}")
    => {:parent {:a :foo}}
    (provided
      (slurp "dev-resources/foo.edn") => "{:a #foo :b}"))

  (fact "readers can return nil"
    ;; Normally EDN reader throws an exception if a reader macro
    ;; returns nil.
    (rs {:readers {'void (constantly nil)}}
        "{:a #void :b}") => {:a nil}))


(facts "can read config files with special keys"
  (let [f #(@#'c/reduction % {:reducers (@#'c/default-reducers)})]
    (fact "include - map"
      (f {:baum/include {:a 100
                         :b 200
                         :baum/include {:c 400}}
          :a 200
          :d 500}) => {:a 200 :b 200 :c 400 :d 500})

    (fact "include - list"
      (f {:baum/include {:a 100 :b 200
                         :baum/include [{:c 400 :c2 200}
                                        {:c2 100}]}
          :a 200
          :d 500}) => {:a 200 :b 200 :c 400 :c2 100 :d 500})

    (fact "include - file"
      (f {:baum/include "child.edn"
          :a {:a :b2}})
      => {:a {:a :b2 :c :d}}
      (provided
        (slurp "child.edn") => "{:a {:a :b :c :d}}"))

    (fact "include - multiple files"
      (f {:baum/include ["child.edn" "child2.edn"]
          :a :aa})
      => {:a :aa :b :b2 :c :c}
      (provided
        (slurp "child.edn")  => "{:a :a :b :b :c :c}"
        (slurp "child2.edn") => "{:a :a2 :b :b2}"))

    (fact "include - multiple files and some of them don't exsist"
      (f {:baum/include ["child.edn" "child2.edn"]})
      => (throws java.io.FileNotFoundException))

    (fact "throw an exception when given value to include is invalid"
      (f {:baum/include "invalid-path"}) => (throws java.io.FileNotFoundException)
      (f {:baum/include 100}) => (throws java.lang.IllegalArgumentException))

    (fact "include* - map"
      (f {:baum/include* {:a 100
                          :b 200}
          :a 200})
      => {:a 200 :b 200})

    (fact "include* - file"
      (f {:baum/include* "child.edn"
          :a {:a :b2}})
      => {:a {:a :b2 :c :d}}
      (provided
        (slurp "child.edn") => "{:a {:a :b :c :d}}"))

    (fact "include* - multiple files"
      (f {:baum/include* ["child.edn" "child2.edn"]
          :a :aa})
      => {:a :aa :b :b2 :c :c}
      (provided
        (slurp "child.edn")  => "{:a :a :b :b :c :c}"
        (slurp "child2.edn") => "{:a :a2 :b :b2}"))

    (fact "just ignore invalid value to include*"
      (f {:baum/include* "invalid-path"}) => {}
      (f {:baum/include* 100}) => {})

    (fact "override"
      (f {:baum/override {:a 100
                          :b 200
                          :baum/override {:c 400}}
          :a 200
          :d 500})
      => {:a 100 :b 200 :c 400 :d 500})

    (fact "override - list"
      (f {:baum/override {:a 100
                          :b 200
                          :baum/override [{:c 400 :c2 200}
                                          {:c2 100}]}
          :a 200
          :d 500})
      => {:a 100 :b 200 :c 400 :c2 100 :d 500})

    (fact "override - file"
      (f {:baum/override "child.edn"
          :a {:a :b2 :c :d}})
      => {:a {:a :b :c :d}}
      (provided
        (slurp "child.edn") => "{:a {:a :b :c :d}}"))

    (fact "override - multiple files"
      (f {:baum/override ["child.edn" "child2.edn"]
          :a :aa
          :c :c})
      => {:a :a2 :b :b2 :c :c}
      (provided
        (slurp "child.edn")  => "{:a :a :b :b}"
        (slurp "child2.edn") => "{:a :a2 :b :b2}"))

    (fact "throw an exception when given value to override is invalid"
      (f {:baum/override "invalid-path"}) => (throws java.io.FileNotFoundException)
      (f {:baum/override 100}) => (throws java.lang.IllegalArgumentException))

    (fact "override*"
      (f {:baum/override* {:a 100
                           :b 200
                           :baum/override* {:c 400}}
          :a 200
          :d 500})
      => {:a 100 :b 200 :c 400 :d 500})

    (fact "override* - list"
      (f {:baum/override* {:a 100
                           :b 200
                           :baum/override* [{:c 400 :c2 200}
                                            {:c2 100}]}
          :a 200
          :d 500})
      => {:a 100 :b 200 :c 400 :c2 100 :d 500})

    (fact "override* - file"
      (f {:baum/override* "child.edn"
          :a {:a :b2 :c :d}})
      => {:a {:a :b :c :d}}
      (provided
        (slurp "child.edn") => "{:a {:a :b :c :d}}"))

    (fact "override* - multiple files"
      (f {:baum/override* ["child.edn" "child2.edn"]
          :a :aa
          :c :c})
      => {:a :a2 :b :b2 :c :c}
      (provided
        (slurp "child.edn")  => "{:a :a :b :b}"
        (slurp "child2.edn") => "{:a :a2 :b :b2}"))

    (fact "Returns an empty map when given value to override is invalid"
      (f {:baum/override* "invalid-path"}) => {}
      (f {:baum/override* 100
          :a :b}) => {:a :b})

    (fact "custom special key"
      (@#'c/reduction {:foo/bar :a}
                      {:reducers {:foo/bar (fn [v m opts]
                                             (assoc m :foo/bar :foo))}})
      => {:foo/bar :foo})))

(facts "let+ref"
  (fact "reader macro"
    (rs "{:baum/let [a 100]
          :a        {:c #baum/ref a}}")
    => {:a {:c 100}})

  (fact "destructuring"
    (rs "{:baum/let [{:keys [a b]}  {:a 100 :b 200}]
          :a #baum/ref a
          :b #baum/ref b}")
    => {:a 100 :b 200}

    (rs "{:baum/let [{:keys [a b]}  #baum/import \"a.edn\"]
          :a #baum/ref a
          :b #baum/ref b}")
    => {:a 100 :b 200}
    (provided
      (slurp "a.edn") => "{:a 100 :b 200}"))

  (fact "nested scope"
    (rs "{:baum/let [a :a
                     b :b]
          :d1 {:baum/let [a :d1-a
                          c :d1-c]
               :a #baum/ref a
               :b #baum/ref b
               :c #baum/ref c}
          :a #baum/ref a
          :b #baum/ref b}")
    => {:d1 {:a :d1-a
             :b :b
             :c :d1-c}
        :a :a
        :b :b})

  (fact "global variable"
    (defmethod c/refer-global-variable 'FOO [_] :foo)
    (rs "#baum/ref FOO") => :foo)

  (fact "Throws an exception when referring to undefined variables"
    (rs "{:a #baum/ref a}") => (throws "Unable to resolve symbol: a in this context"))

  (fact "Cannot access variables defined in inner scopes"
    (rs "{:a #baum/ref a
          :b {:baum/let [a 100]}}") => (throws "Unable to resolve symbol: a in this context")))

(facts "aliasiing"
  (fact "custom"
    (rs {:aliases {'baum/env 'env
                   :baum/let '$let
                   'baum/ref '-}}
        "{$let [user #env :user
                loc  \"home\"]
          :who #- user
          :where #baum/ref loc}")
    => {:who "rkworks" :where "home"}
    (provided (env :user) => "rkworks"))

  (fact "built-in"
    (rs {:shorthand? true}
        "{$let [user #env :user
                loc  \"home\"]
          :who #- user
          :where #baum/ref loc}")
    => {:who "rkworks" :where "home"}
    (provided (env :user) => "rkworks")))

(facts "Complex examples"
  (fact ":baum/include + #baum/match"
    (rs "{:a :a
         :baum/include #baum/match [#baum/env :env
                                    \"dev\" {:a :a2 :b :b2}
                                    \"prod\" {:a :a3 :b :b3}]}")
    => {:a :a :b :b3}
    (provided (env :env) => "prod"))

  (fact ":baum/override + #baum/match"
    (rs "{:a :a
         :baum/override #baum/match [#baum/env :env
                                     \"dev\" {:a :a2 :b :b2}
                                     \"prod\" {:a :a3 :b :b3}]}")
    => {:a :a3 :b :b3}
    (provided (env :env) => "prod"))

  (fact "reader macro + bound var"
    (rs "{:baum/let [a \"a.edn\"]
          :foo #baum/import #baum/ref a}")
    => {:foo {:a :b}}
    (provided (slurp "a.edn") => "{:a :b}")

    (rs "{:baum/let [a :user]
         :a #baum/match [#baum/env #baum/ref a
                         \"foo\" true
                         :else false]}")
    => {:a true}
    (provided (env :user) => "foo")

    (rs "{:baum/let [a #baum/match [#baum/env :user
                                   \"foo\" true
                                   :else false]]
         :a #baum/ref a}")
    => {:a true}
    (provided (env :user) => "foo")

    (rs "{:baum/let [user #baum/env :user]
         :foo? #baum/match [#baum/ref user
                            \"foo\" true
                            :else false]}")
    => {:foo? true}
    (provided (env :user) => "foo")


    (rs "{:baum/let [a :user]
          :foo #baum/env #baum/ref a}") => {:foo (env :user)})

  (fact "let chaining"
    (rs "{:baum/let [env       #baum/env :env
                     file-name #baum/str [#baum/ref env \".edn\"]]

          :baum/include [\"default.edn\"
                         #baum/ref file-name]
          }")
    => {:foo :bar :bar :default}
    (provided
      (env :env) => "prod"
      (slurp "default.edn") => "{:foo :default :bar :default}"
      (slurp "prod.edn") => "{:foo :bar}")

    (rs "{:baum/let [env       #baum/env :env
                    file-name  (str #baum/ref env \".edn\")]

         :f #baum/ref file-name
         }")
    => {:f '(str "prod" ".edn")}
    (provided
      (env :env) => "prod")))


#_(facts "Integration"
    (doseq [[in out] (c/read-config (io/resource "whole.edn") {:shorthand? true})]
      (fact
        in => out)))
