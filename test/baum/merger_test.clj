(ns baum.merger-test
  (:require [baum.merger :as sut]
            [midje.sweet :refer :all]))

(facts "merge-tree"
  (fact "Maps will be deeply merged"
    (sut/merge-tree {:a {:b :c :d :e}}
                    {:a {:b :d}})
    =>
    {:a {:b :d :d :e}})

  (fact "Collections will NOT be concatenated by default"
    (sut/merge-tree [:a :b] [:c :d])
    =>
    [:c :d])

  (facts "By adding metadata, collections will be concatenated"
    (fact "To concatenate collections, use :append"
      (reduce sut/merge-tree [[:a :b]
                              [:c :d]
                              ^:append [:e :f]])
      =>
      [:c :d :e :f]

      (reduce sut/merge-tree [[:a :b]
                              ^:append [:c :d]
                              [:e :f]])
      =>
      [:e :f]

      (reduce sut/merge-tree [^:append [:a :b] ; nothing happens
                              [:c :d]])
      =>
      [:c :d])

    (fact ":prepend"
      (reduce sut/merge-tree [[:a :b]
                              [:c :d]
                              ^:prepend [:e :f]])
      =>
      [:e :f :c :d]

      (reduce sut/merge-tree [[:a :b]
                              ^:prepend [:c :d]
                              [:e :f]])
      =>
      [:e :f]

      (reduce sut/merge-tree [^:prepend [:a :b]
                              [:c :d]
                              [:e :f]])
      =>
      [:e :f]))

  (fact "Sets will NOT be united by default"
    (sut/merge-tree #{:a :b} #{:c :d})
    =>
    #{:c :d})

  (fact "The right one's type should be respected."
    (sut/merge-tree #{:c :d} ^:append (sorted-set :a :b)) => sorted?

    (sut/merge-tree (sorted-set :a :b) ^:append #{:c :d}) =not=> sorted?)

  (fact "other collection types will be concatenated"
    (sut/merge-tree '(:a :b) ^:append [:c :d]) => (every-checker [:a :b :c :d] vector?))

  (fact "nil handling"
    (sut/merge-tree nil 1) => 1
    (sut/merge-tree 1 nil) => nil
    (sut/merge-tree {:a :b} nil) => nil)

  (fact "displace"
    (reduce sut/merge-tree [^:displace {:a :b}
                            {:c :d}
                            {:e :f}]) => {:c :d :e :f}

    (reduce sut/merge-tree [{:a :b}
                            ^:displace {:c :d}
                            {:e :f}]) => {:e :f}

    (reduce sut/merge-tree [{:a :b}
                            {:c :d}
                            ^:displace {:e :f}]) => {:a :b :c :d}

    (reduce sut/merge-tree [^:displace {:c :d}
                            ^:displace {:e :f}]) => {:c :d})

  (fact "replace"
    (reduce sut/merge-tree [{:a :b}
                            ^:replace {:c :d}
                            {:e :f}]) => {:c :d :e :f}

    (reduce sut/merge-tree [^:replace {:a :b}
                            {:c :d}
                            {:e :f}]) => {:a :b :c :d :e :f}

    (reduce sut/merge-tree [{:a :b}
                            {:c :d}
                            ^:replace {:e :f}]) => {:e :f}

    (reduce sut/merge-tree [^:replace {:c :d}
                            ^:replace {:e :f}]) => {:e :f})


  (fact "edge cases"
    (sut/merge-tree ^:displace [:a :b]
                    ^:append [:c :d]) => [:c :d]

    ;; prioritized-merge wins
    (sut/merge-tree [:a :b]
                    ^:displace ^:append [:c :d]) => [:a :b]))
