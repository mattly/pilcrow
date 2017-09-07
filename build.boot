(set-env!
 :source-paths #{"src" "test"}
 :dependencies '[[org.clojure/clojure "1.9.0-alpha17"]

                 [adzerk/boot-test "1.2.0" :scope "test"]
                ;  [org.clojure/test.check "0.9.0" :scope "test"]
                 [org.clojure/test.check "0.10.0-alpha2" :scope "test"]
                 [com.gfredericks/test.chuck "0.2.7" :scope "test"]

                 [proto-repl "0.3.1"]

                 [hiccup "1.0.5"]])

(require '[adzerk.boot-test :refer [test]])
