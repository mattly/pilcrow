(set-env!
 :source-paths #{"src" "test"}
 :resource-paths #{"resources"}
 :dependencies '[[org.clojure/clojure "1.9.0-alpha14"]

                 [adzerk/boot-test "1.1.2" :scope "test"]
                 [org.clojure/test.check "0.9.0" :scope "test"]
                 [com.gfredericks/test.chuck "0.2.7" :scope "test"]

                 [hiccup "1.0.5"]
                 [perun "0.4.0-SNAPSHOT"]])

(require '[adzerk.boot-test :refer [test]]
         '[io.perun :refer :all])

(deftask build-dev
  "Build dev version"
  []
  (comp (global-metadata :filename "meta.edn")
        (print-meta)))
