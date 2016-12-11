(ns pilcrow-test
  (:require
   [clojure.test.check.generators :as gen]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [clojure.zip :as zip]
   [com.gfredericks.test.chuck.clojure-test :refer [checking]]
   [pilcrow.core :as pilcrow]))

(def lorem "Lorem ipsum dolor sit amet")

(def gen-word
  (gen/elements (set (str/split lorem #"\s"))))

(def gen-phrase
  (gen/fmap #(str/join " " %)
            (gen/vector gen-word 1 5)))

(def gen-paragraph
  (gen/fmap (fn [s] {:type :paragraph :children s})
            (gen/vector gen-phrase 1 20)))

(defn gen-some-of [gens]
  (gen/such-that
   #(pos? (count %))
   (gen/fmap
    #(vec (apply concat %))
    (apply gen/tuple (filter identity gens)))
   100))

(def gen-classes
  (gen/hash-map
   ::leading-space gen/boolean
   ::inter-spaces gen/boolean
   :names (gen/set (gen/fmap (comp keyword #(str/join "-" %))
                             (gen/vector gen-word 1 3)))))

(defn gen-block-with-content [level]
  (gen/let [block-type gen-word
            classes gen-classes
            trailing gen/pos-int
            subsec? (gen/frequency [[(- 5 level) gen/boolean]
                                    [5 (gen/return false)]])
            children (gen/vector (gen/frequency
                                  (filterv
                                   identity
                                   [[10 gen-paragraph]
                                    (when (and subsec? (> 4 level))
                                      [3 (gen-block-with-content (inc level))])]))
                                1 5)]
    {:type :block
     :class (:names classes)
     ::class classes
     ::trailing trailing
     :block-type block-type
     :level level
     :children children}))

(defn gen-section-with-content [level]
  (let [msize (max 1 (Math/floor (/ 10 level)))]
    (gen/let [section-name (gen/such-that
                            #(not (contains? #{"header" "footer"}
                                             (str/lower-case %)))
                            gen-word
                            100)
              classes gen-classes
              trailing gen/pos-int
              section-leader (gen/elements pilcrow/section-char)
              ;; mostly here to help prevent gc / heap errors
              subsec (gen/frequency [[msize gen/boolean]
                                     [3 (gen/return false)]])
              children (gen-some-of
                        [(gen/vector
                          (gen/frequency
                            [[10 gen-paragraph]
                             [3 (gen-block-with-content 1)]])
                          0 5)
                         (when (and subsec (> 6 level))
                           (gen/vector
                            (gen-section-with-content (inc level))
                            0 msize))])]
      {:type :section
       :name section-name
       :class (:names classes)
       ::class classes
       ::trailing trailing
       :level level
       :leader section-leader
       :children children})))

(def gen-document
  (gen/fmap
   (fn [[head body]] {:type :document :children (concat [head] body)})
   (gen/let
       [head (gen/one-of
              [(gen/return nil)
               (gen/fmap
                (fn [{:keys [children leader]}]
                  {:type :header
                   :level 1
                   :leader leader
                   :class #{}
                   :name "Header"
                   :children children})
                (gen/hash-map
                 :children (gen/vector gen-paragraph 0 3)
                 :leader (gen/elements pilcrow/section-char)))])
        body (gen/vector (gen-section-with-content 1) 1 10)]
     [head body])))

(declare render-children)

(defn- render-class [classes]
  (str (when (::leading-space classes " "))
       (->> (:names classes)
            (map #(str "." (name %)))
            (str/join (if (::inter-spaces classes) " " "")))))

(defn render-section [section]
  (str (str/join (take (:level section) (repeat (str (:leader section)))))
       (render-class (::class section))
       " " (:name section)
       (when-let [n (::trailing section)]
         (apply str " " (take n (repeat (str (:leader section))))))
       "\n\n"
       (render-children section)))

(defn render-block [block]
  (let [delim (str/join (take (:level block) (repeat "=")))]
    (str "|" delim
         " " (:block-type block)
         (render-class (::class block))
         "\n"
         (render-children block) "\n"
         "|" delim "\n")))

(defn render [node]
  (cond
    (string? node) node

    (map? node)
    (case (:type node)
      :block (render-block node)
      :document (render-children node)
      :header (render-section node)
      :paragraph (str (str/join "\n" (:children node)) "\n\n")
      :section (render-section node)
      (str "\n{unknown node type: " (:type node) "}\n"))))

(defn render-children [{:keys [children]}]
  (->> children (map render) str/join))

(defn remove-ns-keys [m]
  (apply dissoc m (filter namespace (keys m))))

(defn scrub [node]
  (cond (string? node) node
        (map? node)
        (-> (remove-ns-keys node)
            (update :children (fn [c] (->> c (filter identity) (map scrub)))))))

(deftest document
  (checking "parses document" {:num-tests 50}
    [document gen-document]
    (let [rendered (render document)
          scrubbed (scrub document)
          parsed (pilcrow/parse rendered)]
      (is (= scrubbed parsed)
          (str "----\n" rendered "\n----")))))

;; (deftest parsing-errors
;;   (testing "can't skip section levels"
;;     (let [doc "# One\n\nSome Text\n\n### Three\n\nMore Text"
;;           parsed (pilcrow/parse doc)]
;;       (is (= {:error [3 :illegal-section-nesting]}
;;              parsed)))))
