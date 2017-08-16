(ns pilcrow-test
  (:require
   [clojure.test.check.generators :as gen]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [clojure.zip :as zip]
   [com.gfredericks.test.chuck.clojure-test :refer [checking]]
   [pilcrow.core :as pilcrow]))

(def lorem "Lorem ipsum dolor sit amet ümlaut")

(def gen-word
  (gen/elements (set (str/split lorem #"\s"))))

(defn gen-inword-span [span-type]
  (gen/let [word gen-word
            start (gen/choose 1 (- (count word) 2))
            close (gen/choose (inc start) (- (count word) 1))]
    (let [[head tail] (split-at close word)
          [pre content] (split-at start head)]
      [(apply str pre)
       [{:type span-type} (apply str content)]
       (apply str tail)])))

(defn- str-accum [[this-in & rest-in] out]
  (let [newout
        (cond (and (string? this-in) (string? (first out)))
              (conj (rest out) (str (first out) " " this-in))

              (string? this-in)
              (conj out this-in)

              (vector? this-in)
              (conj out this-in))]
    (if (empty? rest-in)
      (reverse newout)
      (recur rest-in newout))))

(def gen-phrase
  (gen/fmap #(str-accum (flatten %) '())
            (gen/vector
             (gen/frequency [[10 gen-word]
                             #_[10 (gen-inword-span :bold)]])
             1 10)))

(defn gen-span-phrase [span-type]
  (gen/fmap (fn [words]
              [{:type span-type} (str/join " " words)])
            (gen/vector gen-word 1 5)))

(def gen-paragraph
  (gen/fmap (fn [s]
              (into [{:type :paragraph}] (str-accum (flatten s) '())))
            (gen/vector
             (gen/frequency [[10 gen-phrase]
                             #_[1 (gen-span-phrase :bold)]])
             1 10)))

(defn gen-some-of [gens]
  (gen/such-that
   #(pos? (count %))
   (gen/fmap
    #(vec (apply concat %))
    (apply gen/tuple (filter identity gens)))
   100))

(def gen-classes
  (gen/hash-map
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
    (into [{:type :block
            :class (:names classes)
            ::class classes
            ::trailing trailing
            :block-type block-type
            :level level}]
          children)))

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
      (into [{:type :section
              :name section-name
              :class (:names classes)
              ::class classes
              ::trailing trailing
              :level level
              :leader section-leader}]
            children))))

(def gen-document
  (gen/fmap
   (fn [[head body]] (into [{:type :document} head] body))
   (gen/let
       [head (gen/one-of
              [(gen/return nil)
               (gen/fmap
                (fn [{:keys [children leader]}]
                  (into
                   [{:type :header
                     :level 1
                     :leader leader
                     :class #{}
                     :name "Header"}]
                   children))
                (gen/hash-map
                 :children (gen/vector gen-paragraph 0 3)
                 :leader (gen/elements pilcrow/section-char)))])
        body (gen/vector (gen-section-with-content 1) 1 10)]
     [head body])))

(declare render-children)

(defn- render-class [classes]
  (str " "
       (->> (:names classes)
            (map #(str "." (name %)))
            (str/join (if (::inter-spaces classes) " " "")))))

(defn render-section [[section & children :as node]]
  (str (str/join (take (:level section) (repeat (str (:leader section)))))
       (render-class (::class section))
       " " (:name section)
       (when-let [n (::trailing section)]
         (apply str " " (take n (repeat (str (:leader section))))))
       "\n\n"
       (render-children node)))

(defn render-block [[block & children :as node]]
  (let [delim (str/join (take (:level block) (repeat "=")))]
    (str "|" delim
         (render-class (::class block))
         " " (:block-type block) "\n"
         (render-children node) "\n"
         "|" delim "\n")))

(defn render [node]
  (cond
    (nil? node) ""
    (string? node) node

    (vector? node)
    (case (:type (first node))
      :bold (str "**" (str/join (drop 1 node)) "**")
      :block (render-block node)
      :document (render-children node)
      :header (render-section node)
      :paragraph (str (render-children node) "\n\n")
      :section (render-section node)
      (str "\n{unknown node type: " (:type (first node)) "}\n"))))

(defn render-children [[info & children]]
  (->> children (map render) str/join))


(defn remove-ns-keys [m]
  (apply dissoc m (filter namespace (keys m))))

(defn scrub [node]
  (cond (string? node) node
        (vector? node)
        (let [[info & children] node]
          (into [(remove-ns-keys info)]
                (->> children (filter identity) (map scrub))))))

(deftest document
  (checking "parses document" {:num-tests 50}
    [document gen-document]
    (let [rendered (render document)
          scrubbed (scrub document)
          parsed (pilcrow/parse rendered)]
      (is (= scrubbed parsed)
          (str "----\n" rendered "\n----")))))
