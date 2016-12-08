(ns pilcrow-test
  (:require
   [clojure.test.check.generators :as gen]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [clojure.zip :as zip]
   [com.gfredericks.test.chuck.clojure-test :refer [checking]]
   [pilcrow.core :as pilcrow]))

;; (def gen-word
;;   (gen/such-that
;;    #(and (< 0 (count %)) (> 30 (count %)))
;;    gen/string-alphanumeric
;;    100))

(def lorem "Lorem ipsum dolor sit amet")

(def gen-word
  (gen/elements (set (str/split lorem #"\s"))))

(def gen-sentence
  (gen/fmap #(str (str/join " " %) ".")
            (gen/vector gen-word 1 40)))

(def gen-paragraph
  (gen/fmap (fn [s] [{:tag :paragraph} (str/join " " s)])
            (gen/vector gen-sentence 1 20)))

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
            content (gen/vector (gen/frequency
                                 (filterv
                                  identity
                                  [[10 gen-paragraph]
                                   (when (and subsec? (> 4 level))
                                     [3 (gen-block-with-content (inc level))])]))
                                1 5)]
    (into [{:tag :block
            :class (:names classes)
            ::class classes
            ::trailing trailing
            :type block-type
            :level level}]
          content)))

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
              content (gen-some-of [(gen/vector (gen/frequency [[10 gen-paragraph]
                                                                [3 (gen-block-with-content 1)]])
                                                0 5)
                                    (when (and subsec (> 6 level))
                                      (gen/vector (gen-section-with-content
                                                   (inc level)) 0 msize))])]
      (into [{:tag :section
              :name section-name
              :class (:names classes)
              ::class classes
              ::trailing trailing
              :level level
              :leader section-leader}]
            content))))

(def gen-document
  (gen/fmap
   #(into [{:tag :document :level 0}] %)
   (gen/fmap
    #(apply concat %)
    (gen/let
        [head (gen/one-of
               [(gen/return nil)
                (gen/fmap
                 #(vector
                   (into [{:tag :header
                           :level 1
                           :leader (:leader %)
                           :class #{}
                           :name "Header"}]
                         (:content %)))
                 (gen/hash-map
                  :content (gen/vector gen-paragraph 0 3)
                  :leader (gen/elements pilcrow/section-char)))])
         body (gen/vector (gen-section-with-content 1) 1 10)]
      [head body]))))

(declare render)

(defn- render-class [classes]
  (str (when (::leading-space classes " "))
       (->> (:names classes)
            (map #(str "." (name %)))
            (str/join (if (::inter-spaces classes) " " "")))))

(defn render-section [attrs content]
  (str (str/join (take (:level attrs) (repeat (str (:leader attrs)))))
       (render-class (::class attrs))
       " " (:name attrs)
       (when-let [n (::trailing attrs)]
         (apply str " " (take n (repeat (str (:leader attrs))))))
       "\n\n"
       (render content)))

(defn render-block [{:keys [level type] :as attrs} content]
  (let [delim (str/join (take level (repeat "=")))]
    (str "|" delim
         " " type
         (render-class (::class attrs))
         "\n"
         (render content) "\n"
         "|" delim "\n")))

(defn render-node [node]
  (cond
    (string? node) node

    (vector? node)
    (let [attrs (first node)
          content (rest node)]
      (case (:tag attrs)
        :block (render-block attrs content)
        :document (render content)
        :header (render-section attrs content)
        :paragraph (str (str/join content) "\n\n")
        :section (render-section attrs content)
        (str "\n{unknown tag: " attrs "}\n")))))

(defn remove-ns-keys [m]
  (apply dissoc m (filter namespace (keys m))))

(defn scrub-node [n]
  (if (zip/end? n)
    n
    (let [this-node (zip/node n)]
      (cond (map? this-node) (-> n
                                 (zip/edit remove-ns-keys)
                                 zip/next
                                 recur)
            :else (recur (zip/next n))))))

(defn scrub [doc]
  (-> (zip/vector-zip doc)
      scrub-node
      zip/root))


(defn render [nodes]
  (str/join (map render-node nodes)))

(deftest document
  (checking "parses document" {:num-tests 50}
    [document gen-document]
    (let [rendered (render document)
          scrubbed (scrub document)
          parsed (pilcrow/parse rendered)]
      (is (= scrubbed parsed)
          (str "----\n" rendered "\n----")))))
