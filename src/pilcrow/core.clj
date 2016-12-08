(ns pilcrow.core
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(defn remove-empties [[tag & content]]
  (if (and (empty? content)
           (nil? (::keep-empty? tag)))
    nil
    (into [(dissoc tag ::keep-empty?)] content)))

(defn- cset [s] (set (map identity s)))
(def whitespace #{\space \tab 0x202F 0x205F 0x3000 0xA0})
(def section-char (cset "§#="))

(defn- regex? [re]
  (= java.util.regex.Pattern (type re)))

(defn split-before [s p]
  (if (= 1 (count s))
    [s ""]
    (->> s
         (split-at
          (first
           (filter
            #(let [tail (apply str (drop % s))]
              (cond
                (empty? tail) true
                (string? p) (str/starts-with? tail p)
                (set? p) (contains? p (first tail))
                (regex? p) (re-find (re-pattern (str "^" (.toString p))) tail)))
            (range 1 (inc (count s))))))
         (mapv #(apply str %)))))

(defn- split-whitespace [s accum]
  (when (contains? whitespace (first s))
    (let [[_ nstr] (split-before s #"[^\s]")]
      [nstr accum])))

(defn- split-classname [s accum]
  (when (= \. (first s))
    (let [[c nstr] (split-before s #"[^-\w]")]
      [nstr (update accum :class conj (keyword (apply str (rest c))))])))

(defn- split-title [s accum]
  (when (re-find #"[a-zA-Z0-9]" s)
    ["" (assoc accum :title (str/replace s #"[\s=#§]*$" ""))]))

(defn- extract-attrs
  ([attrstr] (extract-attrs attrstr {:class #{} :title ""}))
  ([attrstr accum]
   (if (empty? attrstr)
     accum
     (let [[nextstr attrs] (or (split-whitespace attrstr accum)
                               (split-classname attrstr accum)
                               (split-title attrstr accum))]
       (recur nextstr attrs)))))

(defmulti open (fn [type parent-tag node line] type))

(defmethod open :section [_ parent-tag node line]
  (when (and (not-empty line)
             (contains? #{:document :section} (:tag parent-tag))
             (every? #(contains? section-char %) (take (inc (:level parent-tag)) line))
             (not (contains? section-char (.charAt line (inc (:level parent-tag))))))
    (let [attrs (extract-attrs (apply str (drop (inc (:level parent-tag)) line)))
          tag (case [(:level parent-tag) (str/lower-case (or (:title attrs) ""))]
                [0 "header"] :header
                :section)]
      [(merge {:tag tag
               :name (str/trim (or (:title attrs) ""))
               :class (:class attrs)
               :leader (first line)
               :level (inc (:level parent-tag))}
              (when (contains? #{:header} tag)
                {::keep-empty? true}))
       nil])))

(defn- block-marker? [level line]
  (and level
       (not-empty line)
       (contains? (cset "|") (.charAt line 0))
       (every? #(contains? (cset "=") %) (take level (drop 1 line)))))

(defmethod open :block [_ parent-tag node line]
  (let [block-level (if (= :block (:tag parent-tag)) (inc (:level parent-tag)) 1)]
    (when-let [[_ block-type attrstr]
               (and (block-marker? block-level line)
                    (contains? whitespace (first (drop (inc block-level) line)))
                    (re-matches #"\|=+\s+([-\w]+)(.*)" line))]
      (let [attrs (extract-attrs attrstr)]
        [{:tag :block
          :type block-type
          :level block-level
          :class (:class attrs)
          ::keep-empty? true} nil]))))

(defmethod open :paragraph [_ parent-tag node line]
  (when (and (nil? node) (not-empty line))
    [{:tag :paragraph} line]))

(defmulti line-closes? (fn [[open-tag & c] line] (:tag open-tag)))

(defmethod line-closes? :block [[open-tag & c] line]
  (and (block-marker? (:level open-tag) line)
       (let [etc (drop (inc (:level open-tag)) line)]
         (or (empty? etc)
             (contains? whitespace etc)))))

(defmethod line-closes? :paragraph [open-node line] (empty? line))
(defmethod line-closes? :default [_ _] false)

(def children {:block #{:block :paragraph}
               :document #{:section}
               :header #{:block :paragraph}
               :section #{:section :block :paragraph}})

(def siblings {:block #{:block}
               :header #{:section}
               :section #{:section}})

(declare line-rules)

(defn- line-opens [root-node open-node line]
  (let [open-siblings (get siblings (:tag (first open-node)))
        root-children (get children (:tag root-node))
        possible-siblings (if open-siblings
                            (set/intersection open-siblings root-children)
                            root-children)]
    (->> possible-siblings
         (map #(open % root-node open-node line))
         (filter identity)
         first)))

(defn close-node [this-node next-node]
  (cond
    (vector? this-node)
    (let [[node & content] this-node]
      [(case (:tag node)
         :paragraph [node (str/join " " content)]
         (line-rules this-node))
       next-node])

    (nil? this-node)
    [next-node]

    :else [this-node next-node]))

(defn line-rules [[root-node & lines :as root]]
  (let [last-line-no (- (count lines) 1)]
    (->> lines
         (reduce
          (fn [[node-info & children :as parent] line]
            (let [this-node (last children)
                  head (into [node-info] (butlast children))]
              (or (when (line-closes? this-node line)
                    (apply conj head (close-node this-node nil)))
                  (when-let [next-node (line-opens root-node this-node line)]
                    (apply conj head (close-node this-node next-node)))
                  (when this-node
                    (conj head (conj this-node line)))
                  parent)))
          [root-node])
         ((fn [nodes]
            (let [head (vec (butlast nodes))
                  tail (last nodes)]
              (into head (close-node tail nil)))))
         (filterv identity)
         remove-empties)))

(defn split-chunks
  [input]
  (->> input
       str/split-lines
       (into [{:tag :document :level 0} [{:tag :section :level 1 :name "Preamble"}]])
       line-rules))

(defn parse [input]
  (split-chunks input))

