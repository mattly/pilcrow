(ns pilcrow.core
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))


(defn empty-tag? [[tag & content]]
  (and (empty? content)
       (nil? (:keep-empty? tag))))

(defn remove-empties [[tag & content]]
  (if (and (empty? content)
           (nil? (:keep-empty? tag)))
    nil
    (into [(dissoc tag :keep-empty?)] content)))

(defn- cset [s] (set (map identity s)))
(def whitespace #{\space \tab 0x202F 0x205F 0x3000 0xA0})
(def section-char (cset "§#="))

(defmulti open (fn [type parent-tag node line] type))

(defmethod open :section [_ parent-tag node line]
  (when (and (not-empty line)
             (contains? #{:document :section} (:tag parent-tag))
             (every? #(contains? section-char %) (take (inc (:level parent-tag)) line))
             (contains? whitespace (.charAt line (inc (:level parent-tag)))))
    (let [sn (re-find #"\w[^=]*" line)
          tag (case [(:level parent-tag) (str/lower-case sn)]
                [0 "header"] :header
                :section)]
      [(merge {:tag tag
               :name (str/trim (or sn ""))
               :leader (first line)
               :level (inc (:level parent-tag))}
              (when (contains? #{:header} tag)
                {:keep-empty? true}))
       nil])))

(defn- block-marker? [level line]
  (and level
       (not-empty line)
       (contains? (cset "|") (.charAt line 0))
       (every? #(contains? (cset "=") %) (take level (drop 1 line)))))

(defmethod open :block [_ parent-tag node line]
  (let [block-level (if (= :block (:tag parent-tag)) (inc (:level parent-tag)) 1)]
    (when-let [block-type (and (block-marker? block-level line)
                               (contains? whitespace (first (drop (inc block-level) line)))
                               (second (re-matches #"\|=+\s+([-\w]+)" line)))]
      [{:tag :block :type block-type :level block-level :keep-empty? true} nil])))

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

