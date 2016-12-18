(ns pilcrow.core
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.zip :as zip]))

(defn remove-empties [{:keys [children] :as node}]
  (if (and (empty? children)
           (nil? (::keep-empty? node)))
    nil
    (dissoc node ::keep-empty?)))

(defn- cset [s] (set (map identity s)))
(def whitespace #{\space \tab 0x202F 0x205F 0x3000 0xA0})
(def section-char (cset "§#="))
(def attr-class-lead #{\.})

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
                (fn? p) (p (first tail))
                (regex? p) (re-find (re-pattern (str "^" (.toString p))) tail)))
            (range 1 (inc (count s))))))
         (mapv #(apply str %)))))

(defn extract-bold [content idx]
  (let [[before after] (split-at idx content)
        close (->> after
                   (drop 2)
                   (partition 2 1)
                   (map (fn [i [c1 c2]] [i (str c1 c2)]) (range))
                   (filter (fn [[i s]] (= "**" s)))
                   ffirst)
        [bold remainder] (split-at (+ 2 close) after)]
    [[(apply str before)
      {:type :bold :children [(apply str (drop 2 bold))]}]
     (apply str (drop 2 remainder))]))

(defn process-text [this-node content idx]
  (let [this-char (.charAt content idx)
        [new-nodes next-content]
        (case this-char
          \* (extract-bold content idx)
          [[] content])
        next-node (update this-node :children #(apply conj % new-nodes))]
    (cond (or (empty? next-content)
              (= (count content) (inc idx)))
          (update this-node :children conj content)
          (= content next-content) (recur this-node content (inc idx))
          :default (recur next-node next-content 0))))

(defn process-text-content [{:keys [content] :as node}]
  (process-text (dissoc node :content)
                (->> content (map second) (str/join "\n"))
                0))



(defn- split-whitespace [s accum]
  (when (contains? whitespace (first s))
    (let [[_ nstr] (split-before s #(not (contains? whitespace %)))]
      [nstr accum])))

(defn- split-classname [s accum]
  (when (contains? attr-class-lead (first s))
    (let [[c nstr] (split-before s (set/union whitespace section-char attr-class-lead))]
      [nstr (update accum :class conj (keyword (apply str (rest c))))])))

(defn- split-title [s accum]
  (when (not (contains? attr-class-lead (first s)))
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

(defmulti open-node (fn [type parent-node active-node line] type))

(defn- section-marker? [line]
  (and (not-empty line)
       (contains? section-char (first line))
       (count (take-while #(= % (first line)) line))))

(defmethod open-node :section
  [_ {:keys [level type] :or {level 0} :as parent-tag} active-node [lno line]]
  (when (and (contains? #{:document :section} type)
             (not-empty line)
             (contains? section-char (first line))
             (every? #(= % (first line)) (take (inc level) line))
             (not (= (first line) (first (drop (inc level) line)))))
    (let [attrs (extract-attrs (apply str (drop (inc level) line)))
          this-type (case [level (str/lower-case (or (:title attrs) ""))]
                      [0 "header"] :header
                      :section)]
      (merge {:type this-type
              :name (str/trim (or (:title attrs) ""))
              :class (:class attrs)
              :leader (first line)
              :level (inc level)
              :children []
              :content []}
             (when (contains? #{:header} this-type)
               {::keep-empty? true})))))

(defn- block-marker? [level line]
  (and level
       (not-empty line)
       (contains? (cset "|") (.charAt line 0))
       (every? #(contains? (cset "=") %) (take level (drop 1 line)))))

(defmethod open-node :block
  [_ {:keys [level type] :as parent-node} active-node [_ line]]
  (let [block-level (if (= :block type) (inc level) 1)]
    (when-let [[_ block-type attrstr]
               (and (block-marker? block-level line)
                    (contains? whitespace (first (drop (inc block-level) line)))
                    (re-matches #"\|=+\s+([-\p{L}]+)(.*)" line))]
      (let [attrs (extract-attrs attrstr)]
        {:type :block
          :block-type block-type
          :level block-level
          :class (:class attrs)
          :children []
          :content []
          ::keep-empty? true}))))

(defmethod open-node :paragraph [_ parent-node active-node [lno lco :as line]]
  (when (and (nil? active-node) (not-empty lco))
    {:type :paragraph :children [] :content [line]}))


(def children {:block #{:block :paragraph}
               :document #{:section}
               :header #{:block :paragraph}
               :section #{:section :block :paragraph}})

(def closing-siblings
  {:block #{:block}
   :header #{:section}
   :section #{:section}})

(defmulti line-closes-node? (fn [open-child line] (:type open-child)))
(defmethod line-closes-node? :default [_ _] false)
(defmethod line-closes-node? :paragraph [_ line] (empty? line))
(defmethod line-closes-node? :block [{:keys [level] :as block} line]
  (and (block-marker? level line)
       (let [etc (drop (inc level) line)]
         (empty? etc))))

(defn line-opens-node [parent-node active-node [lno lco :as line]]
  (when (not-empty lco)
    (let [possibles (if active-node
                      (get closing-siblings (:type active-node))
                      (get children (:type parent-node)))]
      (->> possibles
           (map #(open-node % parent-node active-node line))
           (filter identity)
           first))))

(declare process-node-content)

(defn close-node [node]
  (when (and node (:type node))
    (if (contains? #{:paragraph} (:type node))
      (process-text-content node)
      (process-node-content node))))

(defn process-node-line
  [{:keys [children active-node]
    :as parent-node}
   [[line-no line-content :as line] & lines]]
  (let [[next-active append-children]
        (or (when (line-closes-node? active-node line-content)
              [nil active-node])
            (when-let [next-child
                       (line-opens-node parent-node active-node line)]
              [next-child active-node])
            (when active-node
              [(update active-node :content conj line)])
            [])
        next-parent (merge parent-node
                           {:active-node next-active}
                           (when append-children
                             {:children (conj children append-children)}))]
    (if (empty? lines)
      (-> next-parent
          (update :children conj (:active-node next-parent))
          (update :children #(map close-node %))
          (update :children #(filter identity %))
          (dissoc :active-node :content)
          remove-empties)
      (recur next-parent lines))))

(defn process-node-content [{:keys [content] :as node}]
  (process-node-line (dissoc node :content) content))

(defn parse--old [input]
  (process-node-content
   {:type :document
    :children []
    :content (map #(vector %1 %2) (map inc (range)) (str/split-lines input))}))

;; ===
(defn read-line [input]
  (str/split input #"\n" 2))

(defn gather-parents
  ([zipper] (gather-parents zipper []))
  ([zipper parents]
   (let [next-parents (conj parents zipper)
         next-node (zip/up zipper)]
     (if (zip/up next-node)
       (recur next-node next-parents)
       next-parents))))

(defn section-marker? [line]
  (when (contains? section-char (first line))
    (let [marker (take-while #(= (first line) %) line)
          level (count marker)
          attrs (extract-attrs (apply str (drop level line)))
          this-type (case [(count marker) (:title attrs)]
                      [0 "header"] :header
                      :section)]
      [(merge {:type this-type
               :name (str/trim (or (:title attrs) ""))
               :level level
               :class (:class attrs)
               :leader (first marker)}
              (when (contains? #{:header} this-type)
                {::keep-empty? true}))
       nil])))

(defmulti close-node (fn [node] (-> node first :type)))
(defmethod close-node :default [node] (filterv not-empty node))

(defn line-open-marker [line]
  (or (section-marker? line)))

(defn closes-sibling? [active new]
  (and (contains? (get closing-siblings (:type active))
                  (:type new))
       (if-let [level (:level active)]
         (= level (:level new))
         true)))

(defn opens-child? [active new]
  (and (contains? (get children (:type active))
                  (:type new))
       (if-let [level (:level active)]
         (= (inc level) (:level new))
         true)))

(defn insert-node [zipper node]
  (as-> zipper $
    (if (zip/node $)
      (-> $ (zip/insert-right node) zip/next)
      (-> $ (zip/replace node)))
    (if (vector? node)
      (-> $ zip/next zip/rightmost)
      $)))

(defn check-parents [zipper line]
  (let [new-node (line-open-marker line)
        new-node-info (first new-node)]
    (->> (gather-parents zipper)
         (map (fn [node]
                (let [info (-> node zip/leftmost zip/node)]
                  (cond
                    (opens-child? info new-node-info)
                    (insert-node node new-node)))))
         (filter identity)
         first)))

(defn open-new-node-from-line [zipper input info]
  (let [[line rest-input] (read-line input)]
    (or (when-let [new-loc (check-parents zipper line)]
          [new-loc rest-input (update info :line inc)])
        (when (and (nil? (zip/node zipper))
                   (not-empty line)
                   (not= :paragraph (-> zipper zip/leftmost zip/node :type)))
          [(insert-node zipper [{:type :paragraph} nil]) input info])
        (when (and (empty? line)
                   (= :paragraph (-> zipper zip/leftmost zip/node :type)))
          [(-> zipper zip/up (zip/insert-right nil) zip/right)
           rest-input
           (update info :line inc)]))))

(defn close-doc [zipper]
  (let [next-doc (zip/edit zipper close-node)]
    (if-let [parent (zip/up next-doc)]
      (recur parent)
      next-doc)))

(defn process-input [zipper input info]
  (let [[new-state new-input new-info]
        (or (when (:at-start info)
              (open-new-node-from-line zipper input info))
            (let [[this-line rest-lines] (read-line input)]
              (if (not-empty this-line)
                [(insert-node zipper this-line) rest-lines (update info :line inc)]
                [zipper rest-lines (update info :line inc)])))]
    (if (empty? new-input)
      (close-doc (zip/up new-state))
      (recur new-state new-input new-info))))

(defn parse [input]
  (-> [{:type :document} nil]
      zip/vector-zip
      zip/next zip/next
      (process-input input {:line 1 :at-start true})
      zip/root))

