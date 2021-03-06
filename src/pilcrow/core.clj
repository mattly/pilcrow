(ns pilcrow.core
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.zip :as zip]))

; == here through "working" is reference
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
    [(apply str before)
     [{:type :bold} (apply str (drop 2 bold))]
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


(defn process-paragraph []
  (process-text ))

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

(def children {:block #{:block :paragraph}
               :document #{:section :header}
               :header #{:block :paragraph}
               :section #{:section :block :paragraph}})

(def closing-siblings
  {:block #{:block}
   :header #{:section}
   :section #{:section}})

(defn read-line
  "Given an input string, returns a tuple of the contents before the first
  newline and the contents after it."
  [input]
  (str/split input #"\n" 2))

(comment
 (read-line "Foo\n\nBar\n\nBee\nBoo"))

(defn gather-parents
  "Returns each parent up the tree from the current node."
  ([zipper] (gather-parents zipper []))
  ([zipper parents]
   (let [next-parents (conj parents zipper)
         next-node (zip/up zipper)]
     (if (zip/up next-node)
       (recur next-node next-parents)
       next-parents))))

(defn read-section-marker [line]
  (when (contains? section-char (first line))
    (let [marker (take-while #(= (first line) %) line)
          level (count marker)
          attrs (extract-attrs (apply str (drop level line)))
          title (str/trim (or (:title attrs) ""))
          this-type (case [level (str/lower-case title)]
                      [1 "header"] :header
                      :section)]
      [(merge {:type this-type
               :name title
               :level level
               :class (:class attrs)
               :leader (first marker)}
              (when (contains? #{:header} this-type)
                {::keep-empty? true}))
       nil])))

(defn read-block-marker [line]
  (when (and (= \| (first line)) (= \= (second line)))
    (let [marker (->> line (drop 1) (take-while #(= (second line) %)))
          level (count marker)
          attrs (extract-attrs (apply str (drop (inc level) line)))
          block-type (str/trim (:title attrs))]
      [{:type :block
        :block-type block-type
        :level level
        :class (:class attrs)}
       nil])))

(defn read-block-open-marker [line]
  (when-let [[info :as marker] (read-block-marker line)]
    (when (not-empty (:block-type info)) marker)))

(defn read-block-close-marker [line]
  (when-let [[info :as marker] (read-block-marker line)]
    (when (and (empty? (:block-type info)) (empty? (:class info)))
      marker)))

(defn line-open-marker
  "If this line is an opening marker, returns a node."
  [line]
  (or (read-section-marker line)
      (read-block-open-marker line)))

(line-open-marker "|== block")

(defn line-close-marker
  [line]
  (or (read-block-close-marker line)))

(defn closes-sibling? [active new]
  (and (contains? (get closing-siblings (:type active))
                  (:type new))
       (if-let [level (:level active)]
         (= level (:level new))
         true)))

(defn opens-child?
  [{active-type :type active-level :level}
   {new-type :type new-level :level}]
  (and (contains? (get children active-type) new-type)
       (cond
         (= active-type new-type) (= (inc active-level) new-level)
         :default (= 1 new-level))))

(defn insert-node
  "Inserts `node` at the end of `zipper`"
  [zipper node]
  (as-> zipper $
    (if (zip/node $)
      (-> $ (zip/insert-right node) zip/right)
      (-> $ (zip/replace node)))
    (if (vector? node)
      (-> $ zip/down zip/rightmost)
      $)))

(defn close-node [node]
  (-> node zip/up (zip/insert-right nil) zip/right))

(defn check-parents
  "If the current line opens a new node, checks to see if that node closes
  its previous sibling or is a child of the new node. If not, ignore it.

  TODO it should probably raise a parser error if the new node is not a valid
  child."
  [zipper line]
  (or
   (when-let [new-node (line-open-marker line)]
     (->> (gather-parents zipper)
          (map (fn [node]
                 (let [info (-> node zip/leftmost zip/node)]
                   (cond
                     (closes-sibling? info (first new-node))
                     (-> node zip/up (insert-node new-node))

                     (opens-child? info (first new-node))
                     (insert-node node new-node)))))
          (filter identity)
          first))
   (when-let [close (line-close-marker line)]
     (->> (gather-parents zipper)
          (map (fn [node]

                 (when
                   (closes-sibling? (-> node zip/leftmost zip/node) (first close))
                   (-> node zip/up (zip/insert-right nil) zip/right))))
          (filter identity)
          first))))

(defn open-new-node-from-line
  "If the line opens a new node, move there. If we're not in a node, opens a
  paragraph. If we're in a paragraph and line is empty, closes it."
  [zipper input info]
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

(defn blank-node?
  "Checks if node is blank or not."
  [node]
  (and (vector? node)
       (= 1 (count node))
       (not (::keep-empty? (first node)))))

(defn remove-empty-node
  "Removes nils from a node and returns nil for an empty node."
  [node']
  (let [node (->> node'
                  butlast
                  (filterv identity))]
    (if (blank-node? node)
      nil
      (update node 0 dissoc ::keep-empty?))))

(defn scrub-node
  "Walks the entire zipper and removes empty nodes and parsing markers."
  [zipper]
  (let [this-node (zip/node zipper)
        next-node
        (cond
          (vector? this-node)
          (-> zipper
              (zip/append-child ::node-end)
              zip/down)

          (= this-node ::node-end)
          (let [this (zip/edit (zip/up zipper) remove-empty-node)]
            (or (zip/right this)
                (zip/next this)))

          :default (zip/next zipper))]
    (if (zip/end? next-node)
      next-node
      (recur next-node))))

(defn goto-root
  "Goes to the root of the zipper."
  [zipper]
  (if-let [parent (zip/up zipper)]
    (recur parent)
    zipper))

(defn process-input
  "The main loop. Reads lines from `input` and puts them into `zipper`."
  [zipper input info]
  (let [[new-state new-input new-info]
        (or (when (:at-start info)
              (open-new-node-from-line zipper input info))
            (let [[this-line rest-lines] (read-line input)]
              (if (not-empty this-line)
                [(insert-node zipper this-line) rest-lines (update info :line inc)]
                [zipper rest-lines (update info :line inc)])))]
    (if (empty? new-input)
      (-> new-state goto-root zip/next scrub-node)
      (recur new-state new-input new-info))))

(defn parse [input]
  (-> [{:type :document} nil]
      zip/vector-zip
      zip/next zip/next
      (process-input input {:line 1 :at-start true})
      zip/root))

(comment
 (parse "Some preface conten

# Section One

This is some **section** content.
This is a second line in the first paragraph.

|= block
block contents
|=

Post block contents
"))
