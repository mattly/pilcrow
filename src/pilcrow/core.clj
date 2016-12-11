(ns pilcrow.core
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [clojure.set :as set]))

;; ===============
(defrecord Either [left right])
(defn left [err] (Either. err nil))
(defn right [val] (Either. nil val))
(defn right? [either] (nil? (:left either)))
(def left? (comp not right?))
(def right-value :right)
(def left-value :left)

(defn bind [v f]
  (if (left? v) v (-> v right-value f)))

(defn fmap [v f]
  (if (left? v) v (-> v right-value f right)))
(defn lift [v]
  (if (right? v) (right-value v) (left-value v)))

(defn branch [v fl fr]
  (if (left? v) (-> v left-value fl) (-> v right-value fr)))
(defn branch-left [v fl]
  (if (left? v) (-> v left-value fl) v))
;; =================

(defn remove-empties [{:keys [children] :as node}]
  (if (and (empty? children)
           (nil? (::keep-empty? node)))
    nil
    (dissoc node ::keep-empty?)))

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

(defmulti open-node (fn [type parent-node active-node line] type))

(defmethod open-node :section
  [_ {:keys [level type] :or {level 0} :as parent-tag} active-node [_ line]]
  (when (and (not-empty line)
             (contains? #{:document :section} type)
             (contains? section-char (first line))
             (every? #(= (first line) %) (take (inc level) line))
             (not (contains? section-char (.charAt line (inc level)))))
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
                    (re-matches #"\|=+\s+([-\w]+)(.*)" line))]
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
      (-> node
          (assoc :children (map second (:content node)))
          (dissoc :content))
      (process-node-content node))))

(defn process-node-line
  [{:keys [children active-node content]
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

(defn parse [input]
  (process-node-content
   {:type :document
    :children []
    :content (map #(vector %1 %2) (map inc (range)) (str/split-lines input))}))

