(ns pilcrow.core
  (:require
   [clojure.string :as str]
   [clojure.zip :as zip]))

;; (defn- return [v]
;;   (fn [input] (list [v input])))

;; (defn- failure [_] '())

;; (defn- parse [parser input] (parser input))
;; (defn- parse-all [parser input]
;;   (->> input
;;        (parse parser)
;;        (filter #(= "" (second %)))
;;        ffirst))

;; (defn- >>= [parser f]
;;   (fn [input]
;;     (->> input
;;          (parse parser)
;;          (mapcat (fn [[v tail]] (parse (f v) tail))))))

;; (defn merge-bind [body bind]
;;   (if (and (not= clojure.lang.Symbol (type bind))
;;            (= 3 (count bind))
;;            (= '<- (second bind)))
;;     `(>>= ~(last bind) (fn [~(first bind)] ~body))
;;     `(>>= ~bind (fn [~'_] ~body))))

;; (defmacro do* [& forms]
;;   (reduce merge-bind (last forms) (reverse (butlast forms))))

;; (defn- any [input]
;;   (if (empty? input) '()
;;       (list [(first input)
;;              (apply str (rest input))])))

;; (defn- sat [pred]
;;   (>>= any (fn [v] (if (pred v) (return v) failure))))

;; (defn- char-cmp [f]
;;   (fn [c] (sat (partial f (first c)))))

;; (def match (char-cmp =))
;; (def none-of (char-cmp not=))

;; (declare plus)
;; (declare optional)

;; (defn- and-then [p1 p2]
;;   (do*
;;    (r1 <- p1)
;;    (r2 <- p2)
;;    (return (str r1 r2))))

;; (defn- or-else [p1 p2]
;;   (fn [input]
;;     (lazy-cat (parse p1 input) (parse p2 input))))

;; (defn- many [parser] (optional (plus parser)))

;; (defn- plus [parser]
;;   (do*
;;    (a <- parser)
;;    (as <- (many parser))
;;    (return (cons a as))))

;; (defn- optional [parser] (or-else parser (return "")))


;; (defn string [s] (reduce and-then (map #(match (str %)) s)))

;; (def blank-line (match "\n"))

;; (parse-all (do* (optional (and-then (match "\n") (match "\n")))
;;                 (p <- (many (none-of "\n")))
;;                 (return (apply str p)))
;;            "foo")

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
(def section-char (cset "§#"))

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

(defmethod open :paragraph [_ parent-tag node line]
  (when (and (nil? node) (not-empty line))
    [{:tag :paragraph} line]))

(defmulti line-closes? (fn [[open-tag & c] line] (:tag open-tag)))

(defmethod line-closes? :paragraph [open-node line] (empty? line))
(defmethod line-closes? :default [_ _] false)

(def children {:document [:section]
               :header [:paragraph]
               :section [:section :paragraph]})

(declare line-rules)

(defn- line-opens [root-node open-node line]
  (->> (get children (:tag root-node))
       (map #(open % root-node open-node line))
       (filter identity)
       first))

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



