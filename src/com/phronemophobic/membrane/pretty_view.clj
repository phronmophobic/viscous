(ns com.phronemophobic.membrane.pretty-view
  (:require [membrane.ui :as ui
             :refer [IChildren -children
                     IBounds -bounds bounds]]
            [membrane.skia :as backend]
            [fipp.edn :as fipp]

             [clojure.string :as str]
             [fipp.ednize]
            [fipp.visit :refer [visit visit*]])
  (:import com.github.davidmoten.rtree.RTree
           com.github.davidmoten.rtree.Entries
           com.github.davidmoten.rtree.geometry.Geometries
           ))


(def colors
  {:keyword [0.46666666865348816 0.0 0.5333333611488342],
   :number
   [0.06666667014360428 0.4000000059604645 0.2666666805744171],
   :def [0.0 0.0 1.0],
   :positive
   [0.13333334028720856 0.6000000238418579 0.13333334028720856],
   :bracket
   [0.6000000238418579 0.6000000238418579 0.46666666865348816],
   :comment [0.6666666865348816 0.3333333432674408 0.0],
   :attribute [0.0 0.0 0.800000011920929],
   :type [0.0 0.5333333611488342 0.3333333432674408],
   :quote [0.0 0.6000000238418579 0.0],
   :header [0.0 0.0 1.0],
   :atom
   [0.13333334028720856 0.06666667014360428 0.6000000238418579],
   :builtin [0.20000000298023224 0.0 0.6666666865348816],
   :hr [0.6000000238418579 0.6000000238418579 0.6000000238418579],
   :string-2 [1.0 0.3333333432674408 0.0],
   :string
   [0.6666666865348816 0.06666667014360428 0.06666667014360428],
   :meta [0.3333333432674408 0.3333333432674408 0.3333333432674408],
   :tag [0.06666667014360428 0.46666666865348816 0.0],
   :qualifier
   [0.3333333432674408 0.3333333432674408 0.3333333432674408],
   :variable-2 [0.0 0.3333333432674408 0.6666666865348816]})



(defrecord Text [color s]
  ui/IOrigin
  (-origin [this] (ui/origin (ui/children this)))
  IBounds
  (-bounds [this] (bounds (ui/children this)))
  IChildren
  (-children [_]
    [(ui/with-color (get colors color [0 0 0])
       (ui/label
        s
        (ui/font "Menlo.ttc" 11)))]))
(defrecord Group [xs]
  ui/IOrigin
  (-origin [this] (ui/origin (ui/children this)))
  IBounds
  (-bounds [this] (bounds (ui/children this)))
  IChildren
  (-children [_]
    [(apply ui/vertical-layout xs)]))
(defrecord Span [xs]
  ui/IOrigin
  (-origin [this] (ui/origin (ui/children this)))
  IBounds
  (-bounds [this] (bounds (ui/children this)))
  IChildren
  (-children [_]
    [(apply ui/horizontal-layout xs)]))
(defrecord Align [xs]
  ui/IOrigin
  (-origin [this] (ui/origin (ui/children this)))
  IBounds
  (-bounds [this] (bounds (ui/children this)))
  IChildren
  (-children [_]
    [(apply ui/vertical-layout xs)]))
(defrecord Line []
  ui/IOrigin
  (-origin [this] (ui/origin (ui/children this)))
  IBounds
  (-bounds [this] (bounds (ui/children this)))
  IChildren
  (-children [_]
    [(ui/spacer 7.0 0)])
  )
(defrecord Space []
  ui/IOrigin
  (-origin [this] (ui/origin (ui/children this)))
  IBounds
  (-bounds [this] (bounds (ui/children this)))
  IChildren
  (-children [_]
    [(ui/spacer 7.0 0)]))

(defn pretty-coll [{:keys [print-level print-length] :as printer}
                   open xs sep close f]
  (let [printer (cond-> printer print-level (update :print-level dec))
        xform (comp (if print-length (take print-length) identity)
                    (map #(f printer %))
                    (interpose sep))
        ys (if (pos? (or print-level 1))
             (sequence xform xs)
             "#")
        ellipsis (when (and print-length (seq (drop print-length xs)))
                   (Span. [sep "..."]))]
    (Group. [(Span. [(Text. :bracket  open)
                     (Align. (if ellipsis
                               (concat ys [ellipsis])
                               ys))])
             
             (Text. :bracket close)])))



(defrecord EdnPrinter [symbols print-meta print-length print-level]

  fipp.visit/IVisitor


  (visit-unknown [this x]
    (visit this (fipp.ednize/edn x)))


  (visit-nil [this]
    (Text. :atom  "nil")) []

  (visit-boolean [this x]
    (Text. :atom  (str x)))

  (visit-string [this x]
    (Text. :string 
           (binding [*print-readably* true]
             (pr-str x))))

  (visit-character [this x]
    (Text. :string-2 
           (binding [*print-readably* true]
             (pr-str x))))

  (visit-symbol [this x]
    (Text. :variable 
           (str x)))

  (visit-keyword [this x]
    (Text. :keyword 
           (str x)))

  (visit-number [this x]
    (binding [*print-dup* false]
      (Text. :number  (pr-str x))))

  (visit-seq [this x]
    (if-let [pretty (symbols (first x))]
      (pretty this x)
      (pretty-coll this "(" x (Line.) ")" visit)))

  (visit-vector [this x]
    (pretty-coll this "[" x (Line.) "]" visit))

  (visit-map [this x]
    (pretty-coll this "{" x (Line.) "}"
                 (fn [printer [k v]]
                   (Span. [(visit printer k) (Space.) (visit printer v)]))))

  (visit-set [this x]
    (pretty-coll this "#{" x (Line.) "}" visit))

  (visit-tagged [this {:keys [tag form]}]
    (Group.
     ["#" (str tag)
      (when (or (and print-meta (meta form))
                (not (coll? form)))
        " ")
      (visit this form)]))


  (visit-meta [this m x]
    (if print-meta
      (Align. (Span.[ "^" (visit this m)])
              (Line.) (visit* this x))
      (visit* this x)))

  (visit-var [this x]
    (Text. :variable  (str x)))

  (visit-pattern [this x]
    (Text. :string  (pr-str x))
)

  (visit-record [this x]
    (visit this (fipp.ednize/record->tagged x)))

  )

(defn pretty
  ([x] (pretty x {}))
  ([x options]
   (let [defaults {:symbols {}
                   :print-length *print-length*
                   :print-level *print-level*
                   :print-meta *print-meta*}
         printer (map->EdnPrinter (merge defaults options))]
     (binding [*print-meta* false]
       (visit printer x)))))


(defn show [v]
  (let [view (pretty v)]
    (backend/run (constantly view))))
