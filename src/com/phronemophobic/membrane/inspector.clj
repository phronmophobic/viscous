(ns com.phronemophobic.membrane.inspector
  (:require
   [membrane.ui :as ui]
   [membrane.skia :as backend]
   [membrane.basic-components :as basic]
   [membrane.component :refer [defui defeffect]
    :as component]
   [clojure.zip :as z]
   [clojure.data.json :as json]

   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]))

(defprotocol PWrapped
  (-unwrap [this]))

(defn wrap [o]
  (reify
    Object
    (hashCode [_] (System/identityHashCode o))
    PWrapped
    (-unwrap [_] o)))


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

(def monospaced (ui/font "Menlo" 11))
(def cell-width (backend/font-advance-x monospaced " "))

(defn indent [n]
  (ui/spacer (* n cell-width) 0))

(defn inspector-dispatch [{:keys [obj]}]
  (cond
    (string? obj) :string
    (integer? obj) :integer
    (float? obj) :float
    (double? obj) :double
    (char? obj) :char
    (map-entry? obj) :map-entry
    (map? obj) :map
    (set? obj) :set
    (list? obj) :list
    (vector? obj) :vector
    (symbol? obj) :symbol
    (keyword? obj) :keyword
    (boolean? obj) :boolean
    (nil? obj) :nil

    (coll? obj) :collection
    (seqable? obj) :seqable
    :else :object))


(defmulti inspector* inspector-dispatch)

(defn ilabel [o width]
  (let [s (str o)
        len (count s)
        shortened
        (when (pos? len)
          (if (<= len width)
            s
            (case width
              0 nil
              (1 2 3) (subs s 0 (min len width))

              ;; else
              (str (subs s 0 (max 0
                                  (- width 3)))
                   "..."))))]
    (when shortened
      (ui/label shortened monospaced))))

(defn split-evenly [width n]
  (if (zero? n)
    []
    (let [chunk-size (max 1
                          (int (/ width n)))]
      (loop [partitions []
             width width]
        (cond
          (zero? width) partitions

          (>= (inc (count partitions))
              n)
          (conj partitions width)

          :else (recur (conj partitions chunk-size)
                       (- width chunk-size)))))))

(defn split-ratio [width r]
  (let [left (int (Math/ceil (* r width)))
        right (- width left)]
    [left right]))

(defmethod inspector* :default
  [{:keys [obj width height]}]
  (let [[left right] (split-ratio width 1/3)]
    (ui/horizontal-layout
     (ilabel (inspector-dispatch {:obj obj})
             left)
     (ilabel (type obj)
             right))))

(defmethod inspector* :string
  [{:keys [obj width height]}]
  (ui/with-color (:string colors)
    (ilabel (str "\"" obj "\"")
            width)))

(defn wrap-selection [x elem]
  (ui/wrap-on
   :mouse-down
   (fn [handler pos]
     (let [intents (handler pos)]
       (if (seq intents)
         intents
         [[::select x]])))
   elem))

(defn inspector-seq [{:keys [obj width height
                              open close]}]
  (if (< height 3)
    (ilabel (str open "..." close) width)
    (let [heights (split-evenly (- height 3)
                                (min 32 (bounded-count 33 obj)))
          children (->> obj
                        (map (fn [height obj]
                               (wrap-selection obj
                                               (inspector* {:obj obj
                                                            :height height
                                                            :width (dec width)})))
                             heights)
                        (apply ui/vertical-layout))]
      (ui/vertical-layout
       (ui/with-color (:bracket colors)
         (ilabel open width))
       (ui/translate cell-width 0
                     (ui/vertical-layout
                      children
                      (when (or (> (bounded-count 33 obj) 32)
                                (< (count heights)
                                   (bounded-count 33 obj)))
                        (ilabel "..." 3))))
       (ui/with-color (:bracket colors)
         (ilabel close width))))))


(defmethod inspector* :vector
  [{:keys [obj width height] :as m}]
  (inspector-seq (assoc m
                        :open "["
                        :close "]")))

(defmethod inspector* :seqable
  [{:keys [obj width height] :as m}]
  (inspector-seq (assoc m
                        :open "("
                        :close ")")))

(defmethod inspector* :collection
  [{:keys [obj width height] :as m}]
  (inspector-seq (assoc m
                        :open "("
                        :close ")")))

(defmethod inspector* :list
  [{:keys [obj width height] :as m}]
  (inspector-seq (assoc m
                        :open "("
                        :close ")")))

(defmethod inspector* :set
  [{:keys [obj width height] :as m}]
  (inspector-seq (assoc m
                        :open "#{"
                        :close "}")))



(defmethod inspector* :map
  [{:keys [obj width height] :as m}]
  (inspector-seq (assoc m
                        :open "{"
                        :close "}"))
  )

(defn inspector-keyword [{:keys [obj width height]}]
  (let [ns (namespace obj)
        [left right] (if ns
                       (split-ratio (- width 2) 1/3)
                       [0 (- width 1)])]
    (ui/with-color (:keyword colors)
      (ui/horizontal-layout
       (ilabel ":" 1)
       (when ns
         (ui/horizontal-layout
          (ilabel ns left)
          (ilabel "/" 1)))
       (ilabel (name obj) right)))))
(defmethod inspector* :keyword
  [{:keys [obj width height] :as m}]
  (inspector-keyword m))


(defn inspector-map-entry [{:keys [obj width height]}]
  (let [[left right] (split-ratio (- width 2) 1/3)
        [k v] obj]
    (ui/horizontal-layout
     (indent 1)
     (wrap-selection k
                     (inspector* {:obj k
                                  :height height
                                  :width left}))
     (indent 1)
     (wrap-selection v
                     (inspector* {:obj v
                                  :height height
                                  :width right})))))
(defmethod inspector* :map-entry
  [{:keys [obj width height] :as m}]
  (inspector-map-entry m))


(defn inspector-symbol [{:keys [obj width height]}]
  (let [ns (namespace obj)
        [left right] (if ns
                       (split-ratio (- width 1) 1/3)
                       [0 width])]
    (ui/with-color (:qualifier colors)
      (ui/horizontal-layout
       (when ns
         (ui/horizontal-layout
          (ilabel ns left)
          (ilabel "/" 1)))
       (ilabel (name obj) right)))))
(defmethod inspector* :symbol
  [{:keys [obj width height] :as m}]
  (inspector-symbol m))


(defn inspector-integer [{:keys [obj width height]}]
  (ui/with-color (:number colors)
    (ilabel obj width)))
(defmethod inspector* :integer
  [{:keys [obj width height] :as m}]
  (inspector-integer m))

(defn inspector-float [{:keys [obj width height]}]
  (ui/with-color (:number colors)
    (ilabel obj width)))
(defmethod inspector* :float
  [{:keys [obj width height] :as m}]
  (inspector-float m))

(defn inspector-double [{:keys [obj width height]}]
  (ui/with-color (:number colors)
    (ilabel obj width)))
(defmethod inspector* :double
  [{:keys [obj width height] :as m}]
  (inspector-double m))


(defn inspector-char [{:keys [obj width height]}]
  (ui/with-color (:string colors)
    (ui/horizontal-layout
     (ilabel "\\" 1)
     (ilabel obj (dec width)))))
(defmethod inspector* :char
  [{:keys [obj width height] :as m}]
  (inspector-char m))

(defn inspector-boolean [{:keys [obj width height]}]
  (ui/with-color (:number colors)
    (ui/horizontal-layout
     (ilabel obj width))))
(defmethod inspector* :boolean
  [{:keys [obj width height] :as m}]
  (inspector-boolean m))

(defn inspector-nil [{:keys [obj width height]}]
  (ilabel "nil" width))
(defmethod inspector* :nil
  [{:keys [obj width height] :as m}]
  (inspector-nil m))



(defui inspector [{:keys [obj width height stack]}]
  (let [stack (or stack [])]
    (ui/vertical-layout
     (basic/button {:text "pop"
                    :on-click
                    (fn []
                      (when (seq stack)
                        [[:set $obj (peek stack)]
                         [:update $stack pop]]))})
     (ui/on
      ::select
      (fn [x]
        [[:update $stack conj obj]
         [:set $obj (wrap x)]])
      (inspector* {:obj (-unwrap obj)
                   :height height
                   :width width} )))))





(defn show [obj]
  (backend/run (component/make-app #'inspector
                                   {:obj (wrap obj)
                                    :width 80
                                    :height 50}))
  )


(s/def ::anything any? )
(comment
  (do
    (def obj (gen/generate (s/gen ::anything) )
      )
    (show obj)
    obj)

  (backend/run #'inspector-test)
  ,)



