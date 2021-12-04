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

(defn inspector-dispatch [{:keys [obj width height]}]
  (if (or (<= width 0)
          (<= height 0))
    :no-space
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
      (instance? clojure.lang.IDeref obj) :deref
      :else :object)))


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
      (vec
       (reverse
        (loop [partitions []
               width width]
          (cond
            (zero? width) partitions

            (>= (inc (count partitions))
                n)
            (conj partitions width)

            :else (recur (conj partitions chunk-size)
                         (- width chunk-size)))))))))

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

(defmethod inspector* :no-space
  [{:keys [obj width height]}]
  nil)

(defmethod inspector* :string
  [{:keys [obj width height]}]
  (ui/with-color (:string colors)
    (ilabel (pr-str obj)
            width)))

(defn wrap-selection [x path elem]
  (ui/wrap-on
   :mouse-down
   (fn [handler pos]
     (let [intents (handler pos)]
       (if (seq intents)
         intents
         [[::select x path]])))
   elem))

(defn wrap-highlight [path highlight-path elem]
  (let [body
        (ui/wrap-on
         :mouse-move
         (fn [handler pos]
           (let [intents (handler pos)]
             (if (seq intents)
               intents
               [[::highlight path]])))
         elem)]
   (if (= path highlight-path)
     (ui/fill-bordered [0.2 0.2 0.2 0.1]
                       0
                       body)
     body)))

(defn inspector-seq-horizontal [{:keys [obj
                                        width
                                        height
                                        highlight-path
                                        path
                                        offset
                                        open close]}]
  (let [open-close-width (+ (count open)
                            (count close))]
    (when (> width open-close-width )
      (let [body
            (loop [body []
                   i 0
                   width (- width
                            (count open)
                            (count close))
                   obj (seq obj)]
              (if (or (not obj)
                      (<= width 0))
                body
                (let [x (first obj)
                      child-path (if (map-entry? x)
                                   (list 'find (key x))
                                   (list 'nth i))
                      path (conj path
                                 child-path)
                      elem
                      (wrap-highlight
                       path
                       highlight-path
                       (wrap-selection
                        x
                        path
                        (inspector* {:obj x
                                     :height 1
                                     :highlight-path highlight-path
                                     :path path
                                     :width width})))
                      pix-width (ui/width elem)
                      elem-width (int (Math/ceil (/ pix-width
                                                    cell-width)))]
                  (recur (conj body elem)
                         (inc i)
                         (- width elem-width
                            ;; add a space between elements
                            1
                            )
                         (next obj)))))]
        (when (pos? (count body))
          (ui/horizontal-layout
           (ui/with-color (:bracket colors)
             (ilabel open (count open)))
           (apply ui/horizontal-layout
                  (interpose (indent 1)
                             body))
           (when (= (count body)
                    (bounded-count (inc (count body)) obj ))
            (ui/with-color (:bracket colors)
              (ilabel close (count close)))))))))
  )

(def chunk-size 32)
(defn inspector-seq [{:keys [obj
                             width
                             height
                             highlight-path
                             path
                             offset
                             open close]
                      :as m}]
  (let [offset (or offset 0)]
    (if (<= height 3)
      (inspector-seq-horizontal m)
      (let [obj (if (pos? offset)
                  (drop offset obj)
                  obj)
            heights (split-evenly (- height 3)
                                  (min chunk-size (bounded-count (inc chunk-size) obj)))
            children (->> obj
                          (map (fn [i height obj]
                                 (let [child-path (if (map-entry? obj)
                                                    (list 'find (key obj))
                                                    (list 'nth i))
                                       path (conj path
                                                  child-path)
                                       body
                                       (wrap-highlight
                                        path
                                        highlight-path
                                        (wrap-selection obj
                                                        path
                                                        (inspector* {:obj obj
                                                                     :height height
                                                                     :path path
                                                                     :highlight-path highlight-path
                                                                     :width (dec width)})))]
                                   (if (= path highlight-path)
                                     (ui/fill-bordered [0.2 0.2 0.2 0.1]
                                                       0
                                                       body)
                                     body)))
                               (range)
                               heights)
                          (apply ui/vertical-layout))]
        (ui/vertical-layout
         (ui/with-color (:bracket colors)
           (ilabel open width))
         (ui/translate cell-width 0
                       (ui/vertical-layout
                        (when (pos? offset)
                          (ui/on
                           :mouse-down
                           (fn [_]
                             ;; only for top level
                             (when (empty? path)
                               [[::previous-chunk]]))
                           (ilabel "..." 3)))
                        children
                        (let [len (bounded-count (inc chunk-size) obj)]
                         (when (or (> len chunk-size)
                                   (< (count heights)
                                      (bounded-count (inc chunk-size) obj)))
                           (ui/on
                            :mouse-down
                            (fn [_]
                              ;; only for top level
                              (when (empty? path)
                                [[::next-chunk (min (count heights) chunk-size)]]))
                            (ilabel "..." 3))))))
         (ui/with-color (:bracket colors)
           (ilabel close width)))))))


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


(defn inspector-map-entry [{:keys [obj width height path highlight-path]}]
  (let [[left right] (split-ratio (- width 2) 1/3)
        [k v] obj]
    (ui/horizontal-layout
     (let [child-path (conj path '(key))]
      (wrap-highlight
       child-path
       highlight-path
       (wrap-selection k
                       child-path
                       (inspector* {:obj k
                                    :height height
                                    :path child-path
                                    :highlight-path highlight-path
                                    :width left}))))
     (indent 1)
     (let [child-path (conj path '(val))]
      (wrap-highlight
       child-path
       highlight-path
       (wrap-selection v
                       child-path
                       (inspector* {:obj v
                                    :height height
                                    :path child-path
                                    :highlight-path highlight-path
                                    :width right})))))))
(defmethod inspector* :map-entry
  [{:keys [obj width height] :as m}]
  (inspector-map-entry m))

(defn inspector-deref [{:keys [obj width height path highlight-path]}]
  (let [[left right] (split-ratio (- width 2) 1/3)
        k (symbol (.getName (class obj)))
        v (if (instance? clojure.lang.IPending obj)
            (if (realized? obj)
              @obj
              "unrealized?")
            (deref obj))]
    (ui/horizontal-layout
     (indent 1)
     (inspector* {:obj k
                  :height height
                  :width left})
     (indent 1)
     (let [child-path (conj path '(deref))]
       (wrap-highlight
        child-path
        highlight-path
        (wrap-selection v
                        child-path
                        (inspector* {:obj v
                                     :height height
                                     :path child-path
                                     :highlight-path highlight-path
                                     :width right})))))))
(defmethod inspector* :deref
  [{:keys [obj width height] :as m}]
  (inspector-deref m))


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



(defui inspector [{:keys [obj width height stack path highlight-path offsets]}]
  (let [stack (or stack [])
        path (or path [])
        offsets (or offsets [0])
        offset (peek offsets)]

    (ui/padding
     4
     (ui/vertical-layout
      (basic/button {:text "pop"
                     :on-click
                     (fn []
                       (when (seq stack)
                         (let [{:keys [obj path offsets]} (peek stack)]
                           [[:set $obj obj]
                            [:set $path path]
                            [:set $offsets offsets]
                            [:update $stack pop]])))})
      (ui/label (str "offset: " offset))
      (ui/label (str "path: " (pr-str path) ))
      (ui/on
       ::highlight
       (fn [path]
         [[:set $highlight-path path]])
       ::previous-chunk
       (fn []
         [[:update $offsets
           (fn [offsets]
             (if (> (count offsets) 1)
               (pop offsets)
               offsets))]])
       ::next-chunk
       (fn [delta]
         [[:update $offsets
           (fn [offsets]
             (let [offset (peek offsets)]
               (conj offsets (+ offset delta))))]])

       ::select
       (fn [x child-path]
         [[:update $stack conj {:obj obj
                                :path path
                                :offsets offsets}]
          [:delete $highlight-path]
          [:update $path into child-path]
          [:set $offsets [0]]
          [:set $obj (wrap x)]])
       (ui/wrap-on
        :mouse-move
        (fn [handler pos]
          (let [intents (handler pos)]
            (if (seq intents)
              intents
              [[:set $highlight-path nil]])))
        (inspector* {:obj (-unwrap obj)
                     :height height
                     :path []
                     :offset offset
                     :highlight-path highlight-path
                     :width width} )))))))






(s/def ::anything any? )
(comment
  (do
    (def obj (gen/generate (s/gen ::anything) )
      )
    (inspect (gen/sample  (s/gen ::anything)
                       100)
          )
    obj)

  (backend/run #'inspector-test)
  ,)

  
(comment
  (require '[pl.danieljanus.tagsoup :as tagsoup])
  (def data (json/read-str (slurp "https://raw.githubusercontent.com/dreadwarrior/ext-giftcertificates/5e447a7316aea57a372203f2aa8de5aef3af671a/ExtensionBuilder.json")))

  (def data (tagsoup/parse-string (slurp "/Users/adrian/workspace/pretty-view/index.html") ))

  (inspect (tagsoup/parse-string (slurp "https://clojure.org/reference/reader")))

  data
  (inspect data)
  ,
)

(comment
  (def a (atom nil))
  (def b (atom a))
  (reset! a b)
  (inspect a)
  ,)
