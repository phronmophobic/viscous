(ns com.phronemophobic.viscous.demo
  (:require
   [membrane.ui :as ui]
   [goog.net.XhrIo :as xhr]
               clojure.edn
   [membrane.basic-components :as basic]
   [com.phronemophobic.viscous :as viscous]
   [membrane.webgl :as webgl]
   [membrane.component :refer [defui defeffect]
    :as component]))


(defn $ [id]
  (js/document.getElementById id))

(def blob-area ($ "blobarea"))
(def update-btn ($ "update-btn"))
(def url-input ($ "url-input"))
(def fetch-example-select ($ "fetch-example-select"))
(def fetch-btn ($ "fetch-btn"))
(def fetch-example-btn ($ "fetch-example-btn"))

(def obj {:a {:b 42}})
(def width 80)
(def height 40)
(defonce repaint nil)
(defonce demo-state (atom {:obj (viscous/wrap obj)
                           :width 80
                           :height 40
                           :show-context? true}))
(defn update-viscous [obj err]
  (swap! demo-state
         (fn [state]
           (-> state
               (assoc :obj (viscous/wrap obj))
               (dissoc :membrane.component/extra))))
  (repaint))

(defn parse-edn-or-json [s]
  (try
    [nil (clojure.edn/read-string {:default (fn [tag x]
                                              (prn "readign tag" tag x)
                                              x)}
                                  s)]
    (catch js/Object edn-error
      (prn edn-error)
      (try
        [nil (js/JSON.parse s)]
        (catch js/Object json-error
          (prn json-error)
          [[edn-error json-error]
           nil])))))

(defonce button-listen (.addEventListener
                        update-btn
                        "click"
                        (fn []
                          (let [blob (.-value blob-area)
                                [errs obj] (parse-edn-or-json blob)]
                            (update-viscous obj errs)))))



(defonce fetch-listen (.addEventListener
                       fetch-btn
                       "click"
                       (fn []
                         (let [url (.-value url-input)]
                           (xhr/send url
                                     (fn [e]
                                       (let [x (.-target e)
                                             [errs obj] (parse-edn-or-json (.getResponseText ^js x))]
                                         (update-viscous obj errs))))))))

(defonce fetch-example-listen (.addEventListener
                               fetch-example-btn
                               "click"
                               (fn []
                                 (let [url (.-value fetch-example-select)]
                                   (xhr/send url
                                             (fn [e]
                                               (let [x (.-target e)
                                                     [errs obj] (parse-edn-or-json (.getResponseText ^js x))]
                                                 (update-viscous obj errs))))))))



(def canvas (.getElementById js/document "canvas"))
(defn -main []
  (webgl/load-font
   (:name viscous/monospaced)
   "https://fonts.googleapis.com/css2?family=Ubuntu+Mono&display=swap"
   "https://fonts.gstatic.com/s/ubuntumono/v6/EgeuS9OtEmA0y_JRo03MQaCWcynf_cDxXwCLxiixG1c.ttf"

   (fn []
     (let [freetype-font (webgl/get-font viscous/monospaced)
           space-glyph (-> freetype-font
                           (.-glyphs)
                           (.-glyphs)
                           (aget 3))
           font-size (:size ui/default-font)
           fscale (membrane.webgl/font-scale freetype-font font-size)
           advance (* fscale (.-advanceWidth space-glyph))]
       (set! viscous/cell-width advance)
       (set! viscous/cell-height (membrane.webgl/font-line-height viscous/monospaced)))
     
     #_(js/console.log (-> (.-fonts js/document)
                         (.load (str 
                                 (:size ui/default-font) "px"
                                 " "
                                 "\"Ubuntu Mono\""
                                 ;;(:name ui/default-font)
                                 ))))

     (let [ 

           app (component/make-app #'viscous/inspector
                                   demo-state)

           [empty-width empty-height] (ui/bounds ((component/make-app #'viscous/inspector
                                                                      {:obj (viscous/wrap nil)
                                                                       :width 0
                                                                       :height 0})))
           window-width (max empty-width
                             (* viscous/cell-width width))
           window-height (+ empty-height
                            height
                            (* viscous/cell-height (inc height)))]
       (defonce canvas-info (membrane.webgl/run
                              app
                              {:container canvas}))
       (set! repaint (:membrane.webgl/repaint canvas-info))))))

