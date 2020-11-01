(ns wksymclj.ui.util
  (:require-macros
   [cljs.core.async.macros :as asyncm :refer (go go-loop)])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs.core.async :as async :refer (<! >! put! chan)]
            [taoensso.sente  :as sente :refer (cb-success?)]
            [cljs.nodejs :as nodejs]
            
            [wksymclj.ui.data-renderer :as data-renderer]))

(defn render-json-data-cache! []
  (reagent/render
   (-> js/window
       (aget "JsonDataCache")
       (aget "*")
       (js->clj)
       (data-renderer/clj->hiccup))
   (js/document.getElementById "app")))

;; (-> js/HANDLER_MAP
;;     (js/Object.keys)
;;     (js/console.log))

;; (let [data ]
;;  (reagent/render
;;   [(fn []
;;      [:div
;;       [:h1
;;        "hello!"]])]
;;   (.getElementById js/document "app")))

(-> js/process.env
    (aget "HOME")
    (js/console.log))

(-> js/__dirname
    (js/console.log))


;; (def jsonrpc (nodejs/require "multitransport-jsonrpc"))

;; (def Client (aget jsonrpc "client"))
;; (def ClientHttp (aget jsonrpc "transports" "client" "http"))
;; (def ClientTcp (aget jsonrpc "transports" "client" "tcp"))

;; (def jsonrpc-client
;;   (Client. (ClientHttp.
;;             "localhost"
;;             js/JRPC_PORT
;;             #js {:path js/JRPC_ENDPOINT})))

;; (-> jsonrpc-client
;;     (.request "sibilant" #js ["(console.log (+ 2 1))"]))
