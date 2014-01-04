(ns clojuremud-client 
  (:require [chord.client :refer [ws-ch]]
            [cljs.core.async :refer [<! >! put! close! timeout]]
            [clojure/string :as s]
            [dommy.core :as d]
            clojure.browser.repl)
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [dommy.macros :refer [node sel1]]))

(defn render-page [bind-input! bind-util! bind-list!]
  (node
   (list
    [:div
     [:h3 "Send a message to the server:"]
     (doto (node [:input {:type :text :size 50}])
       bind-input!)]
    [:div
     [:h3 "Send a command to the server:"]
     (doto (node [:input {:type :text :size 50}])
       bind-util!)]
    [:div
     [:h3 "Messages from the server:"]
     (doto (node [:div])
       bind-list!)])))

(defn render-list [msgs]
  (node
   [:ul
    (if (seq msgs)
      (for [msg msgs]
        [:li (pr-str msg)])
      [:li "None yet."])]))

(defn list-binder [msgs]
  (fn [$list]
    (add-watch msgs ::list
               (fn [_ _ _ msgs]
                 (->> msgs
                      (take 10)
                      (render-list)
                      (d/replace-contents! $list))))))

(defn util-binder [ch]
  (fn [$input]
    (d/listen! $input :keyup
               (fn [e]
                 (when (= 13 (.-keyCode e))
                   (let [input (d/value $input)]
                     (put! ch input)
                     (d/set-value! $input "")))))
    (go (<! (.focus $input)))))

(defn input-binder [ch]
  (fn [$input]
    (d/listen! $input :keyup
               (fn [e]
                 (when (= 13 (.-keyCode e))
                   (let [input (s/split (d/value $input) #" ")
                         verb ((comp keyword s/upper-case first) input)
                         obj  (s/join " " (rest input))]
                     (put! ch (pr-str { :action verb :content (str "\"" obj "\"") } ))
                     (d/set-value! $input "")))))
    (go (<! (.focus $input)))))

;;(timeout 200)
(defn bind-msgs [ch msgs]
  (go
   (loop []
     (when-let [msg (<! ch)]
       (swap! msgs conj msg)
       (recur)))))

(set! (.-onload js/window)
      (fn []
        (go
         (let [msgs (atom [])
               ws (<! (ws-ch "ws://localhost:8000/ws"))]
           (bind-msgs ws msgs)
           (d/replace-contents! (sel1 :#content)
                                (render-page (input-binder ws)
                                             (util-binder ws)
                                             (list-binder msgs)))
           (reset! msgs [])))))




