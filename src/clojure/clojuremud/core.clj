(ns clojuremud.core 
  (:require [clojuremud.eval :as eval]
            [ring.util.response :refer [response]]
            [compojure.core :refer [defroutes GET]]
            [compojure.route :refer [resources]]
            [chord.http-kit :refer [with-channel]]
            [clojure.core.async :refer [<! >! put! close! go-loop mult tap untap chan]]
            [clojure.core.match :refer [match]]
            [hiccup.page :refer [html5 include-js]]
            [frodo :refer [repl-connect-js]]
            [clojure.tools.logging :refer [info error]]
            [clojail.core :refer [sandbox]]
            [clojail.testers :refer [secure-tester]]
            [serializable.fn :as s]))


(def not-nil? (complement nil?))

(defn page-frame []
  (html5
   [:head
    [:title "Chord Example"]
    (include-js "/js/clojuremud-client.js")]
   [:body2
    [:div#content]
    [:script (repl-connect-js)]]))

;;
;; execution
;; 

;;  This implicitly includes the params subject, room and cmdline into
;;  the parameters of the verb. This should contain everything the
;;  verb needs to execute
(defmacro defverb [name doc body]
  `{~name (vary-meta (s/fn ~'[subject room cmdline] ~body) assoc :doc ~doc )})

(defmacro defverb-global [verbname doc & body]
  `(swap! db assoc-in  [:global-verbs] (defverb ~verbname ~doc ~@body)))

(defmacro defverb-object [objtype objname verbname doc & body]
  `(swap! db assoc-in [objtype objname :verbs] (defverb ~verbname ~doc ~@body)))

(defn eval-verb [verb])

(defn with-ns [ns form]
         (let [cur *ns*]
           (try ; needed to prevent failures in the eval code from skipping ns rollback
             (in-ns ns)   
             (eval form)
             (finally 
               (in-ns (ns-name cur))))))

(defn with-ns-str [ns str]
  (with-ns (read-string str)))

;;
;; state
;;

(def global-sb (eval/make-sandbox "global"))

(def db (atom  { :users { "root" : {:username "root"
                                    :room "Home"
                                    :inventory {}
                                    :equipment {}
                                    :sandbox nil
                                    :channel nil
                                    :sesskey nil } }
                 :conx  { } ;; ws to username index
                 :rooms { "Home" : {:roomname "Home"
                                    :shortdesc "You find yourself in a bizarre town square"
                                    :desc "This is a placeholder for a clever description"
                                    :type "room"
                                    :channel nil  
                                    :items {}
                                    :users {} 
                                    :verbs {}}}
                :items {}
                :global-verbs {}
                :connections {} }))

(defn selcert-sandbox [name]
  (if-let [sb (get-in db [:users name :sandbox])]
    sb
    (let [sb (eval/make-sandbox name)]
      (swap! db assoc-in [:users name :sandbox] sb )
       sb)))

(defn channel-user [username] (get-in @db [:users username :channel]))
(defn channel-room [roomname] (get-in @db [:roomss roomnamename :channel]))

;; Commo
  
(def global-chat (chan))
(def gmult (mult global-chat))
(def rooms (atom { :1 (chan) :2 (chan) }))
(def rmult (atom (into {} (for [[k v] @rooms] [k (mult v)])))))

(defn add-connection [ws] 
  (swap! conx #(assoc % ws { :name nil :session nil })))

(defn random-string [length]
  (let [ascii-codes (concat (range 48 58) (range 66 91) (range 97 123))]
    (apply str (repeatedly length #(char (rand-nth ascii-codes))))))

(defn gen-session-key [] (random-string 20))

(defn do-login [ws msg]
  (let [username (msg :loginUsername)
        sesskey (gen-session-key) 
        newmap { :name username :session sesskey } ]
          (swap! conx #( assoc % ws newmap ))
          newmap))

(defn do-logout [ws msg]
  (swap! conx #( dissoc % ws)))

(defn ws-handler [req]
  (with-channel req ws
    (add-connection ws)
    (tap gmult ws)
    (println @conx)
    (println "Opened connection from" (str req))
    (go-loop []
             (let [msg (-> ws <! :message read-string)
                   resp (dispatch msg (get-in @db [:users (get-in @db [:conx ws])]))]
               (info (class msg) msg)
               (info resp)
               (respond response)
               (recur)))))

(defroutes app-routes
  (GET "/" [] (response (page-frame)))
  (GET "/ws" [] ws-handler)
  (resources "/js" {:root "js"}))

(def app
  #'app-routes)

;; TODO resurrect this when I decide to get multi-verb chaining
;; working. 
;; expects a structure like
;; {:users {"root" {:items {"a" {:verbs {"c1" 30, "c2" 31, "c3" 32, "a1" 1, "a2" 2, "a3" 3}}, 
;;                          "b" {:verbs {"c1" 50, "b1" 11, "c2" 51, "b2" 12, "c3" 52, "b3" 13}}}}}}
;; (defn dispatch
;;   "This locates the room from the user's map.
;;    Then it selects the verb from the request line passed in by the user
;;    It then searches the users items, room and global contexts for verbs
;;    that match, it then compiles a verb chain of the matching verbs"
;;   [request username]
;;   (let [words (split request #" ")
;;         verb (first words)
;;         lmerge (partial merge-with list)
;;         room (get-in @db [:users username :room])
;;         eq-verbs (as-> (get-in @db [:users username :equipment])
;;                        x (vals x) (mapcat vals x)
;;                        (lmerge x) (get x verb))
;;         item-verbs (as-> (get-in @db [:rooms room :items])
;;                          x (vals x) (mapcat vals x)
;;                          (lmerge x) (get x verb))
;;         room-verbs (get-in @db [:rooms room :verbs verb])
;;         global-verbs (get @global-verbs verb)
;;         resolved ((comp flatten filter) '(global-verbs room-verbs item-verbs eq-verbs))
;;         verbchain (apply comp resolved)
;;         result (verbchain {:user user :room room :request request})])
;;   (prn result)
;;   ; apply mutation to @db
;;   )

(defn dispatch
  "This locates the room from the user's map.
   Then it selects the verb from the request line passed in by the user
   It then searches the users items, room and global contexts for verbs
   that match, it then compiles a verb chain of the matching verbs"
  [request username]
  (let [words (split request #" ")
        verb (first words)
        lmerge (partial merge-with list)
        room (get-in @db [:users username :room])
        eq-verbs (as-> (get-in @db [:users username :equipment])
                       x (vals x) (mapcat vals x)
                       (lmerge x) (get x verb))
        item-verbs (as-> (get-in @db [:rooms room :items])
                         x (vals x) (mapcat vals x)
                         (lmerge x) (get x verb))
        room-verbs (get-in @db [:rooms room :verbs verb])
        global-verbs (get-in @db [:global-verbs] verb)]

    (runverb (first (filter not-nil? '(eq-verbs item-verbs room-verbs global-verbs)))
             username words )))

(defn runverb [verb subject words]
  (if verb
    (letfn [(msg-to [username msg] (go (>! (channel-user username) msg))) ]
     verb subject (rest words))
    (prn)))

(defverb :say "say [text to be communicated]" '(go (>! (r @rooms) (pr-str msg))))
(defverb :gsay '(go (>! global-chat (pr-str msg))))
(defverb :move "move direction" '(go) )
(defverb :login "login username hashedpass"
  (go (let [room (r @rooms)]
        (tap (get @rmult r) ws) 
        (>! room (pr-str {:action :JOIN :room r :name (:name (get @conx ws))})))))
(defverb :logout "logout"
  (go (let [newmap (do-logout ws msg)]
        (info newmap)
        (map #(untap % ws) (conj (vals @rmult) gmult))
        (>! global-chat (pr-str {:action :PART :name (:name newmap)})))))
(defverb :userlist "userlist mask"
  (go (>! (:channel subject) (pr-str {:action :USERLIST :userlist (map :name (vals @conx))} ))))
(defverb :look "Look around you. Just look around you."
    (go (>! (:channel subject) (:desc room))))

(defn contextual-eval [ctx expr]
(eval
  `(let [~@(mapcat (fn [[k v]] [k `'~v]) ctx)] ~expr)))

