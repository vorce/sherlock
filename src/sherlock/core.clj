(ns sherlock.core
  (:use lamina.core aleph.tcp gloss.core))

(defrecord IRCServer [host port])
(def freenode
  (IRCServer. "irc.freenode.net" 6667))

(defrecord IRCUser [user nick])

(defn new-irc-user [user nick]
  (IRCUser. user nick))
(def test-user
  (IRCUser. "~sherlock_clj" "sherlock_clj"))
(def boss-user
  (IRCUser. "~vorce" "vorce"))

;; :vorce!~vorce@c83-248-68-145.bredband.comhem.se => IRCUser ~vorce vorce
(defn create-irc-user [user-token]
  (let [split-tokens (clojure.string/split user-token #"!|@")]
    (IRCUser. (second split-tokens)
              (clojure.string/join (rest (first split-tokens))))
    )
  )

(defrecord IRCChannel [identifier])
(def test-channel
  (IRCChannel. "#octavorce"))

(defrecord IRCClient [connection server])

;; Examples
;; PING :adams.freenode.net
;; :vorce!~vorce@c83-248-68-145.bredband.comhem.se PRIVMSG #octavorce :oh, you again ;D
;; :sherlock_clj!~sherlock_@c83-248-68-145.bredband.comhem.se QUIT :Ping timeout: 246 seconds
;; ERROR :Closing Link: c83-248-68-145.bredband.comhem.se (Ping timeout: 246 seconds)
;; :sherlock_clj!~sherlock_@c83-248-68-145.bredband.comhem.se JOIN #octavorce
;; :adams.freenode.net 353 sherlock_clj @ #octavorce :sherlock_clj @vorce
(defrecord IRCCommand [irc-user command params raw])

(defn new-irc-command [irc-user command params raw]
  (IRCCommand. irc-user command params raw))

(defn prefix?
  "Check if the raw message has a prefix or not (i.e does it start with ':'?)"
  [raw]
  (= ":" (str (first raw))))

(defn create-irc-command
  "Create a structured IRCCommand from a raw message"
  [raw-message]
  (let [tokens (clojure.string/split raw-message #" ")]
    (if (prefix? raw-message)
    (IRCCommand. (create-irc-user (first tokens)) (second tokens) (nthrest tokens 2) raw-message)
    (IRCCommand. (IRCUser. "" "") (first tokens) (rest tokens) raw-message)
    )
  )
  )

(defn irc-connection
  "Create a connection to a server"
  [irc-server]
  (wait-for-result
    (tcp-client {:host (:host irc-server),
                 :port (:port irc-server),
                 :frame (gloss.core/string :utf-8 :delimiters ["\r\n"])})))

(defn new-irc-client-on [irc-server]
  (IRCClient. (irc-connection irc-server) irc-server))

(defn send-to-server [from out-msg]
  (println (str "SENDING [" out-msg "]" "\n" "     TO [" (:host (:server from)) "]"))
  (lamina.core/enqueue (:connection from) out-msg))

(defn out-command [command params]
  (str command " " (clojure.string/join " " params)))

(defn login-with [irc-client as]
  (send-to-server irc-client
                  (out-command "NICK" [(:nick as)]))
  (send-to-server irc-client
                  (out-command "USER" [(:nick as), "0 *", (str ":" (:user as))]))
)

(defn join-channel [irc-client irc-channel]
  (send-to-server irc-client
                  (out-command "JOIN" [(:identifier irc-channel)])))

(defn pong [irc-client params]
  (send-to-server irc-client (out-command "PONG" [params])))

(defn quit [irc-client]
  (send-to-server irc-client (out-command "QUIT" []))
  (close (:connection irc-client)))

;; PRIVMSG #channel :message
(defn say [irc-client message to]
  (send-to-server irc-client (out-command "PRIVMSG" [to, (str ":" message)])))


(defmulti handle-irc-message (fn[irc-client irc-command] (:command irc-command)))

(defmethod handle-irc-message :default [irc-client irc-command]
  (println (str "!Unhandled command [" (:command irc-command)
                "] with params [" (:params irc-command) "]")))

(defmethod handle-irc-message "PING" [irc-client irc-command]
  (pong irc-client (:params irc-command)))

;; :vorce!~vorce@c83-248-68-145.bredband.comhem.se PRIVMSG sherlock_clj :QUIT IRC YOU FOOL
(defn bosscommand? [irc-command]
  (= (:irc-user irc-command) boss-user))

(defn quitcommand? [irc-command]
  (= (:params irc-command) (seq ["sherlock_clj" ":QUIT" "IRC" "YOU" "FOOL"])))

(defmethod handle-irc-message "PRIVMSG" [irc-client irc-command]
  (if (bosscommand? irc-command)
    (if (quitcommand? irc-command) (quit irc-client))
    ))

(defn process-message [irc-client server-msg]
  (println server-msg)
  (let [irc-cmd (create-irc-command server-msg)]
    (handle-irc-message irc-client irc-cmd))
  )

(defn test-client [server user irc-channel]
  (let [client (new-irc-client-on server)]
    (map* (partial process-message client) (:connection client))
    (login-with client user)
    (join-channel client irc-channel)
  client))

(defn -main [& args]
  (test-client freenode test-user test-channel)
  )
