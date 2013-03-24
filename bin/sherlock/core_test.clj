(ns sherlock.core-test
  (:use clojure.test
        sherlock.core))

(def test-raw-message1
  ":foo!~foo@li231-96.members.linode.com PRIVMSG #4clojure")

(def test-raw-message2
  ":foo!~foo@li231-96.members.linode.com PRIVMSG #4clojure :ckirkendall: would you mind using foo on irc, rather than alan? it's nice to get notifications from my irc client")

;; clear repl: (map #(ns-unmap *ns* %) (keys (ns-interns *ns*)))

(deftest parse-result-raw-should-be-original-message
  (is (= test-raw-message1 (:raw (create-irc-command test-raw-message1)))))

(deftest parse-result-command-should-be-privmsg
  (is (= "PRIVMSG" (:command (create-irc-command test-raw-message1)))))

(deftest parse-result-params-should-be-list-with-4clojure
  (is (= ["#4clojure"] (:params (create-irc-command test-raw-message1)))))

;; (deftest parse-result-host-should-be-li231-96.members.linode.com
;;  (is (= "li231-96.members.linode.com" (:host (create-irc-command test-raw-message1)))))

(deftest parse-result-user-should-be-tildefoo
  (is (= "~foo" (:user (:irc-user (create-irc-command test-raw-message1))))))

(deftest parse-result-nick-should-be-foo
  (is (= "foo" (:nick (:irc-user (create-irc-command test-raw-message1))))))

(deftest parse-result-should-be-complete-IRCCommand
  (testing (str "parse " test-raw-message1)
    (is (= (create-irc-command test-raw-message1)
            (new-irc-command (new-irc-user "~foo" "foo")
                             "PRIVMSG"
                             (seq ["#4clojure"])
                             test-raw-message1))))
  )

(deftest irc-user-from-server-message-should-contain-nick-and-user
  (is (= (create-irc-user ":vorce!~vorce@c83-248-68-145.bredband.comhem.se")
         (new-irc-user "~vorce" "vorce")))
      )

(deftest irc-command-from-server-ping-should-contain-ping-and-server-name
  (is (= (new-irc-command (new-irc-user "" "")
                          "PING" (seq [":adams.freenode.net"])
                          "PING :adams.freenode.net") 
        (create-irc-command "PING :adams.freenode.net"))))

(deftest irc-command-from-server-privmsg-should-contain-user-privmsg-params
  (is (= (new-irc-command (new-irc-user "~vorce" "vorce")
                          "PRIVMSG"
                          (seq ["#octavorce", ":oh,", "you", "again", ":)"])
                          ":vorce!~vorce@c83-248-68-145.bredband.comhem.se PRIVMSG #octavorce :oh, you again :)") 
        (create-irc-command
          ":vorce!~vorce@c83-248-68-145.bredband.comhem.se PRIVMSG #octavorce :oh, you again :)")
         )))

(deftest irc-message-from-vorce-should-be-bosscommand
  (is (= (bosscommand?
           (create-irc-command
             ":vorce!~vorce@c83-248-68-145.bredband.comhem.se PRIVMSG sherlock_clj :HELLO"))
         true)))

(deftest irc-message-from-loser-should-not-be-bosscommand
  (is (= (bosscommand?
           (create-irc-command
             ":loser!~loser@c83-248-68-145.bredband.comhem.se PRIVMSG sherlock_clj :HELLO"))
         false)))

(deftest irc-message-with-magic-words-should-be-quitcommand
  (is (= (quitcommand?
           (create-irc-command
             ":loser!~loser@c83-248-68-145.bredband.comhem.se PRIVMSG sherlock_clj :QUIT IRC YOU FOOL"))
         true)))

(deftest irc-message-without-magic-words-should-not-be-quitcommand
  (is (= (quitcommand?
           (create-irc-command
             ":loser!~loser@c83-248-68-145.bredband.comhem.se PRIVMSG sherlock_clj :HELLO"))
         false)))
               