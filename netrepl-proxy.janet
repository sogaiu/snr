#! /usr/bin/env janet

(import spork/msg)
(import spork/netrepl)

# slightly adapted bits from spork

(defn make-recv-client
  "Similar to msg/make-recv, except has exceptions for out-of-band
  messages (those that begin with 0xFF and 0xFE."
  [stream]
  (def recvraw (msg/make-recv stream))
  (fn recv
    []
    (def x (recvraw))
    (case (get x 0)
      0xFF
      (do
        (prin (string/slice x 1))
        (flush)
        (recv))
      0xFE
      (string/slice x 1)
      x)))

(defn client
  [&opt host port name]
  (default host netrepl/default-host)
  (default port netrepl/default-port)
  (default name (string "[" host ":" port "]"))
  (with [stream (net/connect host port)]
    (def from-server-recv (make-recv-client stream))
    (def to-server-send (msg/make-send stream))
    (to-server-send (string/format "\xFF%j" {:auto-flush true :name name}))
    (forever
      (def p
        (from-server-recv))
      (unless p
        (break))
      # echo prompt
      (file/write stdout p)
      (def line
        (getline p @"" root-env))
      (when (empty? line)
        (break))
      (to-server-send
        (if (keyword? line)
          (string "\xFE" line)
          line)))))

(defn main
  [& args]
  (client))
