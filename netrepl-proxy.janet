#! /usr/bin/env janet

# adapted bits from spork/msg.janet

(defn make-recv
  ``
  Get a function that, when invoked, gets the next message from a
  readable stream.  Provide an optional unpack function that will
  parse the received buffer.
  ``
  [stream &opt unpack]
  (def buf @"")
  (default unpack string)
  (fn receiver []
    (buffer/clear buf)
    (if-not (:chunk stream 4 buf) (break))
    (def [b0 b1 b2 b3] buf)
    (def len (+ b0 (* b1 0x100) (* b2 0x10000) (* b3 0x1000000)))
    (buffer/clear buf)
    (if-not (:chunk stream len buf) (break))
    (unpack (string buf))))

(defn make-send
  ``
  Create a function that when called with a msgs sends that msg.
  Provide an optional pack function that will convert a message to a
  string.
  ``
  [stream &opt pack]
  (def buf @"")
  (default pack string)
  (fn sender [msg]
    (def x (pack msg))
    (buffer/clear buf)
    (buffer/push-word buf (length x))
    (buffer/push-string buf x)
    (:write stream buf)
    nil))

# adapted bits from spork/netrepl.janet

(defn make-recv-client
  ``
  Similar to make-recv, except has exceptions for out-of-band
  messages (those that begin with 0xFF and 0xFE.
  ``
  [stream]
  (def recvraw (make-recv stream))
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
  (default host "127.0.0.1")
  (default port "9365")
  (default name (string "[" host ":" port "]"))
  (with [stream (net/connect host port)]
    (def from-server-recv (make-recv-client stream))
    (def to-server-send (make-send stream))
    (to-server-send (string/format "\xFF%j"
                                   {:auto-flush true :name name}))
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
