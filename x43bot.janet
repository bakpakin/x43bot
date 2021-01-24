#!/usr/bin/env janet

###
### ./x43bot
###
### IRC bot that can dispatch on commands from users.
### Simple to configure and extend in code.
###
### Copyright © Calvin Rose <calsrose@gmail.com> 2019
###

(import spork/argparse)

(def version "0.0.0")

(def irc-peg
  "PEG to parser server IRC commands. Defined in RFC 1459"
  (peg/compile
    ~{:space (some " ")
      :trailing '(any (if-not (set "\0\r\n") 1))
      :middle '(some (if-not (set " \0\r\n") 1))
      :params (+ -1 (* :space (+ (* ":" :trailing) (* :middle :params))))
      :command (+ '(some (range "az" "AZ"))
                  (/ '(between 3 3 (range "09")) ,scan-number))
      :word (some (if-not " " 1))
      :prefix ':word
      :main (* (? (* (constant :prefix)
                     ":" :prefix :space))
               (constant :command) :command
               (constant :params) (group :params))}))

# Sandboxed env
(defn make-sandbox
  [allowed]
  (def newenv @{})
  (def syms
    (map
      symbol
      (peg/match
        '{:ws (set " \n\r\t")
          :main (any (* (any :ws) '(some (if-not :ws 1))))}
        allowed)))
  (each s syms
    (put newenv s (dyn s)))
  newenv)
(def sandbox (make-sandbox ````
% %= * *= + ++ += - -- -= -> ->> -?> -?>> / /= < <= = == > >=
abstract? all and apply array array/concat array/ensure array/insert array/new array/peek array/pop
array/push array/remove array/slice array? as-> as?-> band blshift bnot boolean? bor brshift brushift
buffer buffer/bit buffer/bit-clear buffer/bit-set buffer/bit-toggle buffer/blit buffer/clear
buffer/format buffer/new buffer/new-filled buffer/popn buffer/push-byte buffer/push-string
buffer/push-word buffer/slice buffer? bxor bytes? case cfunction? comment comp compile
complement cond coro count dec deep-not= deep= def- default defglobal defmacro defmacro-
defn defn- describe dictionary? distinct doc doc* doc-format
drop drop-until drop-while dyn each empty? env-lookup error eval eval-string even?
every? extreme false? fiber/current fiber/getenv fiber/maxstack fiber/new fiber/setenv fiber/setmaxstack fiber/status
fiber? filter find find-index first flatten flatten-into for freeze frequencies function? generate gensym get get-in hash idempotent?
identity if-let if-not inc indexed? int/s64 int/u64 int? interleave interpose
invert janet/build janet/config-bits janet/version juxt juxt* keep keys keyword
keyword? kvs last length let loop macex macex1 map
mapcat marshal match math/abs math/acos math/asin math/atan math/atan2 math/ceil math/cos math/cosh
math/e math/exp math/floor math/inf math/log math/log10 math/pi math/pow math/random
math/seedrandom math/sin math/sinh math/sqrt math/tan math/tanh max max-order mean
merge merge-into min min-order module/expand-path nat? native neg? next nil? not not= not== number? odd? one? or
order< order<= order> order>= os/arch os/clock os/date os/time os/which pairs parser/byte
parser/clone parser/consume parser/eof parser/error parser/flush parser/has-more
parser/insert parser/new parser/produce parser/state parser/status parser/where partial partition
peg/compile peg/match pos? postwalk prewalk product propagate put put-in range
reduce resume reverse run-context scan-number seq setdyn short-fn some sort
sorted spit string string/ascii-lower string/ascii-upper string/bytes
string/check-set string/find string/find-all string/format string/from-bytes string/has-prefix?
string/has-suffix? string/join string/repeat string/replace string/replace-all string/reverse
string/slice string/split string/trim string/triml string/trimr string? struct struct? sum
symbol symbol? table table/clone table/getproto table/new table/rawget table/setproto
table/to-struct table? take take-until take-while tarray/buffer tarray/copy-bytes
tarray/length tarray/new tarray/properties tarray/slice tarray/swap-bytes trace true? try
tuple tuple/brackets tuple/setmap tuple/slice tuple/sourcemap tuple/type tuple? type unless
unmarshal untrace update update-in values varglobal walk when when-let with with-dyns
with-syms yield zero? zipcoll````))

(defn main [&]

  (def opts
    (argparse/argparse
      "A Simple IRC bot that responds to messages."
      "verbose" {:kind :flag
                 :short "v"
                 :help "Print debug information to stdout."}
      "nick" {:kind :option
              :short "n"
              :default "x43bot"
              :help "Bot nickname"}
      "channel" {:kind :option
                 :short "c"
                 :help "What irc channel to connect to, with no leading #"
                 :required true}
      "host" {:kind :option
              :short "H"
              :help "The irc server to connect to. x43bot does not support SSL."
              :default "chat.freenode.net"}
      "port" {:kind :option
              :short "p"
              :help "The port to connect."
              :default "6667"}))

  (unless opts (os/exit 1))

  (def {"nick" nick
        "host" host
        "port" port
        "channel" channel
        "verbose" verbose}
    opts)

  (def command-peg
    ```
    Peg to handle incoming commands. Commands look like:
    x43bot: command arg1 'arg 2' "arg'3"
    ```
    (peg/compile
      ~{:param (+ (* `"` '(any (if-not `"` (* (? "\\") 1))) `"`)
                  (* `'` '(any (if-not `'` (* (? "\\") 1))) `'`)
                  '(some (if-not " " 1)))
        :main (* (any " ") ,nick ":"
                 (some (* (any " ") :param)))}))


  # User facing commands go here
  (def available-commands @[])
  (def handlers
    @{:poke (fn [&] "Hey, stop it!")
      :eval (fn [code &]
                 (defn sandbox-wrap []
                   (string/format "%.10q" (eval-string code)))
                 (def f (fiber/new sandbox-wrap :a))
                 (fiber/setenv f sandbox)
                 (def res (resume f))
                 (def sig (fiber/status f))
                 (if (= sig :dead)
                   res
                   (string "signal " sig " raised: " res)))
      :version (fn [&] 
                 (string "x43bot version " version ", Janet "
                               janet/version "-" janet/build))
      :help (fn [&] (string
                      "Available commands: "
                      (string/join available-commands ", ")))})
  (array/concat available-commands (sort (keys handlers)))

  (var client
    "TCP Socket to connect to irc server"
    nil)

  (defn irc-write
    "Write a message to the connected irc channel."
    [& parts]
    (def msg (string ;parts "\r\n"))
    (print "> " ;parts)
    (net/write client msg))

  (def read-queue
    "We may get back large chunks
    of data from the server, and we need it to be
    buffered by line. This queue keeps not yet processed lines."
    @[])

  (defn irc-read
    "Read a server response after sending a command."
    []
    (while (empty? read-queue)
      (def chunk (net/read client 4096))
      (array/concat
        read-queue
        (filter |(not (empty? $))
                (string/split "\r\n" chunk))))
    (def first-in (read-queue 0))
    (array/remove read-queue 0)
    first-in)

  (defn on-bot-command
    "Handle user initiated command to the bot"
    [irc-args]
    (if-let [msg (irc-args 1)]
      (when-let [args (peg/match command-peg msg)]
                (def f (or (handlers (keyword (args 0)))
                           (fn [&]
                             (string "Invalid command " (args 0)
                                     " - " ((handlers :help))))))
                (def response (try
                                (f ;(tuple/slice args 1))
                                ([err] (string "error: " err))))
                (if verbose 
                  (print "> " response)
                  (print "< " msg " > " response))
                (irc-write "PRIVMSG " (irc-args 0) " :" response))))

  (defn on-line
    "Handle an IRC message from the server."
    [line]
    (when verbose (print "< " line))
    (def m (peg/match irc-peg line))
    (unless m (break))
    (def {:params params
          :command command
          :prefix prefix} (table ;m))
    (case command
      "PING" (irc-write "PONG :" (params 0))
      "PRIVMSG" (on-bot-command params)))

  #
  # Main
  #

  (set client (net/connect host (scan-number (string port))))

  (irc-write "NICK " nick)
  (irc-write "USER " nick " * * :X43 Bot")
  (irc-write "JOIN #" channel)

  (while true
    (if-let [line (irc-read)]
      (on-line line))))
