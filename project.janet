(declare-project
  :name "x43bot"
  :description "An IRC bot"
  :author "Calvin Rose"
  :url "https://github.com/bakpakin/x43bot"
  :dependencies ["spork"])

(declare-executable
  :name "x43bot"
  :entry "x43bot.janet"
  :no-core true
  :install true)
