(declare-project
  :name "x43bot"
  :description "An IRC bot"
  :author "Calvin Rose"
  :dependencies ["https://github.com/janet-lang/juv.git"
                 "https://github.com/janet-lang/argparse.git"])

(declare-executable
  :name "x43bot"
  :entry "x43bot.janet"
  :install true)
