(require "asdf")
(asdf:defsystem "flatbot"
  :depends-on ("cl-telegram-bot" "cl-ppcre" "sqlite" "sxql" "cl-cli" "bordeaux-threads")
  :build-operation "program-op"
  :build-pathname "flatbot"
  :entry-point "cli:main"
  :components
  ((:file cli :depends-on ("flatbot"))
   (:file flatbot :depends-on ("database"))
   (:file database)))
