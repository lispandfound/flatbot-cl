(defpackage cli
  (:use :cl :cl-cli :flatbot)
  (:export main))
(in-package cli)
(defparameter *options* '((*db-path* nil "Location of flatbot database." :alias ("-d") :params ("DB"))
                          (*token* nil "HTTP token for bot" :alias ("-t") :params ("TOKEN"))))
(defun main ()
  (multiple-value-bind (vars vals) (parse-cli sb-ext:*posix-argv* *options*)
    (with-environment vars vals
      (start-bot *db-path* *token*)
      (loop for thread in (bt:all-threads)
            when (equal (bt:thread-name thread) "telegram-bot")
            do (bt:join-thread thread)))))
