(defpackage flatbot (:use :cl :cl-telegram-bot :cl-ppcre :database)
            (:import-from :database
             :store-debt
             :get-flatmate
                          :insert-flatmate)
            (:import-from :cl-telegram-bot/message
                          :get-raw-data
                          :get-current-chat)
            (:import-from :cl-telegram-bot/chat
             :get-chat-id)
            (:export start-bot))

(in-package flatbot)

(defbot flatbot)

(defvar *db* nil)

(defun get-sender ()
  (getf (getf (get-raw-data cl-telegram-bot/message::*current-message*) :|from|) :|first_name|))

(defmethod on-message ((bot flatbot)
                       text)
  (register-groups-bind (group receivable amount reason) ("(\\w+) split with (\\w+) $?(\\d+(?:\\.\\d+)?)(?: for (.*))?" text)
    (let* ((chat (get-chat-id (get-current-chat)))
           (group-obj (get-group-by-name *db* chat group))
           (flatmate-obj (get-flatmate *db* chat receivable))
           (me (get-sender))
           (me-obj (get-flatmate *db* chat me))
           (receivable-obj (if (equal receivable "me") me-obj flatmate-obj))
           (amount (read-from-string amount))

           )
      (cond
        ((null group-obj) (reply (format nil "Could not find the group ~a in this chat, please register it using /newgroup ~a" group group)))
        ((null receivable-obj) (reply (format nil "Could not find ~a in this chat, please register them using /register <name>" (if (equal receivable "me") me receivable))))
        (t (let*
               ((flatmates (get-flatmates-in-group *db* (car group-obj)))
                (amount (/ amount (1+ (length flatmates)))))
             (loop for flatmate in flatmates
                   do (store-debt *db* chat (car flatmate) (car receivable-obj) reason amount))
             (reply (format nil "Got it, everyone in ~a owes ~a ~$." (caddr group-obj) (caddr receivable-obj) amount)))))))
  (register-groups-bind (payable receivable amount reason) ("(\\w+) owes (\\w+) \\$?(\\d+(?:\\.\\d+)?)(?: for (.*))?" text)
    (let* ((amount (read-from-string amount))
           (chat (get-chat-id (get-current-chat)))
           (payable-obj (get-flatmate *db* chat payable))
           (receivable-obj (get-flatmate *db* chat receivable))
           (me (get-sender))
           (me-obj (get-flatmate *db* chat me)))
      (cond
        ((null payable-obj) (reply (format nil "Could not find ~a in this chat, please register them using /register <name>." payable)))
        ((and (not (equal receivable "me")) (null receivable-obj)) (reply (format nil "Could not find ~a in this chat, please register them using /register ~a." receivable receivable)))
        ((and (equal receivable "me") (null me-obj)) (reply (format nil "Could not find ~a in this chat, please register yourself using /register ~a." me me)))
        (t (progn
             (store-debt *db* chat (car payable-obj) (or (car receivable-obj) (car me-obj)) reason amount)
             (reply (format nil "Got it, ~a owes ~a $~$" (caddr payable-obj) (if (equal receivable "me") (caddr me-obj) (caddr receivable-obj)) amount))))))))

(defmethod on-command ((bot flatbot)
                       (command (eql :register))
                       name)
  (let ((chat (get-chat-id (get-current-chat))))
    (insert-flatmate *db* chat name)
    (reply (format nil "Added ~a." name))))

(defmethod on-command ((bot flatbot)
                       (command (eql :tally))
                       arguments)
  (let ((arguments (split " " arguments))
        (chat (get-chat-id (get-current-chat))))
    (if (equal (length arguments) 2)
        (let* ((payable (car arguments))
               (receivable (cadr arguments))
               (payable-obj (get-flatmate *db* chat payable))
               (receivable-obj (get-flatmate *db* chat receivable)))
          (cond
          ((null receivable-obj) (reply (format nil "Could not find ~a in this chat, please register them using /register ~a." receivable receivable)))
          ((null payable-obj) (reply (format nil "Could not find ~a in this chat, please register them using /register ~a." payable payable)))
          (t (let ((debt (tally-debt *db* chat (car payable-obj) (car receivable-obj))))
               (reply (format nil "~a owes ~a $~$." (if (> debt 0) (caddr payable-obj) (caddr receivable-obj)) (if (> debt 0) (caddr receivable-obj) (caddr payable-obj)) (abs debt)))
               ))
          ))
        (reply "Invalid use of /tally, correct usage: /tally <receivable> <payable>"))))

(defmethod on-command ((bot flatbot)
                       (command (eql :settle))
                       text)
  (let* ((chat (get-chat-id (get-current-chat)))
         (settle-account (get-flatmate *db* chat text))
         (me (get-flatmate *db* chat (get-sender))))
    (cond 

      ((null settle-account) (reply (format nil "Could not find ~a in this chat, please register them using /register ~a" text text)))
      ((null me) (reply (format nil "Could not find sender in this chat, please register them using /register ~a" (get-sender))))
      (t (progn
           (settle-debts *db* chat (car me) (car settle-account))
           (reply (format nil "~a has settled all outstanding debts with ~a." (caddr me) (caddr settle-account))))))))

(defmethod on-command ((bot flatbot)
                       (command (eql :newgroup))
                       text)
  (let ((chat (get-chat-id (get-current-chat))))
    (create-group *db* chat text)
    (reply (format nil "Created group ~a." text))))

(defmethod on-command ((bot flatbot)
                       (command (eql :groupadd))
                       arguments)

  (let ((arguments (split " " arguments)))
    (if (equal (length arguments) 2)
        (let* ((chat (get-chat-id (get-current-chat)))
               (group (get-group-by-name *db* chat (car arguments)))
               (flatmate (get-flatmate *db* chat (cadr arguments))))
          (cond
            ((null group) (reply (format nil "Could not find group ~a, please add the group with /groupadd ~a" (car arguments) (car arguments))))
            ((null flatmate) (reply (format nil "Could not find ~a in this chat, please add them using /register ~a" (cadr arguments) (cadr arguments))))
            (t (progn
                 (group-add *db* (car flatmate) (car group))
                 (reply (format nil "Added ~a to the group ~a." (caddr flatmate) (caddr group)))))))
        (reply "Incorrect usage of /groupadd, correct usage: /groupadd <group> <member>"))))


(defun start-bot (db-path token)
  (setq *db* (sqlite:connect db-path))
  (start-processing (make-flatbot token)))
