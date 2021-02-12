(defpackage database
  (:use :cl :sxql)
  (:export get-chat store-debt flatmates insert-flatmates tally-debt get-transactions insert-chat
           get-chats pay-debt settle-debts create-group group-add get-flatmates-in-group get-group-by-name))
(in-package database)


(defun execute-one-row-m-v  (db query)
  (multiple-value-bind (q args) (yield query)
    (apply 'sqlite:execute-one-row-m-v db q args)))

(defun execute-query  (db query)
  (multiple-value-bind (q args) (yield query)
    (apply 'sqlite:execute-non-query db q args)))

(defun execute-single  (db query)
  (multiple-value-bind (q args) (yield query)
    (apply 'sqlite:execute-single db q args)))

(defun execute-to-list  (db query)
  (multiple-value-bind (q args) (yield query)
    (apply 'sqlite:execute-to-list db q args)))

(defun get-chat (db telegram-chat-id)
  (execute-to-list db (select (:chat_id :telegram_chat_id :property_id)
                              (from :chat)
                              (where (:= :telegram_chat_id telegram-chat-id)))))


(defun store-debt (db chat-id payable receivable reason amount)
  (execute-query db (insert-into :debt
                                 (set=
                                  :chat_id chat-id
                                  :payable payable
                                  :receivable receivable
                                  :reason reason
                                  :amount amount))))

(defun flatmates (db chat-id)
  (execute-to-list db (select (:flatmate_id :chat_id :name)
                              (from :flatmates)
                              (where (:= :chat_id chat-id)))))

(defun get-flatmate (db chat-id flatmate-name)
  (car (or (sqlite:execute-to-list db "SELECT flatmate_id, chat_id, name FROM flatmates WHERE chat_id = ? AND name = ? COLLATE NOCASE" chat-id flatmate-name) '(nil))))

(defun insert-flatmate (db chat-id name)
  (execute-query db (insert-into :flatmates
                                 (set=
                                  :chat_id chat-id
                                  :name name))))

(defun tally-debt (db chat-id payable receivable)
  (let ((owed-to-payable  (execute-single db (select
                                              (fields (:sum :amount))
                                              (from :debt)
                                              (where (:and (:= :chat_id chat-id) (:= :payable payable) (:= :receivable receivable) (:= :paid 0))))))
        (owed-to-receivable (execute-single db (select
                                                (fields (:sum :amount))
                                                (from :debt)
                                                (where (:and (:= :chat_id chat-id) (:= :payable receivable) (:= :receivable payable) (:= :paid 0)))))))
    (- (or owed-to-payable 0) (or owed-to-receivable 0))))


(defun get-transactions  (db chat-id payable receivable)
  (execute-to-list db (select (:debt_id :payable :receivable :reason :amount :paid)
                              (from :debt)
                              (where (:and (:= :chat_id chat-id) (:or (:and (:= :payable payable) (:= :receivable receivable)) (:and (:= :payable receivable) (:= :receivable payable))))))))

(defun insert-chat  (db chat-id property-id)
  (execute-query db (insert-into :chat
                                 (set= :telegram_chat_id chat-id
                                       :property_id property-id))))

(defun get-chats  (db)
  (execute-to-list db (select (:chat_id :telegram_chat_id :property_id)
                              (from :chat))))

(defun pay-debt  (db debt-id)
  (execute-query db (update :debt
                            (set=
                             :paid 1
                             :debt_id debt-id))))

(defun settle-debts  (db chat-id payable receivable)
  (execute-query db (update :debt
                            (set= :paid 1)
                            (where (:and (:= :chat_id chat-id) (:or (:and (:= :payable payable) (:= :receivable receivable)) (:and (:= :payable receivable) (:= :receivable payable))))))))


(defun create-group  (db chat group-name)
  (execute-query db (insert-into :groups
                                 (set= :group_name group-name
                                       :telegram_chat_id chat))))

(defun group-add  (db flatmate-id group-id)
  (execute-query db (insert-into :group_members
                                 (set=
                                  :group_id group-id
                                  :flatmate_id flatmate-id))))

(defun get-flatmates-in-group  (db group-id)
  (execute-to-list db (select (:flatmates.flatmate_id :chat_id :name)
                              (from :flatmates)
                              (inner-join :group_members :on (:= :group_members.flatmate_id :flatmates.flatmate_id))
                              (where (:= :group_members.group_id group-id)))))

(defun get-group-by-name  (db chat-id group-name)
  (car (or (sqlite:execute-to-list db "SELECT group_id, telegram_chat_id, group_name FROM groups WHERE telegram_chat_id = ? AND group_name = ? COLLATE NOCASE" chat-id group-name) '(nil))))



