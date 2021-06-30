;;; telega-adblock.el --- Advertisement blocking for telega  -*- lexical-binding: t -*-

;; Copyright (C) 2021 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Jun 16 11:10:15 2021
;; Keywords:

;; telega is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; telega is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with telega.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; ellit-org:
;; ** /telega-adblock.el/ -- Block advertisement messages   :new:
;;
;; Telegram channels often advertises another channels in annoying
;; manner.  Sometimes adverts has inappropriate content.  This mode
;; uses [[#client-side-messages-ignoring][client side messages
;; ignoring]] functionality to block such advert messages.
;;
;; To enable advertisement blocking use:
;; #+begin_src emacs-lisp
;; (telega-adblock-mode 1)
;; #+end_src
;;
;; Customizable options:
;; - {{{user-option(telega-adblock-for, 2)}}}
;; - {{{user-option(telega-adblock-chat-order-if-last-message-ignored, 2)}}}
;; - {{{user-option(telega-adblock-verbose, 2)}}}
;; - {{{user-option(telega-adblock-max-distance, 2)}}}

;;; Code:
(require 'telega)

(defgroup telega-adblock nil
  "Customisation for telega adblock mode."
  :prefix "telega-adblock-"
  :group 'telega-modes)

(defcustom telega-adblock-for '(and (type channel) (not verified))
  "Chat Filter defines for which chats to apply adblock logic."
  :type 'list
  :group 'telega-adblock)

(defcustom telega-adblock-max-distance 4
  "Maximum string-distance for self-link.
Used for heuristics to avoid blocking non-advert messages in some channels.
Set it to less value if you see some advert messages not being blocked."
  :type 'integer
  :group 'telega-adblock)

(defcustom telega-adblock-verbose nil
  "Non-nil to show (in echo area) reason why message is ignored."
  :type 'boolean
  :group 'telega-adblock)

(defcustom telega-adblock-chat-order-if-last-message-ignored nil
  "Custom chat order for chats with last message being ignored by adblock.
Set to \"1\" to put chats with ignored last message to the bottom of
the rootbuf."
  :type '(or string null)
  :group 'telega-adblock)

;; TODO: allow links to known chats
(defcustom telega-adblock-allow-links-to-known-chats t
  "Non-nil to not block messages with links to known chats."
  :type 'boolean
  :group 'telega-adblock)

;; TODO: heuristics about multiple links to same url
;; to block messages like https://t.me/c/1127375190/3747

(defun telega-adblock-msg-extract-links (msg)
  "Extract all links from the message."
  (let ((reply-markup (plist-get msg :reply_markup))
        (ret-links nil))
    ;; Extract links from the reply-markup keyboard
    (seq-doseq (row (plist-get reply-markup :rows))
      (seq-doseq (kbd-button row)
        (let ((kbd-type (plist-get kbd-button :type)))
          (when (eq 'inlineKeyboardButtonTypeUrl (telega--tl-type kbd-type))
            (setq ret-links
                  (cons (cons (telega-tl-str kbd-button :text)
                              (plist-get kbd-type :url))
                        ret-links))))))

    ;; Extract links from the message's text
    (let* ((content (plist-get msg :content))
           (msg-text (or (telega-tl-str content :text)
                         (telega-tl-str content :caption))))
      (seq-doseq (txt (telega--split-by-text-prop msg-text :telega-link))
        (when-let* ((txt-link (get-text-property 0 :telega-link txt))
                    (link-url (when (eq 'url (car txt-link))
                                (cdr txt-link))))
          (setq ret-links
                (cons (cons txt link-url)
                      ret-links)))))
    ret-links))

(defun telega-adblock-link-advert-p (chat link-spec)
  "Return non-nil if LINK-SPEC is an advertisement link.
LINK-SPEC is a cons cell, where car is text under the link and cdr is
an URL."
  (let ((chat-title (telega-chat-title chat))
        (chat-username (telega-chat-username chat))
        (link-text (car link-spec))
        (link-url (cdr link-spec)))
    (and
     ;; 1. Link text does not match CHAT's title/username
     (> (string-distance chat-title link-text)
        telega-adblock-max-distance)
     (or (null chat-username)
         (> (string-distance chat-username link-text)
            telega-adblock-max-distance))
     ;; 3. Link URL is not in the chat's description
     (let* ((full-info (telega--full-info (telega-chat--info chat) 'offline))
            (descr (telega-tl-str full-info :description)))
       (or (null descr)
           (not (string-match-p (regexp-quote link-url) descr))))
     ;; 3. Link URL points to some channel or/and post
     (when (or (string-prefix-p "https://t.me/" link-url)
               (string-prefix-p "http://t.me/" link-url)
               (string-prefix-p "https://telegram.me/" link-url)
               (string-prefix-p "https://telegram.dog/" link-url))
       (let ((tme-internal-link (telega-tme-open link-url 'convert)))
         (or (string-prefix-p "tg:join?" tme-internal-link)
             (string-prefix-p "tg:msg_url?" tme-internal-link)
             (string-prefix-p "tg:privatepost?" tme-internal-link)
             (and (string-prefix-p "tg:resolve?" tme-internal-link)
                  ;; 4. Link URL is not direct url to the CHAT
                  (not (and chat-username
                            (string-prefix-p
                             (concat "tg:resolve?domain=" chat-username)
                             tme-internal-link)))))))
     (progn
       (if telega-adblock-verbose
           (message "telega: Blocking advert link: %s in %s"
                    link-url chat-title)
         (telega-debug "ADBLOCK: Blocking advert link: %s in %s"
                       link-url chat-title))
       t)
     )))

(defun telega-adblock-msg-ignore-p (msg)
  "Return non-nil if message MSG is advert message."
  (when-let ((chat (telega-msg-chat msg 'offline)))
    (and (telega-chat-match-p chat telega-adblock-for)
         ;; NOTE: message considered as advertisement if it has link
         ;; to another channel.
         (cl-some (apply-partially #'telega-adblock-link-advert-p chat)
                  (telega-adblock-msg-extract-links msg)))))

(defun telega-adblock--chat-order-if-last-msg-ignored (orig-fun chat &rest args)
  (if (and telega-adblock-chat-order-if-last-message-ignored
           (eq (telega-msg-ignored-p (plist-get chat :last_message))
               'telega-adblock-msg-ignore-p))
      telega-adblock-chat-order-if-last-message-ignored
    (apply orig-fun chat args)))

;;;###autoload
(define-minor-mode telega-adblock-mode
  "Global mode to block ads for `telega-adblock-for' chats."
  :init-value nil :global t :group 'telega-modes
  (if telega-adblock-mode
      (progn
        (add-hook 'telega-msg-ignore-predicates #'telega-adblock-msg-ignore-p)
        (advice-add 'telega-chat-order
                    :around #'telega-adblock--chat-order-if-last-msg-ignored))
    (advice-remove 'telega-chat-order
                   #'telega-adblock--chat-order-if-last-msg-ignored)
    (remove-hook 'telega-msg-ignore-predicates #'telega-adblock-msg-ignore-p)))

(provide 'telega-adblock)

;;; telega-adblock.el ends here