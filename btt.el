;;; btt.el --- Interface to BetterTouchTool -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Andy Rosen

;; Author: Andy Rosen <ajr@corp.mlfs.org>
;; Version: 1.0
;; Keywords: BetterTouchTool, MacOS
;; Homepage: https://github.com/ajrosen/emacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Interface to BetterTouchTool (see URL `https://docs.bettertouchtool.net/').

;;; Code:

(cl-defstruct btt-widget (var nil :read-only t) (uuid nil :read-only t) attributes function)
(cl-defstruct btt-widget-attrs text icon_data icon_path background_color font_color font_size)

(eval-when-compile (defvar btt--widget))

(defvar btt--widgets nil "List of widgets (uuid/function pairs) to be processed by btt--run-hooks")

;; Customization
(defgroup btt nil
  "BetterTouchTool"

  :group 'comm
  :version "1.0"
  :prefix "btt-"
  :link '(url-link "https://docs.bettertouchtool.net/"))

(defcustom btt-protocol "http"
  "Protocol for the BetterTouchTool web server."

  :type '(choice
	  (const :tag "HTTP" :value "http")
	  (const :tag "HTTPS" :value "https"))
  :group 'btt)

(defcustom btt-server "localhost"
  "Name or IP address of the BetterTouchTool server."

  :type 'string
  :group 'btt)

(defcustom btt-port 52888
  "Port the BetterTouchTool web server is running on."

  :type 'integer
  :group 'btt)

(defcustom btt-shared-secret ""
  "Shared secret for the BetterTouchTool web server."

  :type 'string
  :group 'btt)

(defcustom btt-modified-bg "red"
  "Background of the Touch Bar widget for modified buffers."

  :type 'color
  :group 'btt)

(defcustom btt-unmodified-bg "dark green"
  "Background of the Touch Bar widget for unmodified buffers."

  :type 'color
  :group 'btt)


;; BetterTouchTool API URLs
(defmacro btt--baseurl () "Base URL" (concat btt-protocol "://" btt-server ":" (number-to-string btt-port) "/"))
(defmacro btt--secret () "Shared secret query parameter" (concat "&shared_secret=" btt-shared-secret))

(defmacro btt--get-string-url () (concat (btt--baseurl) "get_string_variable/?variableName="))
(defmacro btt--set-string-url () (concat (btt--baseurl) "set_persistent_string_variable/?variableName="))
(defmacro btt--update-url () (concat (btt--baseurl) "update_touch_bar_widget/?uuid="))


;; Variables to track when the current buffer or modified state changes
(defvar btt--buffer nil "Last visited buffer")
(defvar btt--buffer-modified-p nil "Last modified state of current buffer")


;; Helper functions
(defun btt--rgb-to-btt (color)
  "Convert COLOR (a color name) to the RGB format used by BetterTouchTool."

  (concat (number-to-string (* (car (color-name-to-rgb color)) 255)) ","
	  (number-to-string (* (cadr (color-name-to-rgb color)) 255)) ","
	  (number-to-string (* (caddr (color-name-to-rgb color)) 255)) ","
 	  "255"))

(defun btt--remove (var)
  "Remove the widget linked to the BetterTouchTool variable VAR."

  (dolist (h btt--widgets)
    (when (string= (btt-widget-var h) var)
      (setq btt--widgets (remove h btt--widgets)))))


;; BTT APIs
(defun btt-get-string-variable (var)
  "Get the value of the BetterTouchTool variable VAR."

  (let ((buffer (url-retrieve-synchronously (concat (btt--get-string-url) var (btt--secret)))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((x (thing-at-point 'line)))
	(kill-buffer buffer)
	x))))

(defun btt-set-string-variable (var value)
  "Set the VALUE of the BetterTouchTool variable VAR."

  (kill-buffer (url-retrieve-synchronously (concat (btt--set-string-url) var "&to=" value (btt--secret)))))

(defun btt--update-widget (uuid &optional text background)
  "Tell BetterTouchTool to update a Touch Bar widget's text and/or background color.

UUID is the uuid of the BetterTouchTool widget to update.  TEXT
and/or BACKGROUND can be passed as nil, in which case that
parameter is not updated."

  (let ((btt--url (concat (btt--update-url) uuid (btt--secret))))
    (if text (setq btt--url (concat btt--url "&text=" text)))
    (if background (setq btt--url (concat btt--url "&background_color=" (btt--rgb-to-btt background))))
    (url-retrieve btt--url #'(lambda (status) (kill-buffer) status) nil t t)))

;; Meta-hook
(defun btt--run-hooks ()
  "Call the functions stored in btt--widgets."

  (dolist (btt--widget btt--widgets)
    (eval (btt-widget-function btt--widget))))


;; User functions
(defun btt/current-buffer ()
  "Update a BetterTouchTool widget if ‘current-buffer’ or ‘buffer-modified-p’ has changed."

  (unless (minibufferp)
    (unless (and
	     (eq (buffer-modified-p) btt--buffer-modified-p)
	     (eq (current-buffer) btt--buffer))
      (setq btt--buffer-modified-p (buffer-modified-p))
      (setq btt--buffer (current-buffer))
      (let ((btt--bg (if (and btt--buffer-modified-p (buffer-file-name)) btt-modified-bg btt-unmodified-bg)))
	(btt--update-widget (btt-widget-uuid btt--widget) (buffer-name btt--buffer) btt--bg)))))

(defun btt/major-mode ()
  "Show the major mode of the current buffer in a BetterTouchTool widget."

  (btt--update-widget (btt-widget-uuid btt--widget) (symbol-name major-mode)))

(defun btt/emacs-version ()
  "Show the Emacs version in a BetterTouchTool widget."

  (btt--update-widget (btt-widget-uuid btt--widget) emacs-version))

(defun btt/update-widget (&optional text background)
  "Generalized function to update a widget's text and/or background color."

  (if (or text background)
      (btt--update-widget (btt-widget-uuid btt--widget) text)))


;;;###autoload
(defun btt-add (var func)
  "Use the function FUNC to update the Touch Bar widget managed
by BetterTouchTool.  VAR should be the name of a variable in
BetterTouchTool whose value is the Touch Bar widget's UUID."

  (interactive
   (list
    (read-string "BTT variable name: ")
    (completing-read "Update function: " obarray 'functionp 'confirm "btt/")))

  (btt--remove var)

  (let ((widget (make-btt-widget :var var :uuid (btt-get-string-variable var) :function func)))
    (when (> (string-bytes (btt-widget-uuid widget)) 1)
      (add-hook 'focus-in-hook 'btt--run-hooks)
      (add-hook 'post-command-hook 'btt--run-hooks)
      (push widget btt--widgets))))


(provide 'btt)

;;; btt.el ends here
