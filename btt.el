;;; btt.el --- Interface to BetterTouchTool -*- lexical-binding: t; -*-

;; Author: Andy Rosen <ajr@corp.mlfs.org>
;; Version: 1.0
;; Keywords: BetterTouchTool, MacOS
;; Homepage: https://github.com/ajrosen/emacs
;; Package-Requires: ((emacs "26.1"))

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

;; Send commands to a BetterTouchTool Webserver (see URL
;; `https://docs.bettertouchtool.net/', URL
;; `https://docs.bettertouchtool.net/docs/1104_webserver.html').


;;; Code:

;; Structures that represent a TouchBar widget and its attributes
(cl-defstruct btt--widget var uuid args sexp)
(cl-defstruct btt--widget-face text icon_data icon_path background_color font_color font_size)

(eval-when-compile (defvar btt--current-widget))
(defvar btt--widgets nil "List of widgets to be processed by `btt--run-hooks'.")

(defvar btt--current-buffer (buffer-name))
(add-hook 'post-command-hook
	  #'(lambda() (unless (minibufferp) (setq btt--current-buffer (buffer-name)))))

(cl-defstruct (btt--api-url (:include url)) query-string shared_secret)
(setq btt--url (make-btt--api-url))


;;;; Customization
(defgroup btt nil
  "BetterTouchTool"
  :group 'external
  :prefix "btt-"
  :link '(url-link :tag "Integrated Webserver" "https://docs.bettertouchtool.net/docs/1104_webserver.html")
  :link '(url-link :tag "BetterTouchTool Documentation" "https://docs.bettertouchtool.net/"))

(defcustom btt-protocol "http"
  "Protocol for the BetterTouchTool web server."
  :set (lambda (s v) (set-default s v) (setf (url-type btt--url) v))
  :type '(choice
	  (const :tag "http" :value "http")
	  (const :tag "https" :value "https")))

(defcustom btt-server "localhost"
  "Name or IP address of the BetterTouchTool server."
  :set (lambda (s v) (set-default s v) (setf (url-host btt--url) v))
  :type 'string)

(defcustom btt-port 52888
  "Port the BetterTouchTool web server is running on."
  :set (lambda (s v) (set-default s v) (setf (url-port btt--url) v))
  :type 'integer)

(defcustom btt-shared-secret ""
  "Shared secret for the BetterTouchTool web server."
  :set (lambda (s v) (set-default s v) (setf (btt--api-url-shared_secret btt--url) v))
  :type 'string)


;;;; Helper functions
(defun btt--rgb-to-btt (color)
  "Convert COLOR (a color name) to the RGB format used by BetterTouchTool."
  (if color
      (concat (number-to-string (* (car (color-name-to-rgb color)) 255)) ","
	      (number-to-string (* (cadr (color-name-to-rgb color)) 255)) ","
	      (number-to-string (* (caddr (color-name-to-rgb color)) 255)) ","
	      "255")))

(defun btt--remove (var)
  "Remove the widget linked to the BetterTouchTool variable VAR."
  (dolist (h btt--widgets btt--widgets)
    (when (string= (btt--widget-var h) var)
      (setq btt--widgets (remove h btt--widgets)))))

(defun btt--variable-watcher (symbol newval op where)
  "Function for `add-variable-watcher' in `btt-watch'."
  (ignore op where)
  (let ((uuid (get symbol 'btt--uuid))
	(face (get symbol 'btt--face)))
    (btt-update-touch-bar-widget uuid newval (if (functionp face) (funcall face) face))))

(defun btt-face (&rest args)
  "Create a set of attributes for widgets that BetterTouchTool can modify.
ARGS is a set of keywords and their values.  Allowed keywords are
:text, :icon_data, :icon_path, :fg, :bg, and :font_size.

:fg and :bg should be a color name or an RGB triplet string.  See
`color-name-to-rgb'."
  (make-btt--widget-face
   :text (plist-get args :text)
   :icon_data (plist-get args :icon_data)
   :icon_path (plist-get args :icon_path)
   :font_color (btt--rgb-to-btt (plist-get args :fg))
   :background_color (btt--rgb-to-btt (plist-get args :bg))
   :font_size (plist-get args :font_size)))


;;;; BTT APIs
(defun btt--call-api (api query-string &optional async)
  "Call the BetterTouchTool API with QUERY-STRING parameters.  If
ASYNC is non-nil use `use-retrieve'.  The default is to use
`url-retrieve-synchronously'."
  (push (list "shared_secret" (btt--api-url-shared_secret btt--url)) query-string)
  (setf (url-filename btt--url) (concat "/" api "/?" (url-build-query-string query-string)))
  (if async
      (url-retrieve btt--url #'(lambda (status) (kill-buffer) status) nil t t)
    (url-retrieve-synchronously btt--url t t 2)))
 
(defun btt-get-string-variable (var)
  "Get the value of the BetterTouchTool variable VAR."
  (interactive "sBTT variable: ")
  (let ((buffer (btt--call-api "get_string_variable" (list (list "variableName" var)))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((x (thing-at-point 'line)))
	(kill-buffer buffer)
	x))))

(defun btt-set-string-variable (var value)
  "Set the VALUE of the BetterTouchTool variable VAR."
  (interactive "sBTT variable: \nsNew value for %s: ")
  (kill-buffer (btt--call-api
		"set_persistent_string_variable"
		(list (list "variableName" var) (list "to" value)))))

(defun btt-update-touch-bar-widget (uuid &optional text face)
  "Tell BetterTouchTool to update a Touch Bar widget, identified by UUID.

The optional arguments TEXT and FACE determine which attributes
of the widget are updated.  See `btt--widget-face' for a list of
attributes that can be set.

Note that `btt--widget-face' includes a :text slot.  TEXT takes
precedence if it is non-nil."

  (setq qs (list (list "uuid" uuid) (if text (list "text" (format "%s" text)))))

  (if (btt--widget-face-p face)	; Convert face to query string parameters
      (dolist (slot (cl-struct-slot-info 'btt--widget-face))
	(setq slot-name (car slot))
	(unless (string= slot-name "cl-tag-slot")
	  (setq slot-value (cl-struct-slot-value 'btt--widget-face slot-name face))
	  (and slot-value (push (list slot-name slot-value) qs)))))
  (btt--call-api "update_touch_bar_widget" qs t))


;;;; Hooks
(defun btt--run-hooks ()
  "Call the functions stored in `btt--widgets'."
  (dolist (btt--current-widget btt--widgets)
    (eval (btt--widget-sexp btt--current-widget))))


;;;; Functions that can be assigned to widgets
(defun btt/current-buffer ()
  "Show `buffer-name' in a BetterTouchTool widget.  The widget's
:args slot should be a list of two `btt--widget-face's.  The
first face is used for unmodified buffers and buffers that are
not visiting a file.  Otherwise the second face is used."
  (unless (minibufferp)
    (btt-update-touch-bar-widget
     (btt--widget-uuid btt--current-widget)
     (buffer-name)
     (if (and (buffer-file-name) (buffer-modified-p))
	 (cadr (btt--widget-args btt--current-widget))
       (car (btt--widget-args btt--current-widget))))))
 
(defun btt/major-mode ()
  "Show the major mode of the current buffer in a BetterTouchTool widget."
  (btt-update-touch-bar-widget (btt--widget-uuid btt--current-widget) (symbol-name major-mode)))

(defun btt/emacs-version ()
  "Show variable `emacs-version' in a BetterTouchTool widget."
  (btt-update-touch-bar-widget (btt--widget-uuid btt--current-widget) emacs-version))

(defun btt/update-widget (text)
  "Generalized function to update a widget's text.  If TEXT is a
`btt--widget-face' its `:text' slot is used.  Otherwise TEXT is
passed directly."
  (if (btt--widget-face-p text)
      (btt-update-touch-bar-widget (btt--widget-uuid btt--current-widget) (btt--widget-face-text text))
    (btt-update-touch-bar-widget (btt--widget-uuid btt--current-widget) text)))


;;;###autoload
(defun btt-add (var sexp &optional args)
  "Evaluate SEXP to update a Touch Bar widget through
BetterTouchTool.  SEXP may be any valid lisp expression.  The
result will become the text of the widget, using `format'.  ARGS
is an optional parameter to be interpreted by SEXP.

VAR is the name of a variable in BetterTouchTool whose
value is the Touch Bar widget's UUID.

SEXP is evaluated by `focus-in-hook' and `post-command-hook'."

  (interactive
   (list
    (read-string "BTT variable name: ")
    (completing-read "Update with function: " obarray 'functionp 'confirm "btt/")))

  (btt--remove var)

  (let ((widget (make-btt--widget :var var :uuid (btt-get-string-variable var) :sexp sexp :args args)))
    (when (> (string-bytes (btt--widget-uuid widget)) 1)
      (add-hook 'focus-in-hook 'btt--run-hooks)
      (add-hook 'post-command-hook 'btt--run-hooks)
      (push widget btt--widgets) widget)))

;;;###autoload
(defun btt-watch (var widget &optional face)
  "Update the Touch Bar WIDGET when VAR is set."
  (interactive
   (list
    (completing-read "Emacs variable to watch: " obarray 'boundp t)
    (read-string "BTT variable name: ")))

  (btt-unwatch var)
  (put var 'btt--uuid (btt-get-string-variable widget))
  (put var 'btt--face face)
  (add-variable-watcher var 'btt--variable-watcher)
  (set var (eval var)))

(defun btt-unwatch (var)
  "Remove the variable watcher for VAR."
  (interactive)
   (remove-variable-watcher var 'btt--variable-watcher))

;;;###autoload
(defun btt-update (var text &optional face)
  "Set the TEXT of the Touch Bar VAR."
  (interactive
   (list
    (read-string "BTT variable name: ")
    (read-string "Widget text: ")))

  (btt-update-touch-bar-widget (btt-get-string-variable var) text face)
  t)


(provide 'btt)


;;; btt.el ends here
