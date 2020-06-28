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

;; Send commands to a BetterTouchTool Webserver.
;;
;; (see URL `https://docs.bettertouchtool.net/')
;; (see URL `https://docs.bettertouchtool.net/docs/1104_webserver.html')


;;; Code:

;; Structures that represent a TouchBar widget and its attributes
(cl-defstruct btt--widget name uuid args func)
(cl-defstruct btt--widget-face text icon_data icon_path background_color font_color font_size)

(defvar btt--widgets nil "List of widgets to be processed by `btt--run-functions'.")
(eval-when-compile (defvar btt--current-widget nil "The widget currently being processed by `btt--run-functions'."))

(cl-defstruct (btt--api-url (:include url)) query-string shared_secret) ; A URL with separate query-string and shared_secret slots
(setq btt--url (make-btt--api-url))


;;;; Customization
(defgroup btt nil
  "BetterTouchTool web server"
  :tag "BetterTouchTool"
  :group 'external
  :link '(url-link :tag "Integrated Webserver" "https://docs.bettertouchtool.net/docs/1104_webserver.html")
  :link '(url-link :tag "BetterTouchTool Documentation" "https://docs.bettertouchtool.net/"))

(defcustom btt-protocol "http"
  "Protocol for the BetterTouchTool web server."
  :tag "BTT protocol"
  :set (lambda (s v) (set-default s v) (setf (url-type btt--url) v))
  :type '(choice
	  (const :tag "http" :value "http")
	  (const :tag "https" :value "https")))

(defcustom btt-server "localhost"
  "Name or IP address of the BetterTouchTool server."
  :tag "BTT server"
  :set (lambda (s v) (set-default s v) (setf (url-host btt--url) v))
  :type 'string)

(defcustom btt-port 52888
  "Port the BetterTouchTool web server is running on."
  :tag "BTT port"
  :set (lambda (s v) (set-default s v) (setf (url-port btt--url) v))
  :type 'integer)

(defcustom btt-shared-secret ""
  "Shared secret for the BetterTouchTool web server."
  :tag "BTT shared secret"
  :set (lambda (s v) (set-default s v) (setf (btt--api-url-shared_secret btt--url) v))
  :type 'string)

(defcustom btt-uuids nil
  "List of BetterTouchTool variables and their corresponding UUIDs"
  :tag "BTT UUIDs"
  :set (lambda (s v) (set-default s v) (dolist (x v) (if (plist-member v x) (btt-set-string-variable x (plist-get v x)))))
  :initialize (lambda (s v) (custom-initialize-set s v))
  :type '(plist :key-type (string :tag "Name") :value-type (string :tag "UUID")))

(defconst btt-default-fg "white" "Default widget foreground color")
(defconst btt-default-bg "gray29" "Default widget background color")
(defconst btt-default-font-size 15 "Default widget font size")

(defun btt-customize ()
  (interactive)
  (customize-group 'btt t))


;;;; Helper functions
(defun btt--rgb-to-btt (color &optional transparency)
  "Convert COLOR to the RGB format used by BetterTouchTool.  The
default transparency is 255.

If COLOR is not a named color or an RGB triplet then return COLOR."

  (unless transparency (setq transparency "255"))

  (if (and color (xw-color-defined-p color))
      (concat (number-to-string (* (car (color-name-to-rgb color)) 255)) ","
	      (number-to-string (* (cadr (color-name-to-rgb color)) 255)) ","
	      (number-to-string (* (caddr (color-name-to-rgb color)) 255)) ","
	      transparency)
    (concat color "," transparency)))

(defun btt--remove-function (widget)
  "Remove the function associated with the BetterTouchTool WIDGET.

WIDGET is the name of a variable in BetterTouchTool whose value
is the Touch Bar widget's UUID."
  (dolist (i btt--widgets btt--widgets)
    (when (string= (btt--widget-name i) widget)
      (setq btt--widgets (remove i btt--widgets)))))

(defun btt-unwatch (var)
  "Remove the `btt--variable-watcher' variable watcher for VAR."
  (interactive
   (completing-read "Emacs variable to unwatch: " obarray 'boundp t))
  (put var 'btt--uuid nil)
  (put var 'btt--face nil)
  (remove-variable-watcher var 'btt--variable-watcher))

(defun btt--variable-watcher (symbol newval operation where)
  "Function `btt-watch' uses for its call to `add-variable-watcher'.

SYMBOL will be the VAR given to `btt-watch'.  NEWVAL will have
btt--uuid and btt--face properties, as added by `btt-watch'."
  (ignore operation where)
  (let ((uuid (get symbol 'btt--uuid))
	(face (get symbol 'btt--face)))
    (btt-update-touch-bar-widget uuid newval (if (functionp face) (funcall face) face))))

(defun btt-face (&rest args)
  "Create a set of attributes for widgets that BetterTouchTool can modify.
ARGS is a set of keywords and their values.  Allowed keywords are
:text, :icon_data, :icon_path, :fg, :bg, and :font_size.

:fg and :bg should be a color name or an RGB triplet string.  See
`color-name-to-rgb'.

Example: (btt-face :fg \"white\" :bg \"red\" :font_size 12."
  (make-btt--widget-face
   :text (plist-get args :text)
   :icon_data (plist-get args :icon_data)
   :icon_path (plist-get args :icon_path)
   :font_color (btt--rgb-to-btt (plist-get args :fg))
   :background_color (btt--rgb-to-btt (plist-get args :bg))
   :font_size (plist-get args :font_size)))


;;;; BTT APIs
(defun btt--call-api (api query-string &optional async)
  "Call the BetterTouchTool API with QUERY-STRING parameters.

QUERY-STRING is a list of lists to be given to
 `url-build-query-string'.  Eg., (list (list \"variableName\"
 varname) (list \"to\" newval))

If ASYNC is non-nil use `use-retrieve'.  The default is to use
`url-retrieve-synchronously'."
  (push (list "shared_secret" (btt--api-url-shared_secret btt--url)) query-string)
  (setf (url-filename btt--url) (concat "/" api "/?" (url-build-query-string query-string)))
  (if async
      (url-retrieve btt--url #'(lambda (status) (kill-buffer) status) nil t t)
    (url-retrieve-synchronously btt--url t t 2)))

;;;###autoload
(defun btt-get-string-variable (var)
  "Get the value of the BetterTouchTool variable VAR."
  (interactive "sBTT variable: ")
  (let ((buffer (btt--call-api "get_string_variable" (list (list "variableName" var)))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((x (thing-at-point 'sexp)))
	(kill-buffer buffer)
	x))))
 
;;;###autoload
(defun btt-set-string-variable (var value)
  "Set the VALUE of the BetterTouchTool variable VAR."
  (interactive "sBTT variable: \nsNew value for %s: ")
  (kill-buffer (btt--call-api
		"set_persistent_string_variable"
		(list (list "variableName" var) (list "to" value)))))

(defun btt-set-number-variable (var value)
  "Set the VALUE of the BetterTouchTool variable VAR."
  (interactive "sBTT variable: \nnNew value for %s: ")
  (kill-buffer (btt--call-api
		"set_persistent_number_variable"
		(list (list "variableName" var) (list "to" value)))))

(defun btt-trigger-named (name)
  "Trigger NAME."
  (interactive "sTrigger name: ")
  (kill-buffer (btt--call-api "trigger_named" (list (list "trigger_name" name)))))

(defun btt-execute-assigned-actions-for-trigger (uuid)
  "Execute all the assigned actions for a given trigger identified by its UUID."
  (interactive "sTrigger UUID: ")
  (kill-buffer (btt--call-api "execute_assigned_actions_for_trigger" (list (list "uuid" uuid)))))

(defun btt-refresh-widget (uuid)
  "Execute all scripts assigned to a script widget and update its
contents accordingly.  The widget is identified by its UUID."
  (interactive "sWidget UUID: ")
  (kill-buffer (btt--call-api "refresh_widget" (list (list "uuid" uuid)))))

(defun btt-get-trigger (uuid)
  "Return the JSON description for given trigger with the specified UUID."
  (interactive "sTrigger UUID: ")
  (let ((buffer (btt--call-api "get_trigger" (list (list "uuid" uuid)))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((x (thing-at-point 'sexp)))
	(kill-buffer buffer)
	x))))

(defun btt-update-touch-bar-widget (uuid &optional text face)
  "Tell BetterTouchTool to update a Touch Bar widget, identified by UUID.

The optional arguments TEXT and FACE determine which attributes
of the widget are updated.  See `btt--widget-face' for a list of
attributes that can be set.

Note that `btt--widget-face' includes a :text slot.  TEXT takes
precedence if it is non-nil."
  (let ((qs (list (list "uuid" uuid) (if text (list "text" (format "%s" text))))))
    (if (btt--widget-face-p face)	; Convert face to query string parameters
	(dolist (slot (cl-struct-slot-info 'btt--widget-face))
	  (let ((slot-name (car slot)))
	    (unless (string= slot-name "cl-tag-slot")
	      (let ((slot-value (cl-struct-slot-value 'btt--widget-face slot-name face)))
		(and slot-value (push (list slot-name slot-value) qs)))))))
    (btt--call-api "update_touch_bar_widget" qs t)))


;;;; Connecting functions to widgets
(defun btt--run-functions ()
  "Call the functions stored in `btt--widgets'."
  (dolist (btt--current-widget btt--widgets)
    (eval (btt--widget-func btt--current-widget))))

;;;###autoload
(defun btt-assign-func (widget func &optional args)
  "Use FUNC to update the Touch Bar WIDGET.

FUNC may be any valid lisp expression; it will be processed by
`eval'.  The string representation of its result will become the
text of the widget.  ARGS is an optional parameter to be
interpreted by FUNC.

WIDGET is the name of a variable in `btt-uuids' whose value is
the Touch Bar widget's UUID.

Avoid expensive processing in FUNC as it gets called by
`post-command-hook' and `focus-in-hook'.

Returns the new `btt--widget'."
  (interactive
   (list
    (read-string "BTT variable name: ")
    (completing-read "Update with function: " obarray 'functionp 'confirm "btt/")))

  ;; One at a time
  (btt--remove-function widget)

  (let ((widget (make-btt--widget :name widget :uuid (lax-plist-get btt-uuids widget) :func func :args args)))
    (when (> (string-bytes (btt--widget-uuid widget)) 1)
      (add-hook 'focus-in-hook 'btt--run-functions)
      (add-hook 'post-command-hook 'btt--run-functions)
      (push widget btt--widgets) widget)))


;;;; Connecting variables to widgets

;;;###autoload
(defun btt-assign-var (widget var &optional face)
  "Update the Touch Bar WIDGET when VAR is set.

WIDGET is the name of a variable in `btt-uuids' whose value is
the uuid of the widget.

VAR is the Emacs variable whose string-value becomes the text of
the widget.

FACE and the uuid of WIDGET are added as properties to VAR, so
that `btt--variable-watcher' can pass them to
`btt-update-touch-bar-widget'.

Returns the current value of VAR.

Requires Emacs version 26.1 or later."
  (interactive
   (list
    (read-string "BTT variable name: ")
    (completing-read "Emacs variable to watch: " obarray 'boundp t)))

  ;; add-variable-watcher was added to Emacs in version 26.1
  (unless (version< emacs-version "26.1")
    ;; Save the uuid and face as properties of the variable
    (put var 'btt--uuid (lax-plist-get btt-uuids widget))
    (put var 'btt--face face)
    (add-variable-watcher var 'btt--variable-watcher)

    ;; Assign var's value to itself to trigger btt--variable-watcher
    (set var (eval var))))


;;;###autoload
(defun btt-set-widget (widget text &optional face)
  "Set the TEXT of the Touch Bar WIDGET with optional FACE
attributes.

WIDGET is the name of a variable in `btt-uuids' whose value is
the Touch Bar widget's UUID.

Returns t."
  (interactive
   (list
    (read-string "BTT variable name: ")
    (read-string "Widget text: ")))

  (btt-update-touch-bar-widget (lax-plist-get btt-uuids widget) text face)
  t)


;;;; Example functions that can be assigned to widgets with `btt-function'

(defun btt/major-mode ()
  "Show the major mode of the current buffer in a BetterTouchTool widget."
  (btt-update-touch-bar-widget (btt--widget-uuid btt--current-widget) (symbol-name major-mode)))

(defun btt/emacs-version ()
  "Show variable `emacs-version' in a BetterTouchTool widget."
  (btt-update-touch-bar-widget (btt--widget-uuid btt--current-widget) emacs-version))

(defun btt/update-text (text)
  "Generalized function to update a widget's text.

If TEXT is a `btt--widget-face' its `:text' slot is used.  Otherwise TEXT is
passed directly."
  (let ((x (if (btt--widget-face-p text) (btt--widget-face-text text) text)))
    (btt-update-touch-bar-widget (btt--widget-uuid btt--current-widget) x)))


(provide 'btt)

;;; btt.el ends here
