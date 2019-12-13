;;; company-posframe.el --- Use a posframe as company candidate menu

;; Copyright (C) 2017-2019 Clément Pit-Claudel, Feng Shu, Lars Andersen

;; Author: Clément Pit-Claudel, Feng Shu, Lars Andersen <expez@expez.com>
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/company-posframe
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company "0.9.0")(posframe "0.1.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; * company-posframe README                                :README:
;; ** What is company-posframe
;; company-posframe is a company extension, which let company use
;; child frame as its candidate menu.

;; It has the following feature:
;; 1. It is fast enough for daily use.
;; 2. It works well with CJK language.

;; [[./snapshots/company-posframe.png]]

;; ** How to use company-posframe

;; #+BEGIN_EXAMPLE
;; (require 'company-posframe)
;; (company-posframe-mode 1)
;; #+END_EXAMPLE

;; ** Tips
;; *** Work better with desktop.el
;; The below code let desktop.el not record the company-posframe-mode
;; #+BEGIN_EXAMPLE
;; (require 'desktop) ;this line is needed.
;; (push '(company-posframe-mode . nil)
;;       desktop-minor-mode-table)
;; #+END_EXAMPLE

;; ** Note
;; company-posframe.el is derived from Clément Pit-Claudel's
;; company-tooltip.el, which can be found at:

;; [[https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511]]

;; Some quickhelp functions is come from:

;; [[https://github.com/company-mode/company-quickhelp][company-quickhelp]]


;;; Code:
;; * company-posframe's code
(require 'cl-lib)
(require 'company)
(require 'posframe)

(defgroup company-posframe nil
  "Use a child-frame as company candidate menu"
  :group 'company
  :prefix "company-posframe")

(defcustom company-posframe-font nil
  "The font used by company-posframe's frame.
Using current frame's font if it it nil."
  :group 'company-posframe)

(defcustom company-posframe-lighter " company-posframe"
  "The lighter string used by `company-posframe-mode'."
  :group 'company-posframe)

(defcustom company-posframe-show-indicator t
  "Display an indicator for backends in the mode line of the posframe."
  :group 'company-posframe
  :type 'boolean)

(defcustom company-posframe-show-metadata t
  "Display metadata (e.g. signature) of the selection below the visible candidates."
  :group 'company-posframe
  :type 'boolean)

(defcustom company-posframe-backend-separator "|"
  "String used to separate entries in the backend indicator."
  :group 'company-posframe
  :type 'string)

(defcustom company-posframe-backend-format-function #'company-posframe-format-backend-name
  "Function used to format each backend in the indicator."
  :group 'company-posframe
  :type 'function)

(defcustom company-posframe-quickhelp-delay 0.5
  "Delay, in seconds, before the quickhelp popup appears.

If set to nil the popup won't automatically appear, but can still
be triggered manually using `company-posframe-quickhelp-show'."
  :type '(choice (number :tag "Delay in seconds")
                 (const :tag "Don't popup help automatically" nil)))

(defface company-posframe-inactive-backend-name
  '((t :inherit mode-line))
  "Face for the active backend name in the header line.")

(defface company-posframe-active-backend-name
  '((t :inherit mode-line-emphasis))
  "Face for the active backend name in the header line.")

(defface company-posframe-metadata
  '((t :inherit font-lock-comment-face))
  "Face for the metadata footer (not the backend indicator).")

(defface company-posframe-quickhelp
  '((t :inherit default))
  "Face for company-posframe-quickhelp doc.")

(defvar company-posframe-buffer " *company-posframe-buffer*"
  "company-posframe's buffer which used by posframe.")

(defvar company-posframe-quickhelp-buffer " *company-posframe-quickhelp-buffer*"
  "The buffer which used by company-posframe-quickhelp.")

(defvar-local company-posframe-quickhelp-timer nil
  "Quickhelp idle timer.")

(defvar company-posframe-quickhelp-show-params
  (list :internal-border-width 1
        :timeout 60
        :internal-border-color "gray50"
        :no-properties nil
        :poshandler nil)
  "List of parameters passed to `posframe-show'.")

(defvar company-posframe-notification "")

(defvar company-posframe-last-status nil)

(defvar company-posframe-active-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap company-active-map)
    (define-key keymap [mouse-1] 'ignore)
    (define-key keymap [mouse-3] 'ignore)
    (define-key keymap [down-mouse-1] 'ignore)
    (define-key keymap [down-mouse-3] 'ignore)
    (define-key keymap [up-mouse-1] 'ignore)
    (define-key keymap [up-mouse-3] 'ignore)
    (define-key keymap [wheel-down] 'ignore)
    (define-key keymap [wheel-up] 'ignore)

    ;; Quickhelp keys.
    (define-key keymap (kbd "<f1>") 'company-posframe-quickhelp-toggle)
    (define-key keymap (kbd "<f2>") 'company-posframe-quickhelp-scroll-up)
    (define-key keymap (kbd "<f3>") 'company-posframe-quickhelp-scroll-down)

    keymap)
  "Keymap that is enabled during an active completion in posframe.")

(defun company-posframe-format-backend-name (backend)
  "Format BACKEND for displaying in the modeline."
  (propertize (cl-typecase backend
                (symbol (string-remove-prefix "company-" (symbol-name backend)))
                (list (format "[%s]" (mapconcat #'company-posframe-format-backend-name backend "|")))
                (otherwise "-"))
              'face (if (equal backend company-backend)
                        'company-posframe-active-backend-name
                      'company-posframe-inactive-backend-name)))

(defun company-posframe-show ()
  "Show company-posframe candidate menu."
  (let* ((height (min company-tooltip-limit company-candidates-length))
         (meta (when company-posframe-show-metadata
                 (company-fetch-metadata)))
         (lines (company--create-lines company-selection height))
         (backend-names (when company-posframe-show-indicator
                          (mapconcat company-posframe-backend-format-function
                                     company-backends
                                     company-posframe-backend-separator)))
         (width (length (car lines)))
         (contents (concat (mapconcat #'identity lines "\n")
                           (if meta
                               (concat "\n" (propertize (if (> (length meta) width)
                                                            (substring meta 0 width)
                                                          meta)
                                                        'face 'company-posframe-metadata))
                             "")))
         (buffer (get-buffer-create company-posframe-buffer)))
    ;; FIXME: Do not support mouse at the moment, so remove mouse-face
    (setq contents (copy-sequence contents))
    (remove-text-properties 0 (length contents) '(mouse-face nil) contents)
    (with-current-buffer buffer
      (when company-posframe-show-indicator
        (setq-local mode-line-format `(,(substring backend-names 0
                                                   (min width (length backend-names)))))))
    (posframe-show buffer
                   :string contents
                   :position (- (point) (length company-prefix))
                   :min-height (+ height
                                  (if company-posframe-show-indicator 1 0))
                   :min-width width
                   :x-pixel-offset (* -1 company-tooltip-margin (default-font-width))
                   :respect-mode-line company-posframe-show-indicator
                   :font company-posframe-font
                   :min-width company-tooltip-minimum-width
                   :background-color (face-attribute 'company-tooltip :background))))

(defun company-posframe-hide ()
  "Hide company-posframe candidate menu."
  (posframe-hide company-posframe-buffer))

(defun company-posframe-frontend (command)
  "`company-mode' frontend using child-frame.
COMMAND: See `company-frontends'."
  (setq company-posframe-last-status
        (list (selected-window)
              (current-buffer)))
  (let ((run-quickhelp-command-p
         (string-match-p "^company-posframe-quickhelp-"
                         (symbol-name this-command))))
    (cl-case command
      (pre-command
       (when (and company-posframe-quickhelp-delay
                  (not run-quickhelp-command-p))
         (company-posframe-quickhelp-set-timer)
         (company-posframe-quickhelp-hide)))
      (hide
       (when company-posframe-quickhelp-delay
         (company-posframe-quickhelp-cancel-timer))
       (company-posframe-quickhelp-hide)
       (company-posframe-hide))
      (update
       ;; Important: This line is very important, we need to override
       ;; company-active-map again, this is do the job of
       ;; `company--perform'.
       (company-enable-overriding-keymap company-posframe-active-map)
       (company-posframe-show))
      (post-command
       (when (not run-quickhelp-command-p)
         (company-posframe-show))))))

(defun company-posframe-unless-just-one-frontend (command)
  "`company-posframe-frontend', but not shown for single candidates."
  (if (company--show-inline-p)
      (company-posframe-hide)
    (company-posframe-frontend command)))

(defun company-posframe-window-change ()
  "Hide posframe on window change."
  (unless (or (member (buffer-name)
                      (list company-posframe-buffer
                            company-posframe-quickhelp-buffer))
              (equal company-posframe-last-status
                     (list (selected-window)
                           (current-buffer))))
    (company-posframe-hide)
    (company-posframe-quickhelp-hide)))

(defun company-posframe-quickhelp-skip-footers-backwards ()
  "Skip backwards over footers and blank lines."
  (beginning-of-line)
  (while (and (not (= (point-at-eol) (point-min)))
              (or
               ;; [back] appears at the end of the help elisp help buffer
               (looking-at-p "\\[back\\]")
               ;; [source] cider's help buffer contains a link to source
               (looking-at-p "\\[source\\]")
               (looking-at-p "^\\s-*$")))
    (forward-line -1)))

(defun company-posframe-quickhelp-completing-read (prompt candidates &rest rest)
  "`cider', and probably other libraries, prompt the user to
resolve ambiguous documentation requests.  Instead of failing we
just grab the first candidate and press forward."
  (car candidates))

(defun company-posframe-quickhelp-fetch-docstring (backend)
  "Fetch docstring from BACKEND."
  (let ((quickhelp-str (company-call-backend 'quickhelp-string backend)))
    (if (stringp quickhelp-str)
        (with-temp-buffer
          (insert quickhelp-str)
          (company-posframe-quickhelp-skip-footers-backwards)
          (buffer-string))
      (let ((doc (company-call-backend 'doc-buffer backend)))
        (when doc
          (let* ((doc-buffer (if (consp doc) (car doc) doc))
                 (quickhelp-string
                  (with-current-buffer doc-buffer
                    (buffer-string))))
            (with-temp-buffer
              (insert quickhelp-string)
              (company-posframe-quickhelp-skip-footers-backwards)
              (buffer-string))))))))

(defun company-posframe-quickhelp-doc (selected)
  (cl-letf (((symbol-function 'completing-read)
             #'company-posframe-quickhelp-completing-read))
    (let* ((doc (company-posframe-quickhelp-fetch-docstring selected)))
      (unless (member doc '(nil ""))
        doc))))

(defun company-posframe-quickhelp-set-timer ()
  (when (or (null company-posframe-quickhelp-timer)
            (eq this-command #'company-posframe-quickhelp-manual-begin))
    (setq company-posframe-quickhelp-timer
          (run-with-idle-timer company-posframe-quickhelp-delay nil
                               'company-posframe-quickhelp-show))))

(defun company-posframe-quickhelp-cancel-timer ()
  (when (timerp company-posframe-quickhelp-timer)
    (cancel-timer company-posframe-quickhelp-timer)
    (setq company-posframe-quickhelp-timer nil)))

(defun company-posframe-quickhelp-show ()
  (company-posframe-quickhelp-cancel-timer)
  (while-no-input
    (let* ((selected (nth company-selection company-candidates))
           (doc (let ((inhibit-message t))
                  (company-posframe-quickhelp-doc selected)))
           (height
            (max (+ company-tooltip-limit
                    (if company-posframe-show-indicator 1 0)
                    (if company-posframe-show-metadata 1 0)
                    -1)
                 (with-current-buffer company-posframe-buffer
                   (- (frame-height posframe--frame) 1))))
           (header-line
            (substitute-command-keys
             (concat
              "## "
              "\\<company-posframe-active-map>\\[company-posframe-quickhelp-toggle]:Show/Hide  "
              "\\<company-posframe-active-map>\\[company-posframe-quickhelp-scroll-up]:Scroll-Up  "
              "\\<company-posframe-active-map>\\[company-posframe-quickhelp-scroll-down]:Scroll-Down "
              "##"))))
      (when doc
        (with-current-buffer (get-buffer-create company-posframe-quickhelp-buffer)
          (setq-local header-line-format header-line))
        (apply #'posframe-show
               company-posframe-quickhelp-buffer
               :string (propertize doc 'face 'company-posframe-quickhelp)
               :min-width (min 60 (length header-line))
               :min-height height
               :height height
               :respect-header-line t
               ;; When first show quickhelp's posframe, it seem using wrong height,
               ;; maybe header-line's reason, just refresh again, ugly but useful :-).
               :refresh 0.5
               :background-color (face-attribute 'company-posframe-quickhelp :background nil t)
               :foreground-color (face-attribute 'company-posframe-quickhelp :foreground nil t)
               :position
               (with-current-buffer company-posframe-buffer
                 (let ((pos posframe--last-posframe-pixel-position))
                   (cons (+ (car pos) (frame-pixel-width posframe--frame))
                         (cdr pos))))
               company-posframe-quickhelp-show-params)))))

(defun company-posframe-quickhelp-hide ()
  (posframe-hide company-posframe-quickhelp-buffer))

(defun company-posframe-quickhelp-toggle ()
  (interactive)
  (if (posframe-funcall
       company-posframe-quickhelp-buffer
       (lambda ()
         (frame-parameter (window-frame) 'visibility)))
      (company-posframe-quickhelp-hide)
    (company-posframe-quickhelp-show)))

(defun company-posframe-quickhelp-scroll-up (&optional arg)
  (interactive "^P")
  (posframe-funcall company-posframe-quickhelp-buffer
                    'scroll-up-command arg))

(defun company-posframe-quickhelp-scroll-down (&optional arg)
  (interactive "^P")
  (posframe-funcall company-posframe-quickhelp-buffer
                    'scroll-down-command arg))

;;;###autoload
(define-minor-mode company-posframe-mode
  "company-posframe minor mode."
  :global t
  :require 'company-posframe
  :group 'company-posframe
  :lighter company-posframe-lighter
  (if (not (posframe-workable-p))
      (message "company-posframe can not work in current emacs environment.")
    (if company-posframe-mode
        (progn
          (advice-add #'company-pseudo-tooltip-frontend
                      :override #'company-posframe-frontend)
          (advice-add #'company-pseudo-tooltip-unless-just-one-frontend
                      :override #'company-posframe-unless-just-one-frontend)
          ;; When user switches window, child-frame should be hidden.
          (add-hook 'window-configuration-change-hook #'company-posframe-window-change)
          (message company-posframe-notification))
      (posframe-delete company-posframe-buffer)
      (posframe-delete company-posframe-quickhelp-buffer)
      (advice-remove #'company-pseudo-tooltip-frontend
                     #'company-posframe-frontend)
      (advice-remove #'company-pseudo-tooltip-unless-just-one-frontend
                     #'company-posframe-unless-just-one-frontend)
      (company-posframe-quickhelp-cancel-timer)
      (remove-hook 'window-configuration-change-hook #'company-posframe-window-change))))

(provide 'company-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-posframe.el ends here
