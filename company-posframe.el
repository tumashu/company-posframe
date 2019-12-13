;;; company-posframe.el --- Use a posframe as company candidate menu

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Clément Pit-Claudel, Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/company-posframe
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company "0.9.0")(posframe "0.1.0")(company-quickhelp "2.2.0"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; * company-posframe README                                :README:
;; ** What is company-posframe
;; company-posframe is a company extension, which let company use
;; child frame as its candidate menu.

;; It has the following feature:
;; 1. It is fast enough for daily use.
;; 2. It works well with CJK language.

;; *At the moment*, company-posframe can not work well with:
;; 1. company-quickhelp

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

;; https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511


;;; Code:
;; * company-posframe's code
(require 'cl-lib)
(require 'company)
(require 'company-quickhelp)
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

(defvar company-posframe-quickhelp-show-params
  (list :internal-border-width 1
        :timeout 15
        :internal-border-color "gray50"
        :no-properties nil
        :poshandler nil)
  "List of parameters passed to `posframe-show'.")

(defvar company-posframe-notification "")

(defvar company-posframe--last-status nil)

(defvar company-posframe-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [mouse-1] 'ignore)
    (define-key keymap [mouse-3] 'ignore)
    (define-key keymap [down-mouse-1] 'ignore)
    (define-key keymap [down-mouse-3] 'ignore)
    (define-key keymap [up-mouse-1] 'ignore)
    (define-key keymap [up-mouse-3] 'ignore)
    (define-key keymap [wheel-down] 'ignore)
    (define-key keymap [wheel-up] 'ignore)
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
      (setq-local overriding-local-map company-posframe-active-map)
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
  (setq company-posframe--last-status
        (list (selected-window)
              (current-buffer)))
  (cl-case command
    (pre-command nil)
    (hide (company-posframe-hide))
    (update (company-posframe-show))
    (post-command
     (when (not (string-match-p "^company-posframe-quickhelp-"
                                (symbol-name this-command)))
       (company-posframe-show)))))

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
              (equal company-posframe--last-status
                     (list (selected-window)
                           (current-buffer))))
    (company-posframe-hide)
    (company-posframe-quickhelp-hide)))

(defun company-posframe-quickhelp-frontend (command)
  "Advice function of `company-quickhelp-frontend'."
  (pcase command
    (`post-command
     (when (and company-quickhelp-delay
                (not (string-match-p "^company-posframe-quickhelp-"
                                     (symbol-name this-command))))
       (company-quickhelp--set-timer)
       (company-posframe-quickhelp-hide)))
    (`hide
     (when company-quickhelp-delay
       (company-quickhelp--cancel-timer))
     (company-posframe-quickhelp-hide))))

(defun company-posframe-quickhelp-show ()
  "Advice function of `company-quickhelp--show'."
  (when (posframe-workable-p)
    (company-quickhelp--cancel-timer)
    (while-no-input
      (let* ((selected (nth company-selection company-candidates))
             (doc (let ((inhibit-message t))
                    (company-quickhelp--doc selected)))
             (height
              (max (+ company-tooltip-limit
                      (if company-posframe-show-indicator 1 0)
                      (if company-posframe-show-metadata 1 0)
                      -1)
                   (with-current-buffer company-posframe-buffer
                     (- (frame-height posframe--frame) 1))))
             (background (face-attribute 'company-posframe-quickhelp :background nil t))
             (foreground (face-attribute 'company-posframe-quickhelp :foreground nil t))
             (header-line
              (substitute-command-keys
               (concat
                "## "
                "`\\[company-posframe-quickhelp-scroll-up]':Scroll-Up  "
                "`\\[company-posframe-quickhelp-scroll-down]':Scroll-Down  "
                "`\\[company-posframe-quickhelp-hide]':Hide "
                "##"))))
        (when doc
          (with-current-buffer (get-buffer-create company-posframe-quickhelp-buffer)
            (setq-local header-line-format header-line))
          (apply #'posframe-show
                 company-posframe-quickhelp-buffer
                 :string (propertize doc 'face 'company-posframe-quickhelp)
                 :min-width (length header-line)
                 :min-height height
                 :height height
                 :respect-header-line t
                 ;; When first show quickhelp's posframe, it seem using wrong height,
                 ;; maybe header-line's reason, just refresh again, ugly but useful :-).
                 :refresh 0.5
                 :background-color
                 (if (eq background 'unspecified)
                     company-quickhelp-color-background
                   background)
                 :foreground-color
                 (if (eq foreground 'unspecified)
                     company-quickhelp-color-foreground
                   foreground)
                 :position
                 (with-current-buffer company-posframe-buffer
                   (let ((pos posframe--last-posframe-pixel-position))
                     (cons (+ (car pos) (frame-pixel-width posframe--frame))
                           (cdr pos))))
                 company-posframe-quickhelp-show-params))))))

(defun company-posframe-quickhelp-hide (&optional arg)
  (interactive "^P")
  (when (posframe-workable-p)
    (posframe-hide company-posframe-quickhelp-buffer)))

(defun company-posframe-quickhelp-scroll-up (&optional arg)
  (interactive "^P")
  (when (posframe-workable-p)
    (posframe-funcall company-posframe-quickhelp-buffer
                      'scroll-up-command arg)))

(defun company-posframe-quickhelp-scroll-down (&optional arg)
  (interactive "^P")
  (when (posframe-workable-p)
    (posframe-funcall company-posframe-quickhelp-buffer
                      'scroll-down-command arg)))

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
          (advice-add 'company-quickhelp-frontend
                      :override #'company-posframe-quickhelp-frontend)
          (advice-add 'company-quickhelp--show
                      :override #'company-posframe-quickhelp-show)
          ;; Quickhelp keys.
          (define-key company-active-map (kbd "<f2>") 'company-posframe-quickhelp-scroll-up)
          (define-key company-active-map (kbd "<f3>") 'company-posframe-quickhelp-scroll-down)
          (define-key company-active-map (kbd "<f4>") 'company-posframe-quickhelp-hide)
          ;; When user switches window, child-frame should be hidden.
          (add-hook 'window-configuration-change-hook #'company-posframe-window-change)
          (message company-posframe-notification))
      (posframe-delete company-posframe-buffer)
      (posframe-delete company-posframe-quickhelp-buffer)
      (advice-remove #'company-pseudo-tooltip-frontend
                     #'company-posframe-frontend)
      (advice-remove #'company-pseudo-tooltip-unless-just-one-frontend
                     #'company-posframe-unless-just-one-frontend)
      (advice-remove 'company-quickhelp-frontend
                     #'company-posframe-quickhelp-frontend)
      (advice-remove 'company-quickhelp--show
                     #'company-posframe-quickhelp-show)
      (define-key company-active-map (kbd "<f2>") nil)
      (define-key company-active-map (kbd "<f3>") nil)
      (define-key company-active-map (kbd "<f4>") nil)
      (remove-hook 'window-configuration-change-hook #'company-posframe-window-change))))

(provide 'company-posframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-posframe.el ends here
