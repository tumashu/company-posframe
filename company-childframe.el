;;; company-childframe.el --- Use a child-frame as company candidate menu

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

;; Author: Clément Pit-Claudel, Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/company-mode/company-mode
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company "0.9.0")(posframe "0.1.0"))

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

;; * company-childframe README                                :README:
;; ** What is company-childframe
;; company-childframe is a company extension, which let company use
;; child frame as its candidate menu.

;; It has the following feature:
;; 1. It is fast enough for daily use.
;; 2. It works well with CJK language.

;; *At the moment*, company-childframe can not work well with:
;; 1. company-quickhelp

;; ** How to use company-childframe

;; #+BEGIN_EXAMPLE
;; (require 'company-childframe)
;; (company-childframe-mode 1)
;; #+END_EXAMPLE

;; ** Tips
;; *** Work better with desktop.el
;; The below code let desktop.el not record the company-childframe-mode
;; #+BEGIN_EXAMPLE
;; (require 'desktop) ;this line is needed.
;; (push '(company-childframe-mode . nil)
;;       desktop-minor-mode-table)
;; #+END_EXAMPLE

;; ** Note
;; company-childframe.el is derived from Clément Pit-Claudel's
;; company-tooltip.el, which can be found at:

;; https://github.com/company-mode/company-mode/issues/745#issuecomment-357138511


;;; Code:
;; * company-childframe's code
(require 'cl-lib)
(require 'company)
(require 'posframe)

(defgroup company-childframe nil
  "Use a child-frame as company candidate menu"
  :group 'company
  :prefix "company-childframe")

(defcustom company-childframe-font nil
  "The font used by company-childframe's frame.
Using current frame's font if it it nil."
  :group 'company-childframe)

(defcustom company-childframe-lighter " company-childframe"
  "The lighter string used by `company-childframe-mode'."
  :group 'company-childframe)

(defvar company-childframe-buffer " *company-childframe-buffer*"
  "Company-childframe's buffer which used by posframe.")

(defvar company-childframe-notification "")

(defvar company-childframe-active-map
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

(defun company-childframe-show ()
  "Show company-childframe candidate menu."
  (let* ((height (min company-tooltip-limit company-candidates-length))
         (lines (company--create-lines company-selection height))
         (contents (mapconcat #'identity lines "\n"))
         (buffer (get-buffer-create company-childframe-buffer)))
    ;; FIXME: Do not support mouse at the moment, so remove mouse-face
    (setq contents (copy-sequence contents))
    (remove-text-properties 0 (length contents) '(mouse-face nil) contents)
    (with-current-buffer buffer
      (setq-local overriding-local-map company-childframe-active-map))
    (posframe-show buffer
                   :string contents
                   :position (- (point) (length company-prefix))
                   :x-pixel-offset (* -1 company-tooltip-margin (default-font-width))
                   :font company-childframe-font
                   :min-width company-tooltip-minimum-width
                   :background-color (face-attribute 'company-tooltip :background))))

(defun company-childframe-hide ()
  "Hide company-childframe candidate menu."
  (posframe-hide company-childframe-buffer))

(defun company-childframe-frontend (command)
  "`company-mode' frontend using child-frame.
COMMAND: See `company-frontends'."
  (cl-case command
    (pre-command nil)
    (show (company-childframe-show))
    (hide (company-childframe-hide))
    (update (company-childframe-show))
    (post-command (company-childframe-show))))

(defun company-childframe-unless-just-one-frontend (command)
  "`company-childframe-frontend', but not shown for single candidates."
  (if (company--show-inline-p)
      (company-childframe-hide)
    (company-childframe-frontend command)))

;;;###autoload
(define-minor-mode company-childframe-mode
  "Company-childframe minor mode."
  :global t
  :require 'company-childframe
  :group 'company-childframe
  :lighter company-childframe-lighter
  (if company-childframe-mode
      (progn
        (advice-add #'company-pseudo-tooltip-frontend :override #'company-childframe-frontend)
        (advice-add #'company-pseudo-tooltip-unless-just-one-frontend :override #'company-childframe-unless-just-one-frontend)
        ;; When user switches window, child-frame should be hidden.
        (add-hook 'window-configuration-change-hook #'company-childframe-hide)
        (message company-childframe-notification))
    (posframe-delete company-childframe-buffer)
    (advice-remove #'company-pseudo-tooltip-frontend #'company-childframe-frontend)
    (advice-remove #'company-pseudo-tooltip-unless-just-one-frontend #'company-childframe-unless-just-one-frontend)
    (remove-hook 'window-configuration-change-hook #'company-childframe-hide)))

(provide 'company-childframe)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-childframe.el ends here
