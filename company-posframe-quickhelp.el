;;; company-posframe-quickhelp.el --- Let company-quickhelp use posframe

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/company-posframe
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company-posframe "0.1"))

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

;; * company-posframe-quickhelp README                                :README:

;; #+BEGIN_EXAMPLE
;; (require 'company-quickhelp)
;; (require 'company-posframe)
;; (require 'company-posframe-quickhelp)
;; #+END_EXAMPLE

;;; Code:
;; * company-posframe-quickhelp's code
(require 'company-quickhelp)
(require 'company-posframe)

(defface company-posframe-quickhelp
  '((t :inherit default))
  "Face for company-posframe-quickhelp doc.
Fix: need improve.")

(defvar company-posframe-quickhelp-posframe-buffer " *company-posframe-quickhelp-buffer*"
  "The buffer which used by company-posframe-quickhelp.")

(defvar company-posframe-quickhelp-show-params
  (list :internal-border-width 1
        :timeout 15
        :internal-border-color "gray50"
        :no-properties nil
        :poshandler nil)
  "List of parameters passed to `posframe-show'.")

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

(defun company-posframe-quickhelp--show ()
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
          (with-current-buffer (get-buffer-create company-posframe-quickhelp-posframe-buffer)
            (setq-local header-line-format header-line))
          (apply #'posframe-show
                 company-posframe-quickhelp-posframe-buffer
                 :string (propertize doc 'face 'company-posframe-quickhelp)
                 :min-width (length header-line)
                 :min-height height
                 :height height
                 :respect-header-line t
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

(defun company-posframe-quickhelp-scroll-up (&optional arg)
  (interactive "^P")
  (when (posframe-workable-p)
    (posframe-funcall company-posframe-quickhelp-posframe-buffer
                      'scroll-up-command arg)))

(defun company-posframe-quickhelp-scroll-down (&optional arg)
  (interactive "^P")
  (when (posframe-workable-p)
    (posframe-funcall company-posframe-quickhelp-posframe-buffer
                      'scroll-down-command arg)))

(defun company-posframe-quickhelp-hide (&optional arg)
  (interactive "^P")
  (when (posframe-workable-p)
    (posframe-hide company-posframe-quickhelp-posframe-buffer)))

(define-key company-active-map (kbd "<f1>") 'company-posframe-quickhelp-scroll-up)
(define-key company-active-map (kbd "<f2>") 'company-posframe-quickhelp-scroll-down)
(define-key company-active-map (kbd "<f3>") 'company-posframe-quickhelp-hide)

(advice-add 'company-quickhelp-frontend :override #'company-posframe-quickhelp-frontend)
(advice-add 'company-quickhelp--show :override #'company-posframe-quickhelp--show)


(provide 'company-posframe-quickhelp)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-posframe-quickhelp.el ends here
