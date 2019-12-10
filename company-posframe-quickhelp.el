;;; company-posframe-quickhelp.el --- Let company-quickhelp use posframe

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/company-posframe
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company "0.9.0")(posframe "0.1.0")(company-quickhelp "0.1.0"))

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
;; (require 'company-posframe-quickhelp)
;; #+END_EXAMPLE

;;; Code:
;; * company-posframe-quickhelp's code
(require 'company-quickhelp)
(require 'posframe)

(defvar company-posframe-quickhelp-posframe-buffer " *company-posframe-quickhelp-buffer*"
  "The buffer which used by company-posframe-quickhelp.")

(defvar company-posframe-quickhelp-show-params
  (list :internal-border-width 3
        :timeout 300
        :internal-border-color "gray50"
        :no-properties nil
        :poshandler nil)
  "List of parameters passed to `posframe-show'.")

(defun company-posframe-quickhelp-frontend (command)
  "Advice function of `company-quickhelp-frontend'."
  (pcase command
    (`post-command (when company-quickhelp-delay
                     (company-quickhelp--set-timer))
                   (when (posframe-workable-p)
                     (posframe-hide company-posframe-quickhelp-posframe-buffer)))
    (`hide
     (when company-quickhelp-delay
       (company-quickhelp--cancel-timer))
     (when (posframe-workable-p)
       (posframe-hide company-posframe-quickhelp-posframe-buffer)))))

(defun company-posframe-quickhelp--show ()
  "Advice function of `company-quickhelp--show'."
  (when (posframe-workable-p)
    (company-quickhelp--cancel-timer)
    (while-no-input
      (let* ((selected (nth company-selection company-candidates))
             (ovl company-pseudo-tooltip-overlay)
             (overlay-width
              (* (frame-char-width)
                 (if ovl (overlay-get ovl 'company-width) 0)))
             (overlay-position
              (* (frame-char-width)
                 (- (if ovl (overlay-get ovl 'company-column) 1) 1)))
             (doc (let ((inhibit-message t))
                    (company-quickhelp--doc selected)))
             (company-posframe-p
              (and (featurep 'company-posframe)
                   company-posframe-mode)))
        (when doc
          (apply #'posframe-show
                 company-posframe-quickhelp-posframe-buffer
                 :string (propertize doc 'face '(:height 100))
                 :background-color company-quickhelp-color-background
                 :foreground-color company-quickhelp-color-foreground
                 :position
                 (if company-posframe-p
                     (with-current-buffer company-posframe-buffer
                       (let ((pos posframe--last-posframe-pixel-position))
                         (cons (+ (car pos) (frame-pixel-width posframe--frame))
                               (cdr pos))))
                   (overlay-start ovl))
                 :x-pixel-offset
                 (unless company-posframe-p
                   (+ overlay-width overlay-position))
                 company-posframe-quickhelp-show-params))))))

(advice-add 'company-quickhelp-frontend :override #'company-posframe-quickhelp-frontend)
(advice-add 'company-quickhelp--show :override #'company-posframe-quickhelp--show)


(provide 'company-posframe-quickhelp)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-posframe-quickhelp.el ends here
