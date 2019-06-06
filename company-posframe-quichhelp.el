;;; company-posframe-quickhelp.el --- quickhelp support for company-posframe

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Clément Pit-Claudel, Feng Shu
;; Maintainer: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/company-posframe
;; Version: 0.1.0
;; Keywords: abbrev, convenience, matching
;; Package-Requires: ((emacs "26.0")(company "0.9.0")(posframe "0.1.0"))

;; This file is not part of GNU Emacs.

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

;; ** How to use company-posframe-quickhelp

;; #+BEGIN_EXAMPLE
;; (require 'company-posframe-quickhelp)
;; (company-posframe-quickhelp-enable)
;; (company-quickhelp-mode 1)
;; #+END_EXAMPLE

;;; Code:
;; * company-posframe-quickhelp's code
(require 'cl-lib)
(require 'company-posframe)
(require 'company-quickhelp)

(defvar company-posframe-quickhelp-buffer " *company-posframe-quickhelp-buffer*"
  "company-posframe's quickhelp buffer which used by posframe.")

(defvar company-posframe-quickhelp-show-params
  (list :internal-border-width 3
        :timeout 300
        :internal-border-color "gray50"
        :no-properties nil
        ;; If user want to show quickhelp at point
        ;; set :poshandler to nil
        :poshandler #'posframe-poshandler-window-top-right-corner)
  "List of parameters passed to `posframe-show'.")

(defun company-posframe--quickhelp-show ()
  (when (posframe-workable-p)
    (company-quickhelp--cancel-timer)
    (while-no-input
      (let* ((selected (nth company-selection company-candidates))
             (doc (let ((inhibit-message t))
                    (company-quickhelp--doc selected))))
        (when(> (length doc) 0)
          (apply #'posframe-show
                 company-posframe-quickhelp-buffer
                 :string (propertize doc 'face '(:height 100))
                 :position
                 (with-current-buffer company-posframe-buffer
                   (let ((pos posframe--last-posframe-pixel-position))
                     (cons (+ (car pos) (frame-pixel-width posframe--frame))
                           (cdr pos))))
                 :background-color company-quickhelp-color-background
                 :foreground-color company-quickhelp-color-foreground
                 company-posframe-quickhelp-show-params))))))

(defun company-posframe-quickhelp-frontend (command)
  (pcase command
    (`post-command (when company-quickhelp-delay
                     (company-quickhelp--set-timer))
                   (company-posframe--quickhelp-hide))
    (`hide
     (when company-quickhelp-delay
       (company-quickhelp--cancel-timer))
     (company-quickhelp--hide))))

(defun company-posframe--quickhelp-hide ()
  (when (posframe-workable-p)
    (posframe-hide company-posframe-quickhelp-buffer)))

(defun company-posframe-quickhelp-window-change ()
  "Hide posframe on window change."
  (unless (equal company-posframe--last-status
                 (list (selected-window)
                       (current-buffer)))
    (company-posframe--quickhelp-hide)))

(defun company-posframe-quickhelp-enable ()
  "Enable company-posframe-quickhelp"
  (advice-add #'company-quickhelp-frontend
              :override #'company-posframe-quickhelp-frontend)
  (advice-add #'company-quickhelp--show
              :override #'company-posframe--quickhelp-show)
  (advice-add #'company-quickhelp--hide
              :override #'company-posframe--quickhelp-hide)
  (add-hook 'window-configuration-change-hook #'company-posframe-quickhelp-window-change)
  t)

(provide 'company-posframe-quickhelp)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; company-posframe-quickhelp.el ends here
