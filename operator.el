;;; operator.el --- Smart whitespace insertion around operators

;; Copyright (C) 2014 Jim Tian

;; Author: Jim Tian <tianjin.sc@gmail.com>
;; URL: https://github.com/toctan/operator.el
;; Version: 0.1.0
;; Keywords: convenience, operator, whitespace

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;
;;; Code:

(defvar op/operators nil
  "List of operator definitions.")

(defun op/get-position (operator)
  "Get whitespace position from OPERATOR pair."
  (let ((pos (cdr (op/get-operator operator))))
    (if (functionp pos) (funcall pos) pos)))

(defun op/get-operator (operator)
  "Get the OPERATOR definition from `op/operators'."
  (or (op/get-mode-operator operator)
      (op/get-global-operator operator)))

(defun op/get-mode-operator (operator)
  "Get the local OPERATOR in current major mode."
  (let ((mode-operators (assq major-mode op/operators)))
    (op/get--operator operator mode-operators)))

(defun op/get-global-operator (operator)
  "Get the global OPERATOR."
  (let ((global-operators (assq t op/operators)))
    (op/get--operator operator global-operators)))

(defun op/get--operator (operator operators)
  "Get whitespace position with OPERATOR from OPERATORS."
  (assq operator operators))

(defun op/after-whitespace-p ()
  "Return t if the `preceding-char' is whitespace."
  (eq (preceding-char) ?\s))

(defun op/before-whitespace-p ()
  "Return t if the `following-char' is whitespace."
  (eq (following-char) ?\s))

(defun op/point-inside-string-p ()
  "Return t if point is inside a string."
  (nth 3 (syntax-ppss)))

(defun op/remove-extra-whitespace ()
  "Remove the extra whitespace between operators."
  (let* ((char (char-before (- (point) 1)))
         (operator (op/get-operator char)))
    (if operator (delete-char -1))))

(defun op/insert-whitespace-before ()
  "Insert whitespace before the target operator."
  (save-excursion
    (backward-char)
    (if (op/after-whitespace-p)
        (op/remove-extra-whitespace)
      (insert " "))))

(defun op/insert-whitespace-after ()
  "Insert whitespace after the target operator."
  (if (op/before-whitespace-p)
      (forward-char)
    (insert " ")))

(defun op/insert-whitespace-around ()
  "Insert whitespace around the target operator."
  (op/insert-whitespace-before)
  (op/insert-whitespace-after))

(defun op/post-self-insert-function ()
  "The function that actually insert the whitespaces."
  (when (not (op/point-inside-string-p))
    (let ((position (op/get-position last-command-event)))
      (pcase position
        (`before (op/insert-whitespace-before))
        (`after (op/insert-whitespace-after))
        (`around (op/insert-whitespace-around))))))

;;;###autoload
(define-minor-mode operator-mode
  "Automatically insert whitespaces before, around or after some chars.
With a prefix argument ARG, enable Operator mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :group 'operator
  :lighter " +"
  (if operator-mode
      (add-hook 'post-self-insert-hook
                #'op/post-self-insert-function)
    (remove-hook 'post-self-insert-hook
                 #'op/post-self-insert-function)))

(provide 'operator)
;;; operator.el ends here
