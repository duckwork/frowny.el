;;; frowny.el --- insert frownies better             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Case Duckworth

;; Author: Case Duckworth <acdw@acdw.net>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; For alphapapa, so he can insert frownies with electric-pair-mode.

;; This package ships `frowny-mode', which enables inserting a single "(" when
;; after a ":" -- meaning it'll insert :( without closing the paren.

;;; Code:

(defgroup frowny nil
  "Customization group for `frowny'."
  :group 'convenience)

(defcustom frowny-eyes "[:=]"
  "Regex to match frowny eyes to."
  :type 'string)

(defun frowny-self-insert (N)
  "Insert a frowny, or insert the character \"(\" N times."
  (interactive "p")  
  (cond
   ((looking-back frowny-eyes 1)
    (dotimes (_ N)
      (insert "(")))
   (t (self-insert-command N ?\())))

;;;###autoload
(define-minor-mode frowny-mode
  "Minor mode for inserting frownies."
  :init t
  :lighter ":("
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "(" #'frowny-self-insert)
            map))

(provide 'frowny)
;;; frowny.el ends here
