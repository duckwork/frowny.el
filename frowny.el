;;; frowny.el --- insert frownies better             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Case Duckworth

;; Author: Case Duckworth <acdw@acdw.net>
;; Keywords: convenience

;;; License:

;; Everyone is permitted to do whatever with this software, without
;; limitation.  This software comes without any warranty whatsoever,
;; but with two pieces of advice:

;; - Be kind to yourself.

;; - Make good choices.

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
