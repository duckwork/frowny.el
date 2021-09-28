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

(defcustom frowny-eyes "\\(?::-\\|[:=]\\)"
  "Regex to match frowny eyes to."
  :type 'string)

(defcustom frowny-eyes-looking-back-limit 2
  "Number of characters to limit in `frowny-self-insert' for eyes.
Recommend keeping this only as long as the longest alternative in
`frowny-eyes', to save memory; see `looking-back' for more
details."
  :type 'integer)

(defcustom frowny-modes t
  "Modes to enable function `frowny-mode' in.
With an value of t, enable it in all modes."
  :type '(choice (const :tag "All modes" t)
                 (repeat :tag "Modes" function)))

(defun frowny-self-insert (N)
  "Insert a frowny, or insert the character \"(\" N times."
  (interactive "p")  
  (cond
   ((looking-back frowny-eyes frowny-eyes-looking-back-limit)
    (dotimes (_ N)
      (insert "(")))
   (t (self-insert-command N ?\())))

(defun frowny-mode--turn-on ()
  "Turn on function `frowny-mode'."
  (frowny-mode +1))

;;;###autoload
(define-minor-mode frowny-mode
  "Minor mode for inserting frownies."
  :init t
  :lighter ":("
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "(" #'frowny-self-insert)
            map))

;;;###autoload
(define-globalized-minor-mode global-frowny-mode
  frowny-mode
  frowny-mode--turn-on
  :predicate frowny-modes)

(provide 'frowny)
;;; frowny.el ends here
