;;; frowny.el --- Insert frownies easily -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Case Duckworth

;; Author: Case Duckworth <acdw@acdw.net>
;; Homepage: https://github.com/duckwork/frowny.el
;; Keywords: convenience

;; Package-Version: 0.2
;; Package-Requires: ((emacs "24.1"))

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

;;; Customization

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

;;; Functions

(defun frowny--insert-character (character number pred)
  "Insert CHARACTER NUMBER times, depending on frowniness.
Frowniness is determined by whether CHARACTER is proceeded by any
of the regexen in `frowny-eyes', and if PRED is non-nil.

 Internal: use `frowny-self-insert-frowny' or
`frowny-self-insert-smiley' instead."
  (cond
   ((and pred
         (looking-back frowny-eyes frowny-eyes-looking-back-limit))
    (dotimes (_ number)
      (insert character)))
   (t (self-insert-command number character))))

(defun frowny-self-insert-frowny (N)
  "Insert \"(\" N times to complete a frowny."
  (interactive "p")
  (frowny--insert-character ?\( N t))

(define-obsolete-function-alias 'frowny-self-insert 'frowny-self-insert-frowny
  "0.2"
  "Insert a frowny, or insert the character \"(\" N times.")

(defun frowny-self-insert-smiley (N)
  "Insert \")\" N times to complete a smiley."
  (interactive "p")
  (frowny--insert-character ?\) N t))

(defun frowny--in-comment-p (&optional pos)
  "Return t if POS (default: point) is in comment, else nil."
  (and (nth 4 (syntax-ppss pos))
       t))

(defun frowny--in-string-p (&optional pos)
  "Return t if POS (default: point) is in string, else nil."
  (and (nth 3 (syntax-ppss pos))
       t))

(defun frowny-prog-insert-frowny (N)
  "Insert \"(\" N times to complete a frowny in a string or comment."
  (interactive "p")
  (frowny--insert-character ?\( N (or (frowny--in-comment-p)
                                      (frowny--in-string-p))))

(defun frowny-prog-insert-smiley (N)
  "Insert \")\" N times to complete a smiley in a string or comment."
  (interactive "p")
  (frowny--insert-character ?\) N (or (frowny--in-comment-p)
                                      (frowny--in-string-p))))

;;; Modes

(defun frowny-mode--turn-on ()
  "Turn on function `frowny-mode'."
  (if (derived-mode-p 'prog-mode)
      (frowny-mode +1)
    (frowny-prog-mode +1)))

;;;###autoload
(define-minor-mode frowny-mode
  "Minor mode for inserting frownies."
  :init t
  :lighter ":("
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "(" #'frowny-self-insert-frowny)
            (define-key map ")" #'frowny-self-insert-smiley)
            map))

;;;###autoload
(define-minor-mode frowny-prog-mode
  "Minor mode for inserting frownies in comments and strings only."
  :init t
  :lighter "=("
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "(" #'frowny-prog-insert-frowny)
            (define-key map ")" #'frowny-prog-insert-smiley)
            map))

;;;###autoload
(define-globalized-minor-mode global-frowny-mode
  frowny-mode
  frowny-mode--turn-on
  :predicate frowny-modes)

(provide 'frowny)
;;; frowny.el ends here
