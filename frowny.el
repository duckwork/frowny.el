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

;;;###autoload
(defcustom frowny-modes t
  "Modes to enable function `frowny-mode' in.
With an value of t, enable it in all modes."
  :type '(choice (const :tag "All modes" t)
                 (repeat :tag "Modes" function)))

;;;###autoload
(defcustom frowny-inhibit-modes '(special-mode)
  "Modes to inhibit function `frowny-mode' in.
A mode's membership in `frowny-inhibit-modes' overrides its
membership in `frowny-modes'."
  :type '(repeat :tag "Modes" function))

;;; Functions

(defun frowny--insert-character (character number)
  "Insert CHARACTER NUMBER times, depending on frowniness.
Internal: use `frowny-self-insert-frowny' or
`frowny-self-insert-smiley' instead."
  (cond
   ((ignore-errors                      ; necessary for the looking-back-limit
      (looking-back frowny-eyes frowny-eyes-looking-back-limit))
    (dotimes (_ number)
      (insert character)))
   (t (self-insert-command number character))))

(defun frowny-self-insert-frowny (N)
  "Insert \"(\" N times to complete a frowny."
  (interactive "p")
  (frowny--insert-character ?\( N))

(define-obsolete-function-alias 'frowny-self-insert 'frowny-self-insert-frowny
  "0.2"
  "Insert a frowny, or insert the character \"(\" N times.")

(defun frowny-self-insert-smiley (N)
  "Insert \")\" N times to complete a smiley."
  (interactive "p")
  (frowny--insert-character ?\) N))

(defun frowny-mode--turn-on ()
  "Turn on function `frowny-mode'."
  (when (and (not (apply #'derived-mode-p frowny-inhibit-modes))
             (or (eq frowny-modes t)
                 (apply #'derived-mode-p frowny-modes)))
    (frowny-mode +1)))

;;; Mode

;;;###autoload
(define-minor-mode frowny-mode
  "Minor mode for inserting frownies."
  :init t
  :lighter " :("
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map "(" #'frowny-self-insert-frowny)
            (define-key map ")" #'frowny-self-insert-smiley)
            map))

;;;###autoload
(define-globalized-minor-mode global-frowny-mode
  frowny-mode
  frowny-mode--turn-on)

(provide 'frowny)
;;; frowny.el ends here
