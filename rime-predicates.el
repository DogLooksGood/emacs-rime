;;; rime-predicates.el --- Predicates for emacs-rime to automatic input Chinese/English. -*- lexical-binding: t; -*-
;;; cnsunyour/chinese/rime-predicates.el


;;; Commentary:
;;
;; With these predicates, You can continuously input mixed Chinese and English
;; text with punctuation, only using the Spacebar and the Enter key to assist,
;; without the extra switch key.
;;

;;; Code:

(defun rime-predicate-after-alphabet-char-p ()
  "If the cursor is after a alphabet character.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "[a-zA-Z][0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]*$" string))))

(defun rime-predicate-after-ascii-char-p ()
  "If the cursor is after a ascii character.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "[a-zA-Z0-9\x21-\x2f\x3a-\x40\x5b-\x60\x7b-\x7f]$" string))))

(defun rime-predicate-prog-in-code-p ()
  "If cursor is in code.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (derived-mode-p 'prog-mode 'conf-mode)
       (not (or (nth 3 (syntax-ppss))
                (nth 4 (syntax-ppss))))))

(defun rime-predicate-evil-mode-p ()
  "Detect whether the current buffer is in `evil' state.

Include `evil-normal-state' ,`evil-visual-state' ,
`evil-motion-state' , `evil-operator-state'.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (fboundp 'evil-mode)
       (or (evil-normal-state-p)
           (evil-visual-state-p)
           (evil-motion-state-p)
           (evil-operator-state-p))))

(defun rime-predicate-hydra-p ()
  "Whether a hydra keymap is activated.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (featurep 'hydra)
       (bound-and-true-p hydra-curr-map)))

(defun rime-predicate-ace-window-p ()
  "Detect if the `ace-window-mode' is enabled.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (featurep 'ace-window)
       (bound-and-true-p ace-window-mode)))

(defun rime-predicate-current-input-punctuation-p ()
  "If the current charactor entered is a punctuation.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
       (or (and (<= #x21 rime--current-input-key) (<= rime--current-input-key #x2f))
           (and (<= #x3a rime--current-input-key) (<= rime--current-input-key #x40))
           (and (<= #x5b rime--current-input-key) (<= rime--current-input-key #x60))
           (and (<= #x7b rime--current-input-key) (<= rime--current-input-key #x7f)))))

(defun rime-predicate-punctuation-after-space-cc-p ()
  "If input a punctuation after a Chinese charactor with whitespace.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (rime-predicate-current-input-punctuation-p)
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "\\cc +$" string))))

(defun rime-predicate-punctuation-after-ascii-p ()
  "If input a punctuation after a ascii charactor with whitespace.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (rime-predicate-current-input-punctuation-p)
       (rime-predicate-after-ascii-char-p)))

(defun rime-predicate-punctuation-line-begin-p ()
  "Enter half-width punctuation at the beginning of the line.

  Detect whether the current cursor is at the beginning of a
  line and the character last inputted is symbol.

  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (<= (point) (save-excursion (back-to-indentation) (point)))
       (rime-predicate-current-input-punctuation-p)))

(defun rime-predicate-auto-english-p ()
  "Auto switch Chinese/English input state.

  After activating this probe function, use the following rules
  to automatically switch between Chinese and English input:

     1. When the current character is an English
  character (excluding spaces), enter the next character as an
  English character.
    2. When the current character is a Chinese character or the
  input character is a beginning character, the input character is
  a Chinese character.
     3. With a single space as the boundary, automatically switch
  between Chinese and English characters.

  That is, a sentence of the form \"我使用 emacs 编辑此函数\"
  automatically switches between Chinese and English input methods.

  Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (if (string-match-p " +$" string)
             (string-match-p "\\cc +$" string)
           (not (string-match-p "\\cc$" string))))))

(defun rime-predicate-space-after-ascii-p ()
  "If cursor is after a whitespace which follow a ascii character."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (and (string-match-p " +$" string)
              (not (string-match-p "\\cc +$" string))))))

(defun rime-predicate-space-after-cc-p ()
  "If cursor is after a whitespace which follow a non-ascii character."
  (and (> (point) (save-excursion (back-to-indentation) (point)))
       (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
         (string-match-p "\\cc +$" string))))

(defun rime-predicate-current-uppercase-letter-p ()
  "If the current charactor entered is a uppercase letter.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and rime--current-input-key
       (>= rime--current-input-key ?A)
       (<= rime--current-input-key ?Z)))

(defun rime-predicate-org-latex-mode-p ()
  "If cursor is inside an org-mode's LaTeX fragment, macro or its arguments."
  (and (derived-mode-p  'org-mode)
       (or (org-inside-LaTeX-fragment-p)
           (org-inside-latex-macro-p))))

(defun rime-predicate-org-in-src-block-p ()
  "Whether point is in an org-mode's code source block."
  (and (derived-mode-p 'org-mode)
       (org-in-src-block-p)))

(defun rime-predicate-in-code-string-p ()
  "Whether point is in the code string(not comment string)."
  (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-string-face))

(defun rime-predicate-in-code-string-after-ascii-p ()
  "Whether point is in the code string and after a ascii character."
  (and
   (eq (plist-get (text-properties-at (point)) 'face) 'font-lock-string-face)
   (rime-predicate-after-ascii-char-p)))

(defun rime-predicate-tex-math-or-command-p ()
  "If point is inside a (La)TeX math environment, or a (La)TeX command.

Return true if the buffer is in `tex-mode' and one of the following three cases occurs:

  1. The point is inside a (La)TeX math environment;
  2. The current character is `$' or `\\', either at the beginning of a line, or after an ascii/space.
  3. The string before the point is of the form a (La)TeX command. If the command have a parameter, it is closed by `}' or `%'. If not, it is closed by a space or `%'.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (derived-mode-p 'tex-mode)
       (or (and (featurep 'tex-site)
                (texmathp))
           (and rime--current-input-key
                (or (= #x24 rime--current-input-key)
                    (= #x5c rime--current-input-key))
                (or (= (point) (line-beginning-position))
                    (= #x20 (char-before))
                    (rime-predicate-after-ascii-char-p)))
           (and (> (point) (save-excursion (back-to-indentation) (point)))
                (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
                  (or (string-match-p "[\x5c][\x21-\x24\x26-\x7e]*$" string)
                      (string-match-p "[\x5c][a-zA-Z\x23\x40]+[\x7b][^\x7d\x25]*$" string)))))))

;; Obsoleted functions:
(define-obsolete-function-alias 'rime--after-alphabet-char-p 'rime-predicate-after-alphabet-char-p "2020-03-26")
(define-obsolete-function-alias 'rime--prog-in-code-p 'rime-predicate-prog-in-code-p "2020-03-26")
(define-obsolete-function-alias 'rime--evil-mode-p 'rime-predicate-evil-mode-p "2020-03-26")
(define-obsolete-function-alias 'rime--punctuation-line-begin-p 'rime-predicate-punctuation-line-begin-p "2020-03-26")
(define-obsolete-function-alias 'rime--auto-english-p 'rime-predicate-auto-english-p "2020-03-26")
(make-obsolete 'rime-predicate-auto-english-p "please use other predicates instead of it." "2020-03-29")


(provide 'rime-predicates)

;;; rime-predicates.el ends here
