;; -*- lexical-binding: t -*-
;;; rime.el --- RIME input method
;;
;; Author: Shi Tianshu
;; Keywords: convenience, input-method
;; Package-Requires: ((emacs "26.3") (dash "2.12.0") (cl-lib "1.0") (popup "0.5.3") (posframe "0.1.0"))
;; Version: 1.0.0
;; URL: https://www.github.com/DogLooksGood/emacs-rime
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Emacs in Rime, support multiple schemas.
;;
;; * Installation
;;
;; Note: ~make~ and ~gcc~ is required.
;;
;; ** Linux
;;
;; Install librime with your package manager, if you are using fcitx-rime or ibus-rime,
;; the librime should be already installed.
;;
;; Emacs configuration:
;;
;; #+BEGIN_SRC emacs-lisp
;;   (use-package rime
;;     :custom
;;     (default-input-method "rime"))
;; #+END_SRC
;;
;; ** MacOS
;;
;; Download librime release.
;;
;; #+BEGIN_SRC bash
;;   wget https://github.com/rime/librime/releases/download/1.5.3/rime-1.5.3-osx.zip
;;   unzip rime-1.5.3-osx.zip -d ~/.emacs.d/librime
;;   rm -rf rime-1.5.3-osx.zip
;; #+END_SRC
;;
;; Emacs configuration:
;;
;; #+BEGIN_SRC emacs-lisp
;;   (use-package rime
;;     :init
;;     :custom
;;     (rime-librime-root "~/.emacs.d/librime/dist")
;;     (default-input-method "rime"))
;; #+END_SRC
;;
;; * Keybindings in Rime.
;;
;; With following configuration, you can send a serials of keybindings to Rime.
;; Since you may want them to help you with cursor navigation, candidate pagination and selection.
;;
;; Currently the keybinding with Control(C-), Meta(M-) and Shift(S-) is supported.
;;
;; #+BEGIN_SRC emacs-lisp
;;   ;; defaults
;;   (setq rime-translate-keybindings
;;     '("C-f" "C-b" "C-n" "C-p" "C-g"))
;; #+END_SRC
;;
;; * Candidate menu style
;;
;; Set via ~rime-show-candidate~.
;;
;; | Value      | description                                                                 |
;; |------------+-----------------------------------------------------------------------------|
;; | ~nil~        | don't show candidate at all.                                                |
;; | ~minibuffer~ | Display in minibuffer.                                                      |
;; | ~message~    | Display with ~message~ function, useful when you use minibuffer as mode-line. |
;; | ~popup~      | Use popup.                                                                  |
;; | ~posframe~   | Use posfarme, will fallback to popup in TUI                                 |
;;
;; * The lighter
;;
;; You can get a lighter via ~(rime-lighter)~, which returns you a colored ~ㄓ~.
;; Put it in modeline or anywhere you want.
;;
;; You can customize with ~rime-title~, ~rime-indicator-face~ and ~rime-indicator-dim-face~.
;;
;; * Temporarily ascii mode
;;
;; If you want specific a list of rules to automatically enable ascii mode, you can customize ~rime-disable-predicates~.
;;
;; Following is a example to use ascii mode in ~evil-normal-state~ or when cursor is after alphabet character or when cursor is in code.
;;
;; #+BEGIN_SRC emacs-lisp
;;   (setq rime-disable-predicates
;;         '(evil-normal-state-p
;;           rime--after-alphabet-char-p
;;           rime--prog-in-code-p))
;; #+END_SRC
;;
;; ** Force enable
;;
;; If one of ~rime-disable-predicates~ returns t, you can still force enable the input method with ~rime-force-enable~.
;; The effect will only last for one input behavior.
;;
;; You probably want to give this command a keybinding.
;;
;; * The soft cursor
;;
;; Default to ~|~ , you can customize it with
;;
;; #+BEGIN_SRC emacs-lisp
;;   (setq rime-cursor "˰")
;; #+END_SRC
;;
;; * Shortcut to open Rime configuration file
;;
;; Use ~rime-open-configuration~.


;;; Code:

(require 'subr-x)
(require 'dash)
(require 'cl-lib)
(require 'popup nil t)
(require 'posframe nil t)

(defface rime-preedit-face
  '((((class color) (background dark))
     (:underline t))
    (((class color) (background light))
     (:underline t)))
  "Face for inline preedit."
  :group 'rime)

(defface rime-indicator-face
  '((((class color) (background dark))
     (:foreground "#9256B4" :bold t))
    (((class color) (background light))
     (:foreground "#9256B4" :bold t)))
  "Face for mode-line indicator when input-method is available."
  :group 'rime)

(defface rime-indicator-dim-face
  '((((class color) (background dark))
     (:foreground "#606060" :bold t))
    (((class color) (background light))
     (:foreground "#606060" :bold t)))
  "Face for mode-line indicator when input-method is temporarily disabled."
  :group 'rime)

(defface rime-posframe-face
  '((t (:inherit default :background "#333333" :foreground "#dcdccc")))
  "Face for candidate posframe."
  :group 'rime)

(defface rime-code-face
  '((t (:inherit font-lock-string-face)))
  "Face for code in candidate, not available in `message' and `popup'."
  :group 'rime)

(defface rime-candidate-num-face
  '((t (:inherit font-lock-comment-face)))
  "Face for the number before each candidate, not available in `message' and `popup'."
  :group 'rime)

(defcustom rime-librime-root nil
  "The path to the directory of librime.

Leave it nil if you have librime's lib and header files in the standard path.
Otherwise you should set this to where you put librime."
  :type 'string
  :group 'rime)

;;; We need these variables to be buffer local.
(make-variable-buffer-local 'input-method-function)
(make-variable-buffer-local 'deactivate-current-input-method-function)

(defvar rime--temporarily-ignore-predicates nil
  "Temporarily disable all predicates.

Set to t will ensure the next input will be handled by input-method.
Will be reset to nil when `rime-active-mode' is disabled. ")

(defvar rime-force-enable-hook nil
  "hooks run after `rime-force-enable' is called.")

(defvar rime-force-enable-exit-hook nil
  "hooks rum after the state of `rime-force-enable' is turned off.")

(defcustom rime-disable-predicates nil
  "A list of predicate functions, each receive no argument.

If one of these functions return will, the input-method will fallback to ascii mode."
  :type 'list
  :group 'rime)

(defcustom rime-show-candidate 'minibuffer
  "How we display the candidate menu.

nil means don't display candidate at all.
`minibuffer', display canidate in minibuffer.
`popup', display with popup.el.
`message', display with function `message', this is a replacement for `minibuffer' if you use minibuffer as the mode-line.
`posframe', display candidate in posframe, will fallback to popup in TUI.
"
  :type 'symbol
  :options '(minibuffer message popup posframe)
  :group 'rime)

(defcustom rime-user-data-dir (locate-user-emacs-file "rime/")
  "Rime user data directory.

Defaults to ~/.emacs.d/rime/"
  :type 'string
  :group 'rime)

(defcustom rime-share-data-dir
  (cl-case system-type
    ('gnu/linux
     (cl-some (lambda (parent)
                (let ((dir (expand-file-name "rime-data" parent)))
                  (when (file-directory-p dir)
                    dir)))
              (if (fboundp 'xdg-data-dirs)
                  (xdg-data-dirs)
                '("/usr/share/local" "/usr/share"))))
    ('darwin
     "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"))
  "Rime share data directory. "
  :type 'string
  :group 'rime)

(defvar rime--root (file-name-directory (or load-file-name buffer-file-name))
  "The path to the root of rime package.")

(defvar rime--module-path
  (concat rime--root
          "librime-emacs"
          module-file-suffix)
  "The path to the dynamic module.")

(defcustom rime-cursor "|"
  "The character used to display the soft cursor in preedit."
  :type 'string
  :group 'rime)

(make-variable-buffer-local
 (defvar rime--preedit-overlay nil
   "Overlay on preedit."))

(defvar rime--lib-loaded nil
  "If dynamic module is loaded.")

(defvar rime--popup nil
  "The current in-use popup.")

(defvar rime-posframe-buffer " *rime-posframe*"
  "The buffer name for candidate posframe.")

(defvar rime-posframe-hide-posframe-hooks
  '(window-configuration-change-hook)
  "Hide posframe in these hooks.")

;;;###autoload
(defvar rime-title "ㄓ"
  "The title of input method.")

(defvar rime-translate-keybindings
  '("C-f" "C-b" "C-n" "C-p" "C-g")
  "A list of keybindings those sent to Rime during composition.

Currently only Shift, Control, Meta is supported as modifiers.
Each keybinding in this list, will be bound to `rime--send-keybinding' in `rime-active-mode-map'. ")

(defun rime--after-alphabet-char-p ()
  "If the cursor is after a alphabet character.

Can be used in `rime-disable-predicates'."
  (looking-back "[a-zA-Z][-_:.0-9/]*" 1))

(defun rime--prog-in-code-p ()
  "If major-mode derives from `prog-mode' and `conf-mode', and the cursor is in in comment or string.

Can be used in `rime-disable-predicates'."
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun rime--should-enable-p ()
  "If key event should be handled by input-method."
  (or rime--temporarily-ignore-predicates
      (not (seq-find 'funcall rime-disable-predicates))))

(defun rime--minibuffer-display-content (content)
  "Display CONTENT in minibuffer."
  (with-selected-window (minibuffer-window)
    (erase-buffer)
    (insert content)))

(defun rime--popup-display-content (content)
  "Display CONTENT with popup.el"
  (if (featurep 'popup)
      (progn
        (when rime--popup
          (popup-delete rime--popup)
          (setq rime--popup nil))
        (unless (string-blank-p content)
          (setq rime--popup (popup-tip content :nowait t))))
    ;; Fallback to popup when not available.
    (rime--minibuffer-display-content content)))

(defun rime--minibuffer-message (string)
  "Concatenate STRING and minibuffer contents.

Used to display in minibuffer when we are using input method in minibuffer. "
  (message nil)
  (let ((inhibit-quit t)
        point-1)
    (save-excursion
      (insert string)
      (setq point-1 (point)))
    (sit-for 1000000)
    (delete-region (point) point-1)
    (when quit-flag
      (setq quit-flag nil
            unread-command-events '(7)))))

(defun rime--init-minibuffer ()
  (deactivate-input-method))

(defun rime--posframe-display-content (content)
  "Display CONTENT with posframe."
  (if (and (featurep 'posframe) (display-graphic-p))
      (if (string-blank-p content)
          (rime-posframe-hide-posframe)
        (posframe-show rime-posframe-buffer
                       :string content
                       :background-color (face-attribute 'rime-posframe-face :background)
                       :foreground-color (face-attribute 'rime-posframe-face :foreground))
        (dolist (hook rime-posframe-hide-posframe-hooks)
          (add-hook hook #'rime-posframe-hide-posframe nil t)))
    ;; Fallback to popup when not available.
    (rime--popup-display-content result)))

(defun rime-posframe-hide-posframe ()
  (posframe-hide rime-posframe-buffer)
  (rime-lib-clear-composition)
  (rime--clear-overlay)
  (dolist (hook rime-posframe-hide-posframe-hooks)
    (remove-hook hook 'rime-posframe-hide-posframe t)))

(defun rime--show-content (content)
  "Display CONTENT as candidate."
  (if (minibufferp)
        (rime--minibuffer-message
         (concat "\n" result))
      (cl-case rime-show-candidate
        (minibuffer (rime--minibuffer-display-content content))
        (message (message content))
        (popup (rime--popup-display-content content))
        (posframe (rime--posframe-display-content content))
        (t (progn)))))

(defun rime--build-candidate-content ()
  (let* ((context (rime-lib-get-context))
         (candidates (alist-get 'candidates (alist-get 'menu context)))
         (composition (alist-get 'composition context))
         (length (alist-get 'length composition))
         (preedit (alist-get 'preedit composition))
         (commit-text-preview (alist-get 'commit-text-preview context))
         (cursor-pos (alist-get 'cursor-pos composition))
         (before-cursor (alist-get 'before-cursor composition))
         (after-cursor (alist-get 'after-cursor composition))
         (sel-start (alist-get 'sel-start composition))
         (sel-end (alist-get 'sel-end composition))
         (menu (alist-get 'menu context))
         (input (rime-lib-get-input))
         (page-no (alist-get 'page-no menu))
         (idx 1)
         (result ""))
    (when context
      (when preedit
        (setq result (concat (propertize
                              (concat before-cursor rime-cursor after-cursor)
                              'face 'rime-code-face)
                             " ")))
      (dolist (c candidates)
        (setq result
              (concat result
                      (propertize
                       (format "%d. " idx)
                       'face 'rime-candidate-num-face)
                      (format "%s " c)))
        (setq idx (1+ idx))))
    (when (and page-no (not (zerop page-no)))
      (setq result (concat result (format " [%d]" (1+ page-no)))))
    result))

(defun rime--show-candidate ()
  (rime--show-content (rime--build-candidate-content)))

(defun rime--parse-key-event (event)
  "Translate Emacs key EVENT to Rime's format.

the car is keyCode, the cdr is mask. "
  (let* ((modifiers (event-modifiers event))
         (type (event-basic-type event))
         (mask (+
                (if (member 'shift modifiers)
                    1                   ; 1 << 0
                  0)
                (if (member 'meta modifiers)
                    8                   ; 1 << 3
                  0)
                (if (member 'control modifiers)
                    4                ; 1 << 2
                  0))))
    (cons type mask)))

(defun rime--clear-overlay ()
  (when (overlayp rime--preedit-overlay)
    (delete-overlay rime--preedit-overlay)
    (setq rime--preedit-overlay nil)))

(defun rime--display-preedit ()
  (let ((preedit (alist-get 'commit-text-preview (rime-lib-get-context))))
    ;; Always delete the old overlay.
    (rime--clear-overlay)
    ;; Create the new preedit
    (when preedit
      (setq rime--preedit-overlay (make-overlay (point) (point)))
      (overlay-put rime--preedit-overlay
                   'after-string (propertize preedit 'face
                                             'rime-preedit-face)))))

(defun rime--rime-lib-module-ready-p ()
  (fboundp 'rime-lib-clear-composition))

(defun rime--redisplay (&rest ignores)
  "Display inline preedit and candidates."
  (rime--display-preedit)
  (rime--show-candidate))

(defun rime--backspace ()
  (interactive)
  (when (rime--rime-lib-module-ready-p)
    (when-let ((context (rime-lib-get-context)))
      (rime-lib-process-key 65288 0)
      (rime--redisplay))
    (rime--refresh-mode-state)))

(defun rime--escape ()
  (interactive)
  (when (rime--rime-lib-module-ready-p)
    (when-let ((context (rime-lib-get-context)))
      (rime-lib-clear-composition)
      (rime--redisplay))
    (rime--refresh-mode-state)))

(defun rime--return ()
  (interactive)
  (when (rime--rime-lib-module-ready-p)
    (when-let (input (rime-lib-get-input))
      (rime--clear-overlay)
      (insert input)
      (rime-lib-clear-composition)
      (rime--redisplay))
    (rime--refresh-mode-state)))

(defun rime-input-method (key)
  "Process KEY with input method."
  (when (rime--rime-lib-module-ready-p)
    (if (and (not (rime--should-enable-p))
             (not (rime-lib-get-context)))
        (list key)
      (rime-lib-process-key key 0)
      (with-silent-modifications
        (let* ((context (rime-lib-get-context))
               (preedit (thread-last context
                          (alist-get 'composition)
                          (alist-get 'preedit)))
               (commit (rime-lib-get-commit)))
          (unwind-protect
              (cond
               ((and (not context) (not commit) (not preedit))
                (list key))
               (commit
                (rime--clear-overlay)
                (mapcar 'identity commit))
               (t (rime--redisplay)))
            (rime--refresh-mode-state)))))))

(defun rime--send-keybinding ()
  (interactive)
  (let* ((parsed (rime--parse-key-event last-input-event))
         (key (car parsed))
         (mask (cdr parsed)))
    (rime-lib-process-key key mask)
    (rime--redisplay)
    (rime--refresh-mode-state)))

(defun rime--clean-state ()
  (rime-lib-clear-composition)
  (rime--display-preedit)
  (rime--show-candidate)
  (rime--refresh-mode-state))

(defun rime--refresh-mode-state ()
  (if (rime-lib-get-context)
      (rime-active-mode 1)
    ;; Whenever we disable `rime-active-mode', we should also unset `rime--temporarily-ignore-predicates'.
    (when rime--temporarily-ignore-predicates
      (setq rime--temporarily-ignore-predicates nil)
      (run-hooks 'rime-force-enable-exit-hook))
    (rime-active-mode -1)))

(defun rime-select-schema ()
  "Select Rime schema."
  (interactive)
  (if rime--lib-loaded
      (let* ((schema-list (rime-lib-get-schema-list))
             (schema-names (mapcar 'cdr schema-list))
             (schema-name (completing-read "Schema: " schema-names))
             (schema (thread-last schema-list
                       (seq-find (lambda (s)
                                   (equal (cadr s) schema-name)))
                       (car))))
        (message "Rime schema: %s" schema-name)
        (rime-lib-select-schema schema))
    (message "Rime is not activated.")))

;;;###autoload
(defun rime-lighter ()
  "Return a lighter which can be used in mode-line.

The content is `rime-title'.

You can customize the color with `rime-indicator-face' and `rime-indicator-dim-face'."

  (if (and (equal current-input-method "rime")
           (bound-and-true-p rime-mode))
      (if (rime--should-enable-p)
          (propertize
           (concat " " rime-title)
           'face
           'rime-indicator-face)
        (propertize
         (concat " " rime-title)
         'face
         'rime-indicator-dim-face))
    ""))

(defun rime-compile-module ()
  "Compile dynamic module if lib file is not exists."
  (unless (file-exists-p rime--module-path)
    (let ((env (if rime-librime-root
                    (format "LIBRIME_ROOT=%s" (file-name-as-directory rime-librime-root))
                  "")))
      (shell-command
       (format "cd %s; %s make lib" rime--root env)))))

(defun rime-load-dynamic-module ()
  "Load dynamic module."
  (if (not (file-exists-p rime--module-path))
      (error "Failed to compile dynamic module.")
    (load-file rime--module-path)
    (rime-lib-start rime-share-data-dir rime-user-data-dir)
    (setq rime--lib-loaded t)))

;;;###autoload
(defun rime-activate (name)
  (unless rime--lib-loaded
    (rime-compile-module)
    (rime-load-dynamic-module))

  (setq input-method-function 'rime-input-method
		deactivate-current-input-method-function #'rime-deactivate)

  (dolist (binding rime-translate-keybindings)
	(define-key rime-active-mode-map (kbd binding) 'rime--send-keybinding))

  (rime--clean-state)
  (add-hook 'minibuffer-setup-hook 'rime--init-minibuffer)
  (rime-mode 1)
  (message "Rime activate."))

(defun rime-deactivate ()
  (rime--clean-state)
  (remove-hook 'minibuffer-setup-hook 'rime--init-minibuffer)
  (rime-mode -1)
  (message "Rime deactivate."))

(defvar rime-active-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "DEL") 'rime--backspace)
    (define-key keymap (kbd "<backspace>") 'rime--backspace)
    (define-key keymap (kbd "RET") 'rime--return)
    (define-key keymap (kbd "<escape>") 'rime--escape)
    keymap)
  "Keymap during composition.")

(defvar rime-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap when input method is enabled.")

;;; Initializer

(defun rime--init-hook-default ()
  (add-hook 'post-self-insert-hook 'rime--redisplay nil t))

(defun rime--uninit-hook-default ()
  (remove-hook 'post-self-insert-hook 'rime--redisplay))

(defun rime--init-hook-vterm ()
  (advice-add 'vterm--redraw :after 'rime--redisplay)
  (define-key vterm-mode-map (kbd "<backspace>") 'rime--backspace))

(defun rime--uninit-hook-vterm ()
  (advice-add 'vterm--redraw :after 'rime--redisplay)
  (define-key vterm-mode-map (kbd "<backspace>") 'vterm-send-backspace))

(defun rime-active-mode--init ()
  (cl-case major-mode
    (vterm-mode (rime--init-hook-vterm))
    (t (rime--init-hook-default))))

(defun rime-active-mode--uninit ()
  (cl-case major-mode
    (vterm-mode (rime--uninit-hook-vterm))
    (t (rime--uninit-hook-default))))

(define-minor-mode rime-active-mode
  "Mode used in composition.

Should not be enabled manually."
  nil
  nil
  rime-active-mode-map
  (if rime-active-mode
      (rime-active-mode--init)
    (rime-active-mode--uninit)))

(define-minor-mode rime-mode
  "Mode used when input method is activated."
  nil
  nil
  rime-mode-map)

;;;###autoload
(register-input-method "rime" "euc-cn" 'rime-activate rime-title)

(defun rime-deploy()
  (interactive)
  (liberime-finalize)
  (liberime--start))

(defun rime-sync ()
  (interactive)
  (liberime-sync-user-data))

(defun rime-force-enable ()
  "Enable temporarily ascii mode.

Will resume when finish composition."
  (interactive)
  (setq rime--temporarily-ignore-predicates t)
  (run-hooks 'rime-force-enable-hook))

(defun rime-open-configuration ()
  "Open Rime configuration file."
  (interactive)
  (find-file (expand-file-name "default.custom.yaml" rime-user-data-dir)))

(provide 'rime)

;;; rime.el ends here
