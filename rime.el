;; -*- lexical-binding: t -*-
;;; rime.el --- RIME integration with liberime for XingMa user.
;;
;; Author: Shi Tianshu
;; Keywords: input method, rime
;; Package-Requires: ((emacs "26.3") (dash "2.12.0") (cl-lib "1.0") (popup "0.5.3"))
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

;; * 此项目的目的
;;
;; 在 Emacs 中通过 [[https://github.com/merrickluo/liberime][liberime]] 集成 RIME 输入法，支持多种形码输入方案。
;;
;; 拼音方案请移步： [[https://github.com/tumashu/pyim][pyim]] 或 [[https://github.com/QiangF/liberime][erime]].
;;
;; * 使用方法
;;
;; #+BEGIN_SRC emacs-lisp
;;   (use-package liberime
;;     :quelpa (liberime
;;              :fetcher github
;;              :repo "merrickluo/liberime"
;;              :files ("CMakeLists.txt" "Makefile" "src" "liberime.el")))
;;
;;   (use-package rime
;;     :quelpa (rime
;;              :fetcher github
;;              :repo "DogLooksGood/emacs-rime"))
;; #+END_SRC
;;
;; * 候选项展示
;;
;; 设置 ~rime-show-candidate~ 。
;;
;; | 可选值       | 说明                                                      |
;; |--------------+-----------------------------------------------------------|
;; | ~nil~        | 不展示                                                    |
;; | ~minibuffer~ | 在minibuffer中展示， 推荐使用的方式                       |
;; | ~message~    | 直接使用 ~message~ 输出，兼容控制 ~minibuffer~ 内容的插件 |
;; | ~popup~      | 使用 ~popup.el~ 展示跟随的候选                            |
;;
;; * 用于展示的提示符
;;
;; 使用函数 ~(rime-lighter)~ 返回一个用于展示的 ~ㄓ~ 符号。
;; 可以通过 ~rime-indicator-face~ 和 ~rime-indicator-dim-face~ 设置样式。
;;
;; * 临时英文模式的切换
;; 如果使用模式编辑，或是需要在一些特定的场景下自动使用英文，可以 ~rime-disable-predicates~ 。
;;
;; 一个在 ~evil-normal-state~ 中、在英文字母后面以及代码中自动使用英文的例子。
;;
;; #+BEGIN_SRC emacs-lisp
;;   (setq rime-disable-predicates
;;         '(evil-normal-state-p
;;           rime--after-alphabet-char-p
;;           rime--prog-in-code-p))
;; #+END_SRC
;;
;; * 如果你使用 Linux
;; Emacs 有一个优秀的远古BUG: 如果 ~LC_CTYPE~ 为 ~en_US.UTF8~ 的话，那么就无法调用起 Fcitx.
;; 所以可以利用这点把 Emacs 内切换输入法的快捷键和系统快捷键设为同一个键。
;;
;; * 优秀的 Emacs 输入法
;;
;; 你可能需要 [[https://github.com/tumashu/pyim][pyim]], [[https://github.com/merrickluo/liberime][liberime]], [[https://github.com/QiangF/liberime][erime]].

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'cl-lib)
(require 'popup nil t)

(defface rime-preedit-face
  '((((class color) (background dark))
     (:underline t))
    (((class color) (background light))
     (:underline t)))
  "输入法嵌入首选的样式"
  :group 'rime)

(defface rime-indicator-face
  '((((class color) (background dark))
     (:foreground "#9256B4" :bold t))
    (((class color) (background light))
     (:foreground "#9256B4" :bold t)))
  "提示符的样式"
  :group 'rime)

(defface rime-indicator-dim-face
  '((((class color) (background dark))
     (:foreground "#606060" :bold t))
    (((class color) (background light))
     (:foreground "#606060" :bold t)))
  "提示符的样式"
  :group 'rime)

;;; 只要`input-method-function'有定义就会被使用。而启用输入法只生效在当前`buffer'
;;; 所以需要这些变量为`buffer-local'，
(make-variable-buffer-local 'input-method-function)
(make-variable-buffer-local 'deactivate-current-input-method-function)

(defcustom rime-disable-predicates nil
  "当此列表中任何一个断言函数成立时，进入临时英文模式。"
  :type 'list
  :group 'rime)

(defcustom rime-show-candidate 'minibuffer
  "是否在`minibuffer'中显示候选列表。"
  :type 'symbol
  :options '(minibuffer message popup)
  :group 'rime)

(make-variable-buffer-local
 (defvar rime--preedit-overlay nil
   "存储嵌入首选的`overlay'，用于标记其范围便于修改。"))

(defvar rime--liberime-loaded nil
  "是否已经加载了`liberime'。")

(defvar rime-input-editable t
  "是否启用可编辑的编码，通常用于拼音输入法。

若在 Rime 启用了 translator 中的 preedit_format 功能，启用该项将无法正常工作。")

(defvar rime-title "ㄓ"
  "输入法的展示符号")

(defvar rime-translate-keybindings
  '("C-f" "C-b")
  "交由 Rime 处理的组合快捷键。

当前仅支持 Shift, Control, Meta 的组合键。
列出的按键会在`rime-mode-map'中生成一个到`rime--send-keybinding'的绑定。")

(defun rime--after-alphabet-char-p ()
  "当前光标是否在英文的后面。"
  (looking-back "[a-zA-Z][-_:.0-9/]*" 1))

(defun rime--prog-in-code-p ()
  "当前为`prog-mode'或`conf-mode'，且光标在注释或字符串当中。"
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun rime--should-enable-p ()
  (not (seq-find 'funcall rime-disable-predicates)))

(defun rime--minibuffer-display-result (result)
  (with-selected-window (minibuffer-window)
	(erase-buffer)
	(insert result)))

(defun rime--show-candidate ()
  (let* ((context (liberime-get-context))
         (candidates (alist-get 'candidates (alist-get 'menu context)))
         (composition (alist-get 'composition context))
         (length (alist-get 'length composition))
         (preedit (alist-get 'preedit composition))
         (cursor-pos (alist-get 'cursor-pos composition))
         (cursor-pos-in-preedit (when cursor-pos
                                  (- (length preedit)
                                     (- length cursor-pos))))
         (menu (alist-get 'menu context))
         (input (liberime-get-input))
         (page-no (alist-get 'page-no menu))
         (preedit-with-cursor
          (when preedit
            (if (or (not rime-input-editable)
                    (>= cursor-pos-in-preedit (length preedit)))
                preedit
              (concat
               (substring-no-properties preedit 0 cursor-pos-in-preedit)
               "|"
               (substring-no-properties preedit cursor-pos-in-preedit)))))
         (idx 1)
         (result ""))
    (when context
      (setq result
            (concat result (format "%s " preedit-with-cursor)))
      (dolist (c candidates)
        (setq result
              (concat result (format "%d. %s " idx c)))
        (setq idx (1+ idx))))
    (when (and page-no (not (zerop page-no)))
      (setq result (concat result (format " [%d] " (1+ page-no)))))
    (cl-case rime-show-candidate
      (minibuffer (rime--minibuffer-display-result result))
      (message (message result))
      (popup (popup-tip result))
      (t (progn)))))

(defun rime--parse-key-event (event)
  "将 Emacs 中的 Key 换成 Rime 中的 Key + Mask.

返回中`car'是 KeyCode, `cdr'是 Mask."
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
  (let ((preedit (alist-get 'commit-text-preview (liberime-get-context))))
    ;; Always delete the old overlay.
    (rime--clear-overlay)
    ;; Create the new preedit
    (when preedit
      (setq rime--preedit-overlay (make-overlay (point) (point)))
      (overlay-put rime--preedit-overlay 'after-string (propertize preedit 'face 'rime-preedit-face)))))

(defun rime--liberime-module-ready-p ()
  (fboundp 'liberime-clear-composition))

(defun rime--redisplay (&rest ignores)
  "绘制嵌入编码和候选。"
  (rime--display-preedit)
  (rime--show-candidate))

(defun rime--backspace ()
  (interactive)
  (when (rime--liberime-module-ready-p)
    (when-let ((context (liberime-get-context)))
      (liberime-process-key 65288)
      (rime--redisplay))
    (rime--refresh-mode-state)))

(defun rime--escape ()
  (interactive)
  (when (rime--liberime-module-ready-p)
    (when-let ((context (liberime-get-context)))
      (liberime-clear-composition)
      (rime--redisplay))
    (rime--refresh-mode-state)))

(defun rime--return ()
  (interactive)
  (when (rime--liberime-module-ready-p)
    (when-let ((preedit (thread-last (liberime-get-context)
                          (alist-get 'composition)
                          (alist-get 'preedit))))
      (rime--clear-overlay)
      (insert preedit)
      (liberime-clear-composition)
      (rime--redisplay))
    (rime--refresh-mode-state)))

(defun rime-input-method (key)
  (when (rime--liberime-module-ready-p)
    (if (and (not (rime--should-enable-p))
             (not (liberime-get-context)))
        (list key)
      (liberime-process-key key)
      (with-silent-modifications
        (let* ((context (liberime-get-context))
               (preedit (thread-last context
                          (alist-get 'composition)
                          (alist-get 'preedit)))
               (commit (liberime-get-commit)))
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
    (liberime-process-key key mask)
    (rime--redisplay)
    (rime--refresh-mode-state)))

(defun rime--clean-state ()
  "清空状态，包换`liberime'的状态和`preedit'。"
  (liberime-clear-composition)
  (when (overlayp rime--preedit-overlay)
    (delete-overlay rime--preedit-overlay)
    (setq-local rime--preedit-overlay nil))
  (rime--refresh-mode-state))

(defun rime--refresh-mode-state ()
  (if (liberime-get-context)
      (rime-mode 1)
    (rime-mode -1)))

(defun rime-register-and-set-default ()
  "注册 RIME 输入法并设置为默认的方案。"
  (register-input-method "rime" "euc-cn" 'rime-activate "ㄓ")
  (setq-default default-input-method 'rime))

(defun rime-select-schema ()
  "选择 RIME 中使用的方案。"
  (interactive)
  (let* ((schema-list (liberime-get-schema-list))
         (schema-names (mapcar 'cdr schema-list))
         (schema-name (completing-read "Schema: " schema-names))
         (schema (thread-last schema-list
                   (seq-find (lambda (s)
                               (message "%s %s" (cdr s) schema-name)
                               (equal (cadr s) schema-name)))
                   (car))))
    (message "Rime schema: %s" schema-name)
    (liberime-select-schema schema)))

(defun rime-lighter ()
  "返回一个可以用于展示在`modeline'的符号。

该符号可通过修改变量`rime-title'进行设置。
在激活/非激活的情况下使用不同的`face'。"
  (if (equal current-input-method "rime")
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

(defun rime-activate (name)
  (setq input-method-function 'rime-input-method
        deactivate-current-input-method-function #'rime-deactivate)
  (dolist (binding rime-translate-keybindings)
    (define-key rime-mode-map (kbd binding) 'rime--send-keybinding))
  (message "Rime activate."))

(defun rime-deactivate ()
  (rime--clean-state)
  (message "Rime deactivate."))

(defvar rime-mode-map
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "DEL") 'rime--backspace)
        (define-key keymap (kbd "RET") 'rime--return)
        (define-key keymap (kbd "<escape>") 'rime--escape)
        keymap))

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

(defun rime-mode--init ()
  (cl-case major-mode
    (vterm-mode (rime--init-hook-vterm))
    (t (rime--init-hook-default))))

(defun rime-mode--uninit ()
  (cl-case major-mode
    (vterm-mode (rime--uninit-hook-vterm))
    (t (rime--uninit-hook-default))))

(define-minor-mode rime-mode
  "仅用于提供输入法激活状态下的按键绑定。

该模式不应该被手动启用。"
  nil
  nil
  rime-mode-map
  (if rime-mode
      (rime-mode--init)
    (rime-mode--uninit)))

;;;###autoload
(defun rime-toggle ()
  "激活/关闭 RIME 输入法。

有候选时切换输入法会清空未上屏的内容。"
  (interactive)
  (if (not (equal "rime" current-input-method))
      (progn
        (unless rime--liberime-loaded
          (require 'liberime nil t)
          (if (not (featurep 'liberime))
              (error "Can't enable Rime, liberime is needed.")
            (register-input-method "rime" "euc-cn" 'rime-activate rime-title)
            (setq rime--liberime-loaded t)))
        (set-input-method 'rime))
    (when (liberime-get-context)
      (liberime-clear-composition)
      (rime--redisplay))
    (set-input-method nil)))

(provide 'rime)

;;; rime.el ends here
