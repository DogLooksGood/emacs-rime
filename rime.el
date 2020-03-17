;; -*- lexical-binding: t -*-
;;; rime.el --- RIME integration with liberime for XingMa user.
;;
;; Author: Shi Tianshu
;; Keywords: input method, rime
;; Package-Requires: ((emacs "26.3") (dash "2.12.0") (cl-lib "1.0") (popup "0.5.3") (posframe "0.1.0"))
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
;;   (use-package liberime-config
;;     :quelpa (liberime-config
;;              :fetcher github
;;              :repo "DogLooksGood/liberime"
;;              :files ("CMakeLists.txt" "Makefile" "src" "liberime-config.el")))
;;
;;   (use-package rime
;;     :quelpa (rime
;;              :fetcher github
;;              :repo "DogLooksGood/emacs-rime")
;;     :bind
;;     (("C-\\" . 'rime-toggle)))
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
;; | ~posframe~   | 使用 ~posframe~ 展示跟随的候选                            |
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
(require 'posframe nil t)

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

(defface rime-posframe-face
  '((t (:inherit default :background "#333333" :foreground "#dcdccc")))
  "posframe 的样式"
  :group 'rime)

;;; 只要`input-method-function'有定义就会被使用。而启用输入法只生效在当前`buffer'
;;; 所以需要这些变量为`buffer-local'，
(make-variable-buffer-local 'input-method-function)
(make-variable-buffer-local 'deactivate-current-input-method-function)

(defvar rime--temporarily-ignore-predicates nil
  "是否临时忽略禁用断言。

该变量在关闭`rime-active-mode'时会被重置为`nil'。")

(defvar rime-force-enable-hook nil
  "激活强制模式时的`hook'。")

(defvar rime-force-enable-exit-hook nil
  "退出强制模式时的`hook'。")

(defcustom rime-disable-predicates nil
  "当此列表中任何一个断言函数成立时，进入临时英文模式。"
  :type 'list
  :group 'rime)

(defcustom rime-show-candidate 'minibuffer
  "是否在`minibuffer'中显示候选列表。"
  :type 'symbol
  :options '(minibuffer message popup posframe)
  :group 'rime)

(defcustom rime-cursor "|"
  "用于表示软光标的字符。"
  :type 'string
  :group 'rime)

(make-variable-buffer-local
 (defvar rime--preedit-overlay nil
   "存储嵌入首选的`overlay'，用于标记其范围便于修改。"))

(defvar rime--liberime-loaded nil
  "是否已经加载了`liberime'。")

(defvar rime--popup nil
  "当前在使用的 popup")

(defvar rime-posframe-buffer " *rime-posframe*"
  "posframe 的 buffer")

(defvar rime-posframe-hide-posframe-hooks
  '(window-configuration-change-hook))

;;;###autoload
(defvar rime-title "ㄓ"
  "输入法的展示符号")

(defvar rime-translate-keybindings
  '("C-f" "C-b" "C-n" "C-p" "C-g")
  "交由 Rime 处理的组合快捷键。

当前仅支持 Shift, Control, Meta 的组合键。
列出的按键会在`rime-active-mode-map'中生成一个到`rime--send-keybinding'的绑定。")

(defun rime--after-alphabet-char-p ()
  "当前光标是否在英文的后面。"
  (looking-back "[a-zA-Z][-_:.0-9/]*" 1))

(defun rime--prog-in-code-p ()
  "当前为`prog-mode'或`conf-mode'，且光标在注释或字符串当中。"
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (not (or (nth 3 (syntax-ppss))
             (nth 4 (syntax-ppss))))))

(defun rime--should-enable-p ()
  (or rime--temporarily-ignore-predicates
      (not (seq-find 'funcall rime-disable-predicates))))

(defun rime--minibuffer-display-result (result)
  (with-selected-window (minibuffer-window)
    (erase-buffer)
    (insert result)))

(defun rime--minibuffer-message (string)
  "当在 minibuffer 中使用 rime 输入中文时，需要将
minibuffer 原来显示的信息和 rime 选词框整合在一起显示
这个函数就是作这个工作。"
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
  "确保 `minibuffer' 每次打开都是英文模式."
  (deactivate-input-method))

(defun rime--popup-display-result (result)
  (if (featurep 'popup)
      (progn
        (when rime--popup
          (popup-delete rime--popup)
          (setq rime--popup nil))
        (unless (string-blank-p result)
          (setq rime--popup (popup-tip result :nowait t))))
    ;; 没有`popup'的时候使用`minibuffer'
    (rime--minibuffer-display-result result)))

(defun rime--posframe-display-result (result)
  (if (and (featurep 'posframe) (display-graphic-p))
      (if (string-blank-p result)
          (rime-posframe-hide-posframe)
        (posframe-show rime-posframe-buffer
                       :string result
                       :background-color (face-attribute 'rime-posframe-face :background)
                       :foreground-color (face-attribute 'rime-posframe-face :foreground))
        (dolist (hook rime-posframe-hide-posframe-hooks)
          (add-hook hook #'rime-posframe-hide-posframe nil t)))
    ;; 在非 GUI 或没有`posframe'的时候使用`popup'
    (rime--popup-display-result result)))

(defun rime-posframe-hide-posframe ()
  " 隐藏 posframe "
  (posframe-hide rime-posframe-buffer)
  (liberime-clear-composition)
  (rime--clear-overlay)
  (dolist (hook rime-posframe-hide-posframe-hooks)
    (remove-hook hook 'rime-posframe-hide-posframe t))
  )

(defun rime--show-candidate ()
  (let* ((context (liberime-get-context))
         (candidates (alist-get 'candidates (alist-get 'menu context)))
         (composition (alist-get 'composition context))
         (length (alist-get 'length composition))
         (preedit (alist-get 'preedit composition))
         (commit-text-preview (alist-get 'commit-text-preview context))
         (cursor-pos (alist-get 'cursor-pos composition))
         (sel-start (alist-get 'sel-start composition))
         (sel-end (alist-get 'sel-end composition))
         (menu (alist-get 'menu context))
         (input (liberime-get-input))
         (page-no (alist-get 'page-no menu))
         (text)
         (idx 1)
         (result ""))
    (when preedit
      (let ((i 0)
            (w 0))
        (when (zerop cursor-pos)
          (setq result (propertize rime-cursor 'face font-lock-function-name-face)))
        (while (< i (length preedit))
          (let* ((ch (char-to-string (aref preedit i)))
                 (len (liberime-string-length ch)))
            (setq w (+ w len)
                  i (1+ i))
            (setq text (if (= w cursor-pos)
                           (concat text ch rime-cursor)
                         (concat text ch)))))))
    (when context
      (setq result (concat result (propertize
                                   (format "%s " text)
                                   'face font-lock-function-name-face)))
      (dolist (c candidates)
        (setq result
              (concat result (format "%d. %s " idx c)))
        (setq idx (1+ idx))))
    (when (and page-no (not (zerop page-no)))
      (setq result (concat result (format " [%d] " (1+ page-no)))))
    (if (minibufferp)
        (rime--minibuffer-message
         (concat "\n" result))
      (cl-case rime-show-candidate
        (minibuffer (rime--minibuffer-display-result result))
        (message (message result))
        (popup (rime--popup-display-result result))
        (posframe (rime--posframe-display-result result))
        (t (progn))))))

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
      (overlay-put rime--preedit-overlay
                   'after-string (propertize preedit 'face
                                             'rime-preedit-face)))))

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
    (when-let (input (liberime-get-input))
      (rime--clear-overlay)
      (insert input)
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
  (rime--display-preedit)
  (rime--show-candidate)
  (rime--refresh-mode-state))

(defun rime--refresh-mode-state ()
  (if (liberime-get-context)
      (rime-active-mode 1)
    ;; 任何我们关闭候选的时候，都要关闭强制输入法状态
    (when rime--temporarily-ignore-predicates
      (setq rime--temporarily-ignore-predicates nil)
      (run-hooks 'rime-force-enable-exit-hook))
    (rime-active-mode -1)))

(defun rime-register-and-set-default ()
  "注册 RIME 输入法并设置为默认的方案。"
  (register-input-method "rime" "euc-cn" 'rime-activate "ㄓ")
  (setq-default default-input-method 'rime))

(defun rime-select-schema ()
  "选择 RIME 中使用的方案。"
  (interactive)
  (if rime--liberime-loaded
      (let* ((schema-list (liberime-get-schema-list))
             (schema-names (mapcar 'cdr schema-list))
             (schema-name (completing-read "Schema: " schema-names))
             (schema (thread-last schema-list
                       (seq-find (lambda (s)
                                   (equal (cadr s) schema-name)))
                       (car))))
        (message "Rime schema: %s" schema-name)
        (liberime-select-schema schema))
    (message "Rime is not activated.")))

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

;;;###autoload
(defun rime-activate (name)
  (if (require 'liberime-config nil t)
      (progn
        (setq rime--liberime-loaded t
		      input-method-function 'rime-input-method
		      deactivate-current-input-method-function #'rime-deactivate)
	    (dolist (binding rime-translate-keybindings)
	      (define-key rime-active-mode-map (kbd binding) 'rime--send-keybinding))
        (rime--clean-state)
		(add-hook 'minibuffer-setup-hook 'rime--init-minibuffer)
        (rime-mode 1)
        (message "Rime activate."))
    (error "Can't enable Rime, liberime is needed.")))

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
  "输入法有候选时的按键。")

(defvar rime-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "输入法启用时的按键。")

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
  "仅用于提供输入法输入中的按键绑定。

该模式不应该被手动启用。"
  nil
  nil
  rime-active-mode-map
  (if rime-active-mode
      (rime-active-mode--init)
    (rime-active-mode--uninit)))

(define-minor-mode rime-mode
  "输入法启用时的按键绑定。"
  nil
  nil
  rime-mode-map)

;;;###autoload
(register-input-method "rime" "euc-cn" 'rime-activate rime-title)

(defun rime-force-enable ()
  "临时强制使用输入法处理按键，在上屏，清空输入切换输入法时恢复原状态。"
  (interactive)
  (setq rime--temporarily-ignore-predicates t)
  (run-hooks 'rime-force-enable-hook))

(defun rime-open-configuration ()
  "打开 rime 配置文件"
  (interactive)
  (find-file (expand-file-name "default.custom.yaml" liberime-user-data-dir)))

;;;###autoload
(defun rime-toggle ()
  (interactive)
  (message "`rime-toggle' is deprecated, use `toggle-input-method' instead"))

(provide 'rime)

;;; rime.el ends here
