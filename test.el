(load-file (concat (file-name-directory (buffer-file-name)) "emacs-rime-lib.so"))

(rime-lib-start "/usr/share/rime-data/" "/home/tianshu/.emacs.d/rime/")

(rime-lib-process-key 97 0)

(rime-lib-get-context)

(rime-lib-get-input)

;; (rime-lib-clear-composition)

(rime-lib-get-commit)
