
(package-initialize)

(require 'ivy)

(ivy-mode 1)

(add-to-list 'load-path (file-name-directory buffer-file-name))

(setq rime-librime-root "~/.emacs.d/librime")

(require 'rime)

(rime-compile-module)

(rime-load-dynamic-module)




(setq default-input-method "rime"
      rime-show-candidate 'posframe)

(toggle-input-method)

(rime-lib-select-schema "terra_pinyin")
