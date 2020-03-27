(package-initialize)

(require 'rime)

(rime-compile-module)

(setq default-input-method "rime"
      rime-show-candidate 'posframe)

(toggle-input-method)

(rime-lib-select-schema "terra_pinyin")
