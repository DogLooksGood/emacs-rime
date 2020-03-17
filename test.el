(package-initialize)

(require 'ivy)
(require 'dash)
(require 'cl-lib)

(ivy-mode 1)

(add-to-list 'load-path (file-name-directory buffer-file-name))

(require 'rime)

(setq default-input-method "rime")
