;; Do not initialise the package manager.  This is done in `init.el'.
(setq package-enable-at-startup nil)

;; Allow loading from the package cache.
(setq package-quickstart t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; disable tool-bar and menu-bar before being initialized
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
