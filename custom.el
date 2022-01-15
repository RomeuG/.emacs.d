(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(custom-safe-themes
   '("6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" default))
 '(helm-apropos-function-list
   '(helm-def-source--emacs-commands helm-def-source--emacs-functions helm-def-source--emacs-variables helm-def-source--emacs-faces))
 '(helm-ff-lynx-style-map t)
 '(helm-imenu-lynx-style-map t t)
 '(helm-occur-use-ioccur-style-keys t)
 '(helm-semantic-lynx-style-map t t)
 '(lsp-semantic-tokens-enable nil nil nil "Customized with use-package lsp-mode")
 '(org-preview-latex-process-alist
   '((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -D %D -T tight -o %O %f"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim +repage -antialias -border 10 -bordercolor \"#1d2021\" %f -quality 100 -sharpen 0x1.0 %O"))))
 '(package-selected-packages
   '(color-theme eglot ox-extra org-contrib org-plus-contrib ob-latex-as-png nasm-mode vimrc-mode ox-reveal htmlize visual-regexp magit-todos flx pcre2el counsel auctex-latexmk auctex org-agenda crux ol lunar solar dired-subtree dired-aux dired-async rg isearch org-capture visual-regexp-steroids use-package unicode-fonts shrink-path rust-mode px projectile popwin ox-gfm multiple-cursors move-text lua-mode lsp hungry-delete helm gruvbox-theme git-timemachine forge flycheck exec-path-from-shell diminish diffview diff-hl csv-mode company-quickhelp cmake-mode clean-aindent-mode clang-format ccls browse-at-remote avy all-the-icons))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#1d2021"))
 '(popwin:popup-window-height 0.5 t)
 '(show-paren-mode t)
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "dark blue" :foreground "dark blue"))) nil '(diff-hl-delete ((t (:background "dark red" :foreground "dark red")))))
 '(org-ellipsis ((t (:foreground "gray40" :underline nil)))))
