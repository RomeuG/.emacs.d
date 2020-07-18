(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(ccls-executable "ccls")
 '(company-echo-delay 0.1 t)
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-delay 0.8 t)
 '(company-tooltip-align-annotations t)
 '(custom-safe-themes
   '("a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" default))
 '(helm-apropos-function-list
   '(helm-def-source--emacs-commands helm-def-source--emacs-functions helm-def-source--emacs-variables helm-def-source--emacs-faces))
 '(helm-ff-lynx-style-map t)
 '(helm-imenu-lynx-style-map t t)
 '(helm-occur-use-ioccur-style-keys t)
 '(helm-semantic-lynx-style-map t t)
 '(lsp-auto-guess-root t t)
 '(lsp-diagnostic-package :none t)
 '(lsp-enable-semantic-highlighting nil t)
 '(lsp-enable-snippet nil t)
 '(lsp-log-io nil)
 '(lsp-prefer-flymake nil t)
 '(lsp-print-io nil t)
 '(lsp-print-performance nil t)
 '(lsp-trace nil t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position 'top)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-fontify 'on-demand)
 '(lsp-ui-peek-list-width 50)
 '(lsp-ui-peek-peek-height 20)
 '(lsp-ui-sideline-code-actions-prefix "" t)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-code-actions t)
 '(lsp-ui-sideline-show-diagnostics nil)
 '(lsp-ui-sideline-show-hover t)
 '(lsp-ui-sideline-show-symbol t)
 '(magit-auto-revert-mode nil)
 '(package-selected-packages
   '(unicode-fonts all-the-icons diminish visual-regexp-steroids use-package typescript-mode stickyfunc-enhance srefactor sr-speedbar smartparens smart-mode-line rust-mode rg restclient px popwin pcre2el parent-mode ox-gfm org-web-tools org-ref org-bullets multiple-cursors move-text magit-popup lua-mode lsp-ui latex-preview-pane iedit hungry-delete helm-gtags helm-ag gruvbox-theme graphql git-timemachine ghub+ ggtags function-args fringe-helper forge flycheck find-file-in-project exec-path-from-shell elpy disable-mouse diffview diff-hl csv-mode company-quickhelp company-lsp company-auctex cmake-mode clean-aindent-mode clang-format ccls browse-at-remote bison-mode avy auctex-latexmk))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#32302f"))
 '(popwin:popup-window-height 0.5 t)
 '(safe-local-variable-values
   '((eval setq compile-command
           (concat "make -C " my-project-path " all"))
     (eval set
           (make-local-variable 'my-project-path)
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "dark blue" :foreground "dark blue"))) nil '(diff-hl-delete ((t (:background "dark red" :foreground "dark red")))))
 '(org-ellipsis ((t (:foreground "gray40" :underline nil)))))
