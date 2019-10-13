;; misc settings
;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
     ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
     ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
     ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
     ("Glossaries" "makeglossaries %s" TeX-run-command nil t :help "Run makeglossaries to create glossary file")
     ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
     ("upMendex" "upmendex %s" TeX-run-index t t :help "Run mendex to create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Xelatex" "xelatex %s" TeX-run-command nil t))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open"))))
 '(blink-cursor-mode t)
 '(bmkp-last-as-first-bookmark-file "/home/romeu/.emacs.d/bookmarks")
 '(ccls-executable "ccls" t)
 '(column-number-mode t)
 '(company-c-headers-path-system
   (quote
    ("/usr/local/include/" "/usr/include/c++/6.2.1/" "/usr/x86_64-w64-mingw32/include/" "/usr/include" "/lib/modules/4.7.6-1-ARCH/build/include/")))
 '(company-clang-arguments
   (quote
    ("-I/lib/modules/4.9.6-1-ARCH/build/include/" "-I/usr/x86_64-w64-mingw32/include/")))
 '(company-clang-insert-arguments t)
 '(company-echo-delay 0 t)
 '(company-idle-delay 0)
 '(company-lsp-async t)
 '(company-lsp-cache-candidates t)
 '(company-lsp-enable-recompletion t)
 '(company-lsp-enable-snippet t)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-delay 0.8 t)
 '(company-tooltip-align-annotations t)
 '(compile-command
   "make -k -j4 -C /home/romeu/Documents/Projects/CHIP8_IMGUI/cmake-build-debug")
 '(custom-safe-themes
   (quote
    ("a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "48708977f2006e931015c50935b43871f22eb9d58a30f35ee3a480bfd1943764" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "5fc373710ac9beee070cccd4b749e8a6a9e938a482d99e4187a95e1d09ac0895" "79524534a996535c29ed3ded96421524174ca14da23463332515da690d79e6d2" "d2d79dc5c93b7dd253a45cfe8f1a0c926ddd42a6d4809d00e91a559598c6be17" "c86f868347919095aa44d2a6129dd714cbcf8feaa88ba954f636295b14ceff8f" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" default)))
 '(disaster-objdump "objdump -d -M intel -Sl --no-show-raw-insn")
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-sane-defaults)))
 '(flycheck-clang-include-path nil)
 '(flycheck-disabled-checkers (quote (tex-lacheck tex-chktex)))
 '(global-hl-line-mode nil)
 '(global-semantic-decoration-mode t)
 '(global-subword-mode t)
 '(helm-apropos-function-list
   (quote
    (helm-def-source--emacs-commands helm-def-source--emacs-functions helm-def-source--emacs-variables helm-def-source--emacs-faces)))
 '(helm-ff-lynx-style-map t)
 '(helm-imenu-lynx-style-map t t)
 '(helm-occur-use-ioccur-style-keys t)
 '(helm-semantic-lynx-style-map t t)
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(irony-cdb-search-directory-list (quote ("." "build")))
 '(lsp-auto-guess-root t t)
 '(lsp-document-sync-method (quote incremental) t)
 '(lsp-log-io nil)
 '(lsp-prefer-flymake nil t)
 '(lsp-print-io nil t)
 '(lsp-print-performance nil t)
 '(lsp-response-timeout 10 t)
 '(lsp-trace nil t)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-doc-header t)
 '(lsp-ui-flycheck-enable nil)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-imenu-kind-position (quote top))
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-fontify (quote on-demand))
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
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/Documents/Notes/projects.org")))
 '(org-ellipsis "↴")
 '(org-log-done t)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(org-src-preserve-indentation t)
 '(org-src-tab-acts-natively t)
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(package-selected-packages
   (quote
    (uniquify org-ref org-web-tools browse-at-remote diffview git-timemachine company-quickhelp hungry-delete ccls lsp-ui lsp-mode company-lsp rainbow-delimiters clang-format helm-ag restclient disable-mouse diff-hl srefactor bison-mode popwin exec-path-from-shell auctex rust-mode helm-gtags cmake-mode org-bullets ace-window material-theme csv-mode px magic-latex-buffer org company flycheck helm esup w3m color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized color-theme-solarized solarized-theme pretty-mode discover-my-major pcre2el visual-regexp visual-regexp-steroids ws-butler use-package stickyfunc-enhance sr-speedbar spacemacs-theme sml-modeline smartparens smart-mode-line pdf-tools nasm-mode multiple-cursors mode-icons markdown-mode magit lua-mode latex-preview-pane latex-pretty-symbols iedit htmlize gruvbox-theme ggtags function-args flycheck-tip company-php clean-aindent-mode)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#1d2021")))
 '(popwin:popup-window-height 0.5 t)
 '(preview-gs-options
   (quote
    ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(racer-rust-src-path "/home/romeu/Documents/Repositories/rust/src")
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude
   (quote
    ((expand-file-name package-user-dir)
     ".cache" "cache" "recentf" "COMMIT_EDITMSG\\'")))
 '(recentf-max-saved-items 20000000)
 '(sml/modified-char "X")
 '(sml/modified-time-string "Modified on %T %Y/%m/%d.")
 '(sp-escape-quotes-after-insert nil)
 '(speedbar-show-unknown-files t)
 '(tls-program
   (quote
    ("gnutls-cli --x509cafile %t -p %p %h" "gnutls-cli --x509cafile %t -p %p %h --protocols ssl3")))
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:background "dark blue" :foreground "dark blue"))) nil (quote (diff-hl-delete ((t (:background "dark red" :foreground "dark red"))))))
 '(diff-hl-delete ((t (:background "dark red" :foreground "dark red"))))
 '(diff-hl-insert ((t (:background "dark green" :foreground "dark green"))))
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline "ForestGreen"))))
 '(flycheck-warning ((t (:underline "DarkOrange"))))
 '(semantic-decoration-on-includes ((t nil)) t)
 '(zjl-hl-function-call-face ((t (:foreground "#83A598" :weight bold)))))
