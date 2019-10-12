;; Window System
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 1)

(setq gnutls-min-prime-bits 1024)
(setq gnutls-algorithm-priority "SECURE128:-VERS-SSL3.0:-VERS-TLS1.3")


(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize))

(setq load-prefer-newer t)

(setq gc-cons-threshold 50000000)

(setenv "GTAGSLIBPATH" "/usr/include")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prev-window ()
  "CHANGE WINDOW."
  (interactive)
  (other-window -1))

(defun comment-auto-fill ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode 1))

(defun my-minibuffer-setup-hook ()
  "GC BUFFER."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "GC BUFFER."
  (setq gc-cons-threshold 800000))

(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
	(save-match-data
	  (beginning-of-line)
	  ;; get rid of tabs at beginning of line
	  (when (looking-at "^\\s-+")
		(untabify (match-beginning 0) (match-end 0)))
	  (when (looking-at "^	  ")
		(replace-match "")))))

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point)))
		eol)
	(save-excursion
	  (end-of-line)
	  (setq eol (point))
	  (let ((line (buffer-substring bol eol))
			(buffer-undo-list t)
			(count arg))
		(while (> count 0)
		  (newline)			;; because there is no newline in 'line'
		  (insert line)
		  (setq count (1- count)))
		)
	  (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
	)
  (next-line arg))

(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
						   '((vertical-scroll-bars . nil)
							 (horizontal-scroll-bars . nil))))

(defun bookmark-to-abbrevs ()
   "Create abbrevs based on `bookmark-alist'."
   (dolist (bookmark bookmark-alist)
   (let* ((name (car bookmark))
		  (file (bookmark-get-filename name)))
	 (define-abbrev global-abbrev-table name file))))

(defun endless/flycheck-dir (dir)
  "Run flycheck for each file in current directory.
Results are reported in a compilation buffer."
  (interactive "DDirectory: ")
  (displaying-byte-compile-warnings
   (let ((p nil))
	 (with-current-buffer (get-buffer-create
						   byte-compile-log-buffer)
	   (setq default-directory dir)
	   (unless (eq major-mode 'compilation-mode)
		 (compilation-mode))
	   (goto-char (point-max))
	   (let ((inhibit-read-only t))
		 (insert "\n\xc\n\n"))
	   (setq p (point)))
	 (dolist (file (directory-files "./" nil
									"\\`[^\\.].*\\'"))
	   (endless/-flycheck-file file))
	 (with-selected-window (display-buffer
							byte-compile-log-buffer)
	   (goto-char p)
	   (recenter 1)))))

(defun endless/-report-error (fmt &rest args)
  "Print an error on `byte-compile-log-buffer'."
  (let ((inhibit-read-only t)
		(fill-prefix "	  "))
	(with-current-buffer byte-compile-log-buffer
	  (let ((l (point)))
		(insert "\n" (apply #'format fmt args))
		(fill-region (1+ l) (point))))))

(defun endless/-flycheck-file (file)
  "Check FILE and report to `byte-compile-log-buffer'."
  (let ((was-visited (find-buffer-visiting file)))
	(with-current-buffer (or was-visited
							 (progn (find-file file)
									(current-buffer)))
	  (when (ignore-errors (flycheck-buffer))
		(while (flycheck-running-p)
		  (accept-process-output nil 0.1))
		(pcase flycheck-last-status-change
		  ((or `errored `suspicious)
		   (endless/-report-error
			"%s: Something wrong here!"
			(file-name-nondirectory (buffer-file-name))))
		  (`finished
		   (dolist (e flycheck-current-errors)
			 (endless/-report-error
			  "%s:%s:%s:%s: %s"
			  (file-name-nondirectory (buffer-file-name))
			  (flycheck-error-line e)
			  (flycheck-error-column e)
			  (flycheck-error-level e)
			  (flycheck-error-message e))))))
	  (if was-visited
		  (bury-buffer was-visited)
		(kill-buffer (current-buffer))))))

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
				 ((not prefix) "%d.%m.%Y")
				 ((equal prefix '(4)) "%Y-%m-%d")
				 ((equal prefix '(16)) "%A, %d. %B %Y")))
		(system-time-locale "de_DE"))
	(insert (format-time-string format))))

(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
	(if (zerop (call-process "global" nil t nil "-pr"))
		(buffer-substring (point-min) (1- (point-max)))
	  nil)))

(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-hook ()
  (when (gtags-root-dir)
	(gtags-update)))

(defadvice move-beginning-of-line (around smarter-bol activate)
  ;; Move to requested line if needed.
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))

(defun *-package-upgrade ()
  "Upgrade all packages"
  (interactive)
  (save-window-excursion
    (with-temp-buffer
      (package-list-packages)
      (package-menu-mark-upgrades)
      (package-menu-mark-obsolete-for-deletion)
      (package-menu-execute t))))

(defun *-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

(defun run-python3 ()
  (interactive)
  (run-python  "/usr/bin/python2" nil t))

(defun run-python2 ()
  (interactive)
  (run-python  "/usr/bin/python" nil t))

(defun run-ipython3 ()
  (interactive)
  (run-python  "/usr/bin/ipython" nil t))

(defun python/pip-search ()
  "Search for a pip package. at: https://pypi.python.org"
  (interactive)
  (browse-url
   (format "https://pypi.python.org/pypi?%%3Aaction=search&term=%s&submit=search"
           (read-string "Pip: ")
           (message "Searching pip.")
		   )))

(defun python/doc-search ()
  "Search Python3 official documentation. at: https://docs.python.org"
  (interactive)
  (browse-url
   (format "https://docs.python.org/3/search.html?q=%s"
           (read-string "Python3 doc: ")
           )))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun me/date-iso ()
  "Insert the current date, ISO format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun me/date-iso-with-time ()
  "Insert the current date, ISO format with time, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun me/date-long ()
  "Insert the current date, long format, eg. December 09, 2016."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

(defun me/date-long-with-time ()
  "Insert the current date, long format, eg. December 09, 2016 - 14:34."
  (interactive)
  (insert (capitalize (format-time-string "%B %d, %Y - %H:%M"))))

(defun me/date-short ()
  "Insert the current date, short format, eg. 2016.12.09."
  (interactive)
  (insert (format-time-string "%Y.%m.%d")))

(defun me/date-short-with-time ()
  "Insert the current date, short format with time, eg. 2016.12.09 14:34"
  (interactive)
  (insert (format-time-string "%Y.%m.%d %H:%M")))

(defun me/delete-old-backup-files ()
  "Delete old backup files."
  (interactive)
  (message "Deleting old backup files...")
  (let ((week (* 60 60 24 7))
		(current (float-time (current-time))))
	(dolist (file (directory-files temporary-file-directory t))
	  (when (and (backup-file-name-p file)
				 (> (- current (float-time (fifth (file-attributes file))))
					week))
		(message "%s" file)
		(delete-file file)))))

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load early
(require 'tramp)

(setq-default confirm-kill-emacs 'yes-or-no-p
			  cursor-in-non-selected-windows t
			  help-window-select t
			  x-stretch-cursor t)

(global-subword-mode)

(add-hook 'focus-out-hook #'garbage-collect)

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (exec-path-from-shell-initialize))

;; mouse settings
(xterm-mouse-mode t)
(global-unset-key [mouse-2])
(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 3)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   3)))

;; new home key action
(global-set-key (kbd "<home>") 'move-beginning-of-line)

;; comments
(global-set-key (kbd "C-x C-;") #'*-comment-or-uncomment-region-or-line)

;; my custom C-backspace
(global-set-key [C-backspace] 'my-backward-delete-word)

;; disable suspend frame
(global-unset-key (kbd "C-z"))

;; text marking
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; cursor type
(setq-default cursor-type 'box)

;; auto fill comments
(add-hook 'prog-mode-hook 'comment-auto-fill)

;; quiet startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message ""))
(setq frame-title-format nil)
(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq set-mark-command-repeat-pop t)  ; Repeating set-mark after popping mark pops it again
(setq track-eol t)			; Keep cursor at end of lines.
(setq line-move-visual nil)		; To be required by track-eol
(setq-default kill-whole-line t)	; Kill line including '\n'
(setq-default indent-tabs-mode nil)   ; use space
(defalias 'yes-or-no-p #'y-or-n-p)

;; startup mode
(setq initial-major-mode 'org-mode)

;; column number
(column-number-mode 1)

;; highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; scroll bar new-frame
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; require trailing newline
(setq-default require-final-newline t)
;; delete trailing whitespace on write
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; isearch
(setq-default
 isearch-allow-scroll t
 lazy-highlight-cleanup nil
 lazy-highlight-initial-delay 0)

;; redefining sentences in emacs
(setq-default sentence-end-double-space nil)

;; font settings
(setq use-default-font-for-symbols nil)
(setq inhibit-compacting-font-caches t)

(cond ((string-equal system-type "gnu/linux"))
	  (setq default-font-family "Source Code Pro Medium")
	  (set-frame-font "Source Code Pro Medium-11")
	  (setq default-frame-alist '((font . "Source Code Pro Medium-11")))
	  )

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

(add-hook 'after-save-hook #'gtags-update-hook)

;; marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; visual bell and less keystrokes
(setq echo-keystrokes 0.1
	  use-dialog-box nil
	  visible-bell t)

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
	  backup-by-copying t
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-version 2
	  version-control t)

;; disable warning when killing buffers
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; custom file
(setq custom-file "~/.emacs.d/custom.el")

;; history
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
	  '(kill-ring
		search-ring
		regexp-search-ring))

;; save disk space
(setq delete-old-versions t)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; utf-8
;; (prefer-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (when (display-graphic-p)
;;   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT
;; STRING)))
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8-unix
(setq save-buffer-coding-system 'utf-8-unix) ; nil
(setq process-coding-system-alist
  (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; backtab delete spaces
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)

;; set font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; highlight NOTE and TODO
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
		(font-lock-add-keywords
		 mode
		 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
		   ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
	  fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

;; compilation
(global-set-key (kbd "<f5>") (lambda ()
							   (interactive)
							   (call-interactively 'compile)))

;; UTF8
;; (set-fontset-font t 'unicode "Symbola" nil 'prepend)

;; GDB
(setq gdb-many-windows t
	  gdb-show-main	   t)

(add-hook 'c-mode-hook (lambda () (local-unset-key (kbd "C-d"))))
(add-hook 'c++-mode-hook (lambda () (local-unset-key (kbd "C-d"))))

(global-set-key (kbd "C-d") 'duplicate-line)

;; bison & lex
(add-to-list 'auto-mode-alist '("\\.y\\'" ignore t))
(add-to-list 'auto-mode-alist '("\\.l\\'" ignore t))

;; Use more convenient bindings for previous-error and next-error.
(dolist (key (append (where-is-internal #'previous-error)
                     (where-is-internal #'next-error)))
  (global-unset-key key))
(global-set-key (kbd "M-N") #'next-error)
(global-set-key (kbd "M-P") #'previous-error)

(fringe-mode '(nil . 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; Recent files
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 20000000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '((expand-file-name package-user-dir)
                     ".cache"
                     "cache"
                     "recentf"
                     "COMMIT_EDITMSG\\'"))
  :preface
  (defun ladicle/recentf-save-list-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if (fboundp 'shut-up)
          (shut-up (recentf-save-list))
        (recentf-save-list)))
    (message ""))
  (defun ladicle/recentf-cleanup-silence ()
    (interactive)
    (let ((message-log-max nil))
      (if shutup-p
          (shut-up (recentf-cleanup))
        (recentf-cleanup)))
    (message ""))
  :hook
  (focus-out-hook . (ladicle/recentf-save-list-silence ladicle/recentf-cleanup-silence)))

(use-package projectile
  :diminish
  :bind
  ("M-o p" . counsel-projectile-switch-project)
  :config
  (projectile-mode +1))

(use-package gruvbox-theme
  :defer 1
  :config
  (load-theme 'gruvbox-dark-hard))

(use-package rainbow-delimiters
  :defer 1
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package cc-mode
  :defer 1
  :config
  (setq c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (global-set-key (kbd "RET") 'newline-and-indent)	; automatically indent when press RET

  (global-set-key (kbd "C-c w") 'whitespace-mode)
  (add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)

  (c-set-offset 'inlambda 0)
  (fset 'c-indent-region 'clang-format-region)
  )

(use-package visual-regexp
  :defer 1
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

(use-package visual-regexp-steroids
  :defer 1)

(use-package lua-mode
  :defer 1)

(use-package uniquify
  :defer 1
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "  ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  )

(use-package markdown-mode
  :defer 1
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package multiple-cursors
  :defer 1
  :bind (("C-c m"   . mc/edit-lines)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package cmake-mode
  :defer 1)

(use-package magit
  :defer 1
  :init
  (set-default 'magit-stage-all-confirm nil)
  (add-hook 'magit-mode-hook 'magit-load-config-extensions)
  :custom
  (magit-auto-revert-mode nil)
  :config
  (defun magit-quick-commit ()
	(interactive)
	(magit-stage-modified)
	(magit-commit)))

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

(use-package browse-at-remote
  :bind ("M-g r" . browse-at-remote))

(use-package diffview
  :commands (diffview-region diffview-current)
  :preface
  (defun rg/diffview-dwim ()
    (interactive)
    (if (region-active-p)
        (diffview-region)
      (diffview-current)))
  :bind ("M-g v" . rg/diffview-dwim))

(use-package px
  :defer 1)

(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  (after-init . show-smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap))
  (sp-with-modes '(c-mode c++-mode)
	(sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
	(sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
											  ))))

(use-package clean-aindent-mode
  :defer 1
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package smart-mode-line
  :defer 1
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 40)
  (sml/setup))

(use-package org
  :defer 1
  :custom
  (org-src-fontify-natively t)
  (org-startup-folded nil)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-log-done t)
  (org-startup-indented t)
  (org-pretty-entities t)
  :init
  (customize-set-variable 'org-ellipsis "↴")
  (setq org-agenda-files (list "~/Documents/Notes/projects.org"
                               "~/Documents/Notes/todo.org")))

(use-package popwin
  :defer 1
  :init
  (customize-set-variable 'popwin:popup-window-height 0.5)
  :config
  (popwin-mode 1))

(use-package diff-hl
  :defer 1
  :init
  (custom-set-faces
   '(diff-hl-change ((t (:background "dark blue" :foreground "dark blue")))
					'(diff-hl-insert ((t (:background "dark green" :foreground "dark green"))))
					'(diff-hl-delete ((t (:background "dark red" :foreground "dark red"))))))
  (diff-hl-flydiff-mode)
  (diff-hl-margin-mode)
  (global-diff-hl-mode 1)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package helm
  :defer 1
  :config

  (require 'helm-config)

  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c h o") 'helm-occur)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (when (executable-find "ack")
    (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 30)

  (setq helm-M-x-fuzzy-match                  t
        helm-semantic-fuzzy-match             t
        helm-imenu-fuzzy-match                t
        helm-buffer-fuzzy-matching            t
        helm-recentf-fuzzy-match              t
        helm-split-window-in-side-p           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t
        helm-move-to-line-cycle-in-source     t
        helm-autoresize-mode                  t
		helm-ff-auto-update-initial-value     t
        )

  (customize-set-variable 'helm-apropos-function-list
						  '(helm-def-source--emacs-commands
							helm-def-source--emacs-functions
							helm-def-source--emacs-variables
							helm-def-source--emacs-faces))

  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

  ;; recover old keybinds
  (define-key helm-map (kbd "<left>") 'helm-previous-source)
  (define-key helm-map (kbd "<right>") 'helm-next-source)

  (customize-set-variable 'helm-ff-lynx-style-map t)
  (customize-set-variable 'helm-imenu-lynx-style-map t)
  (customize-set-variable 'helm-semantic-lynx-style-map t)
  (customize-set-variable 'helm-occur-use-ioccur-style-keys t)

  (helm-mode 1))

(use-package helm-gtags
  :defer 1
  :config
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode))

(use-package company
  :defer 1
  :diminish company-mode
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  :init (global-company-mode)
  :hook
  ((c++-mode
    c-mode) . (lambda () (set (make-local-variable 'company-backends)
                            '((;; company-yasnippet
                               company-lsp
                               company-files
                               ;; company-dabbrev-code
                               )))))
   :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  :config

  (defun indent-or-complete ()
	(interactive)
	(if (looking-at "\\_>")
		(company-complete-common)
	  (indent-according-to-mode)))

  (setq company-require-match nil)
  (setq company-tooltip-idle-delay .25)

  (global-unset-key (kbd "C-SPC"))
  (global-set-key (kbd "C-SPC") 'company-complete)

  ;; Show quick tooltip
  (use-package company-quickhelp
	:defines company-quickhelp-delay
	:bind (:map company-active-map
				("M-h" . company-quickhelp-manual-begin))
	:hook (global-company-mode . company-quickhelp-mode)
	:custom (company-quickhelp-delay 0.8)))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-document-sync-method 'incremental) ;; none, full, incremental, or nil
  (lsp-response-timeout 10)
  (lsp-prefer-flymake nil) ;; t(flymake), nil(lsp-ui), or :none
  :config
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
   (use-package lsp-ui
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix "")
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
         (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
    ;; ("C-c C-r" . lsp-ui-peek-find-references)
    ;; ("C-c C-j" . lsp-ui-peek-find-definitions)
    ;; ("C-c i"   . lsp-ui-peek-find-implementation)
    ;; ("C-c m"   . lsp-ui-imenu)
    ("C-c s"   . lsp-ui-sideline-mode)
    ("C-c d"   . ladicle/toggle-lsp-ui-doc))
    :hook
    (lsp-mode . lsp-ui-mode))

   (use-package company-lsp
	 :ensure t
	 :commands company-lsp
	 :custom
	 (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
	 (company-lsp-async t)
	 (company-lsp-enable-snippet t)
	 (company-lsp-enable-recompletion t)
	 :config (push 'company-lsp company-backends)))

(use-package ccls
  :ensure t
  :custom
  (ccls-executable "ccls")
  (lsp-prefer-flymake nil)
  ;;(ccls-sem-highlight-method 'font-lock)
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package eldoc
  :defer 1
  :ensure nil
  :config (global-eldoc-mode -1))

(use-package iedit
  :defer 1
  :preface
  (defun iedit-dwim (arg)
	"Starts iedit but uses \\[narrow-to-defun] to limit its scope."
	(interactive "P")
	(if arg
		(iedit-mode)
	  (save-excursion
		(save-restriction
		  (widen)
		  ;; this function determines the scope of `iedit-start'.
		  (if iedit-mode
			  (iedit-done)
			;; `current-word' can of course be replaced by other
			;; functions.
			(narrow-to-defun)
			(iedit-start (current-word) (point-min) (point-max)))))))
  :config
  (global-set-key (kbd "C-;") 'iedit-dwim)
  )

(use-package csv-mode
  :defer 1)

;; (use-package semantic
;;   :defer 1
;;   :init
;;   (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;;   :config
;;   (semantic-mode 1))

;; (use-package stickyfunc-enhance
;;   :defer 1)

;; (use-package flycheck
;;   :defer 1
;;   :init
;;   (add-hook 'c-mode-hook (lambda () (setq flycheck-gcc-language-standard "c99")))
;;   (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   )

;; (use-package ggtags
;;   :defer 1
;;   :config
;;   (setq ggtags-sort-by-nearness nil) ; Enabling nearness requires global 6.5+
;;   (setq ggtags-navigation-mode-lighter nil)
;;   (setq ggtags-mode-line-project-name nil)
;;   (setq ggtags-oversize-limit (* 30 1024 1024)) ; 30 MB

;;   (dolist (hook '(c-mode-hook
;; 				  c++-mode-hook))
;;     (add-hook hook #'ggtags-mode))
;;   )

;; (use-package elpy
;;   :defer 1
;;   :init
;;   (elpy-enable)
;;   :config
;;   (setq python-shell-prompt-detect-failure-warning nil)
;;   (setq python-shell-interpreter "ipython")
;;   (setq python-shell-interpreter-args "--simple-prompt --pprint")

;;   (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")
;;   (define-key python-mode-map (kbd "RET") 'newline-and-indent)

;;   (font-lock-add-keywords
;;    'python-mode
;;    '(("^[^\n]\\{80\\}\\(.*\\)$"
;;       1 font-lock-warning-face prepend)))
;;   )

;; (use-package tex
;;   :defer 1
;;   :init
;;   (load "auctex.el" nil t t)
;;   (add-hook 'LaTeX-mode-hook (lambda ()
;; 							   (TeX-global-PDF-mode t)
;; 							   ))
;;   (add-hook 'LaTeX-mode-hook 'reftex-mode)
;;   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;   (add-hook 'LaTeX-mode-hook 'visual-line-mode)

;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq TeX-view-program-list '(("Zathura" "zathura %o")))
;;   )

(put 'upcase-region 'disabled nil)
(load custom-file)
