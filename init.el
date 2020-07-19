;; garbage collector
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1)))

;; file-name-handler-list optimization
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist my--file-name-handler-alist)))

;; Window System
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 1)

(setq gnutls-min-prime-bits 1024)
(setq gnutls-algorithm-priority "SECURE128:-VERS-SSL3.0:-VERS-TLS1.3")

(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t)
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

(setq package-enable-at-startup nil)

(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-other-buffers ()
  "Kill all other buffers except scratch."
  (interactive)
  (mapc 'kill-buffer (delq "*scratch*" (buffer-list))))

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
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))))

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

(defun edit-file-with-sudo ()
  "Take the file currently being edited, and open it as root with `sudo'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load early
(require 'tramp)

;; bind-key for use-package
(require 'bind-key)

;; mode line
(display-time-mode 1)

(setq-default confirm-kill-emacs 'yes-or-no-p)
(setq-default cursor-in-non-selected-windows t)
(setq-default help-window-select t)
(setq-default x-stretch-cursor t)
(setq-default window-combination-resize t)

(setq x-underline-at-descent-line t)
(setq underline-minimum-offset 1)

(global-subword-mode 1)

(add-hook 'focus-out-hook #'garbage-collect)

;; (setq garbage-collection-messages t)
(setq jit-lock-defer-time 0)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-dont-pause t)
(setq read-process-output-max (* 1024 1024))

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

;; set new keyboard-quit
(global-set-key (kbd "C-q") 'keyboard-quit)

;; set new goto line
(global-set-key (kbd "C-g") 'goto-line)

;; disable suspend frame
(global-unset-key (kbd "C-z"))

;; text marking
(delete-selection-mode t)
(transient-mark-mode t)
(setq-default select-enable-clipboard t)
(setq-default x-select-enable-clipboard t)

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
(setq-default frame-title-format nil)
(setq-default ring-bell-function 'ignore)
(setq-default uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq-default adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq-default adaptive-fill-first-line-regexp "^* *$")
(setq-default sentence-end "\\([。、！？]\\|……\\|[,.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq-default sentence-end-double-space nil)
(setq-default set-mark-command-repeat-pop t)  ; Repeating set-mark after popping mark pops it again
(setq-default track-eol t)			; Keep cursor at end of lines.
(setq-default line-move-visual nil)		; To be required by track-eol
(setq-default kill-whole-line t)	; Kill line including '\n'
(setq-default indent-tabs-mode nil)   ; use space

(defalias 'yes-or-no-p #'y-or-n-p)

;; scratch buffer mode
;; (setq initial-major-mode 'org-mode)

;; column number
(column-number-mode 1)

;; highlighting
(global-font-lock-mode t)

;; scroll bar new-frame
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; require trailing newline
(setq-default require-final-newline t)
;; delete trailing whitespace on write
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Defer fontification a little bit, so we could gain a bit in scroll speed
(setq jit-lock-defer-time 0.02)

;; redefining sentences in emacs
(setq-default sentence-end-double-space nil)

;; font settings
(setq-default use-default-font-for-symbols nil)
(setq-default inhibit-compacting-font-caches t)

(cond ((string-equal system-type "gnu/linux")
       (setq-default default-font-family "Fira Code Retina")
       (set-frame-font "Fira Code Retina-11")
       (setq-default default-frame-alist '((font . "Fira Code Retina-11")
					   (height . 75)))
       )
      )

(set-fontset-font "fontset-default" nil (font-spec :size 11 :name "Symbola"))
(set-fontset-font "fontset-default" 'unicode "DejaVu Sans")

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

;;(add-hook 'after-save-hook #'gtags-update-hook)

;; visual bell and less keystrokes
(setq-default echo-keystrokes 0.1)
(setq-default use-dialog-box nil)
(setq-default visible-bell t)

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; backups
(setq-default backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq-default backup-by-copying t)
(setq-default delete-old-versions t)
(setq-default kept-new-versions 6)
(setq-default kept-old-version 2)
(setq-default version-control t)

;; disable warning when killing buffers
(setq-default kill-buffer-query-functions
	      (remq 'process-kill-buffer-query-function
		    kill-buffer-query-functions))

;; custom file
(setq-default custom-file "~/.config/emacs/custom.el")

;; history
(setq-default savehist-file "~/.config/emacs/savehist")
(savehist-mode 1)
(setq-default history-length t)
(setq-default history-delete-duplicates t)
(setq-default savehist-save-minibuffer-history 1)
(setq-default savehist-additional-variables
	      '(kill-ring
		search-ring
		regexp-search-ring))

;; save disk space
(setq-default delete-old-versions t)
(setq-default version-control t)
(setq-default vc-make-backup-files t)
(setq-default auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t)))

;; utf8 stuff
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; backtab delete spaces
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)

;; set font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
       (text-scale-set 1)
       (kill-local-variable 'text-scale-mode-amount)
       (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
       (global-text-scale-mode 1)
       )

(global-set-key (kbd "M-0")
		'(lambda () (interactive)
		   (global-text-scale-adjust (- text-scale-mode-amount))
		   (global-text-scale-mode -1)))
(global-set-key (kbd "M-+")
		'(lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "M--")
		'(lambda () (interactive) (global-text-scale-adjust -1)))

;; highlight NOTE and TODO
(setq-default fixme-modes '(c++-mode c-mode emacs-lisp-mode))
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
(setq compile-command "make -C .. all")
(setq compile-read-command nil)
(setq compilation-scroll-output t)
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

(global-set-key (kbd "<f5>") (lambda ()
			       (interactive)
			       (call-interactively 'recompile)))

;; disable recentf
(recentf-mode -1)
(setq recentf-mode nil)

;; GDB
(setq-default gdb-many-windows t)
(setq-default gdb-show-main	   t)

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
(set-fringe-mode fringe-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package isearch
  :defer
  :ensure nil
  :diminish
  :config
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)

  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " (%s/%s)")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  )

(use-package dired
  :defer
  :ensure nil
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  :hook ((dired-mode-hook . dired-hide-details-mode)
         (dired-mode-hook . hl-line-mode)))

(use-package dired-aux
  :defer
  :ensure nil
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t)
  )

(use-package find-dired
  :defer
  :ensure nil
  :after dired
  :config
  (setq find-ls-option
        '("-ls" . "-AGFhlv --group-directories-first --time-style=long-iso"))
  (setq find-name-arg "-iname"))

(use-package async :ensure)

(use-package dired-async
  :defer
  :ensure nil
  :after (dired async)
  :hook (dired-mode-hook . dired-async-mode))

(use-package dired-subtree
  :defer
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<S-iso-lefttab>" . dired-subtree-remove)))

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

(use-package projectile
  :diminish
  :bind
  ("M-o p" . counsel-projectile-switch-project)
  :config
  (projectile-mode +1))

(use-package gruvbox-theme
  :defer 0
  :config
  (load-theme 'gruvbox-dark-hard)
  (set-face-attribute 'mode-line nil :background "#1d2021" :foreground "#fbf1c7" :box "#fe8019")
  )

(use-package move-text
  :defer 0
  :config
  (move-text-default-bindings)
  )

(use-package cc-mode
  :config
  (setq-default c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (global-set-key (kbd "RET") 'newline-and-indent)	; automatically indent when press RET

  (global-set-key (kbd "C-c w") 'whitespace-mode)
  (add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)

  (c-set-offset 'inlambda 0)
  (fset 'c-indent-region 'clang-format-region)

  (add-hook 'before-save-hook
	    (lambda ()
	      (when (member major-mode '(c-mode c++-mode))
		(progn
		  (when (locate-dominating-file "." ".clang-format")
		    (clang-format-buffer))
		  ;; Return nil, to continue saving.
		  nil))))
  )

;; Use :defer to load steroids first
(use-package visual-regexp
  :defer
  :bind
  (("C-c r" . vr/replace)
   ("C-c q" . vr/query-replace)))

;; hack to actually load this package
(use-package visual-regexp-steroids
  :ensure
  :demand)

(use-package lua-mode)

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "  ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package multiple-cursors
  :bind (("C-c m"   . mc/edit-lines)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package cmake-mode)

(use-package magit
  :diminish magit-auto-revert-mode
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

(use-package forge
  :after magit)

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

(use-package px)

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
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package org
  :commands org-indent-mode
  :config
  (defvar root-dir "/home/romeu/Documents/Org/")
  (defvar my-org-dir root-dir)
  (defvar my-org-publish-dir (concat root-dir "Publish/"))
  (defvar my-org-meta-dir (concat root-dir "Meta/"))
  (defvar my-org-archive-dir (concat my-org-meta-dir "Archive/"))
  (defvar my-org-diary-file (concat root-dir "Diary/Diary.org"))

  (defun my-maybe-lob-ingest ()
    (if (and buffer-file-name
             (string-match
              (format "%s/.*code\\.inc$" my-org-dir)
              buffer-file-name))
        (org-babel-lob-ingest buffer-file-name)))

  (defun my-after-save-hook ()
    (my-maybe-lob-ingest))

  (defun my-org-mode-hook ()
    (my-maybe-lob-ingest)
    (turn-on-auto-fill)
    (org-indent-mode 1)
    (setq fill-column 80))

  (defun my-chromium (ppl)
    (start-process "fox" nil "open" "-a"
                   "chromium" (format "file://%s" my-org-publish-dir)))

  (defun my-git-publish (ppl)
    (let ((publish-script (format "%s/publish.sh" my-org-publish-dir)))
      (when (file-executable-p publish-script)
	(start-process-shell-command "pub" nil publish-script))))

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (member lang '("sh" "python" "elisp" "ruby" "shell" "dot" "perl"))))

  (defun my-publish (a b c)
    (setq org-export-with-toc t)
    (org-html-publish-to-html a b c)
    (setq org-export-with-toc nil)
    (org-ascii-publish-to-ascii a b c)
    (org-gfm-publish-to-gfm a b c))

  (setq org-directory my-org-dir)
  (setq org-metadir my-org-meta-dir)
  (setq org-archive-location my-org-archive-dir)
  (setq diary-file my-org-diary-file)

  (setq org-image-actual-width nil)
  (setq org-startup-indented t)
  (setq org-babel-min-lines-for-block-output 1)
  (setq org-startup-folded "showeverything")
  (setq org-startup-with-inline-images t)
  (setq org-src-preserve-indentation t)
  (setq org-use-speed-commands t)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc t)
  (setq org-export-with-date nil)
  (setq org-export-time-stamp-file nil)
  (setq org-export-with-email t)
  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  (setq org-todo-keyword-faces
        '(
          ("DONE"      . (:foreground "#afd8af"     :weight bold))
          ("WAITING"   . (:foreground "dark salmon" :weight bold))
          ("CANCELLED" . (:foreground "dim gray"    :weight bold))
          ("BUY"       . (:foreground "goldenrod"   :weight bold))
          ("HOWTO"     . (:foreground "SkyBlue3"    :weight bold))
          ("INFO"      . (:foreground "khaki1"      :weight bold))
          ("COLLECT"   . (:foreground "MediumSeaGreen"   :weight bold))
          ("SOLVE"     . (:foreground "orange red"    :weight bold))
          ))

  (setq org-ellipsis " ▼ ")
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)

  (setq org-pretty-entities 1)
  (setq org-pretty-entities-include-sub-superscripts nil)

  (setq org-descriptive-links nil)
  (setq org-src-fontify-natively t)

  (setq org-tags-column -110)
  (setq org-agenda-tags-column -110)
  (setq org-habit-graph-column 100)

  (setq org-babel-default-header-args (cons '(:noweb . "yes") (assq-delete-all :noweb org-babel-default-header-args)))
  (setq org-babel-default-header-args (cons '(:exports . "both") (assq-delete-all :exports org-babel-default-header-args)))
  (setq org-babel-default-header-args (cons '(:results . "output verbatim replace") (assq-delete-all :results org-babel-default-header-args)))


  ;; TODO states configurations
  (setq org-todo-keywords '((sequence "TODO" "WORKING" "|" "DONE")))
  (setq org-log-done 'time)
  (setq org-log-done 'note)

  (custom-set-faces '(org-ellipsis ((t (:foreground "gray40" :underline nil)))))
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)

  ;; Org Capture
  ;; TODO: improve and add more templates
  (setq org-capture-templates
        '(
          ("t" "Todo" entry (file "TODO.org")
           "* TODO %?\n%U" :empty-lines 1)
          ("s" "Songs" entry (file+headline "Songs.org" "Songs")
           "* %^{Song: } %?\n")
          )
        )

  (setq org-babel-interpreters
        (quote
         ("emacs-lisp" "python" "sh" ""))
        )

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (python . t)
     (ruby . t)
     (perl . t)
     (emacs-lisp . t)
     (dot . t)))

  ;; org crypt
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-tag-matcher "encrypt")
  (setq org-crypt-key "09852491") ;; TODO: make it a constant
  (add-to-list 'org-tags-exclude-from-inheritance (quote "encrypt"))
  (add-to-list 'org-tags-exclude-from-inheritance (quote "crypt"))

  :bind (("M-p" . #'org-publish))
  :hook
  (after-save . my-after-save-hook)
  (org-mode . my-org-mode-hook)
  )

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook #'org-bullets-mode)
  )

(use-package ox-gfm)

(use-package popwin
  :init
  (customize-set-variable 'popwin:popup-window-height 0.5)
  :config
  (popwin-mode 1)
  (setq display-buffer-function 'popwin:display-buffer)
  )

(use-package diff-hl
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
  :defer 0
  :diminish helm-ff-cache-mode
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
        helm-recentf-fuzzy-match              nil
        helm-split-window-in-side-p           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf nil
        helm-move-to-line-cycle-in-source     t
        helm-autoresize-mode                  t
	helm-ff-auto-update-initial-value     nil
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

(use-package company
  :diminish company-mode
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  :init (global-company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-echo-delay 0.1)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :config
  (defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
	(company-complete-common)
    (indent-according-to-mode)))

  (delete 'company-dabbrev company-backends)
  (delete '(company-dabbrev-code company-gtags company-etags company-keywords) company-backends)

  (setq company-require-match nil)
  (setq company-tooltip-idle-delay .25)

  (global-unset-key (kbd "C-SPC"))
  (global-set-key (kbd "C-SPC") 'company-complete)

  ;; deactivate auto complete selection
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)

  ;; Show quick tooltip
  (use-package company-quickhelp
    :defines company-quickhelp-delay
    :bind (:map company-active-map
  		("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)
    :custom (company-quickhelp-delay 0.8))
  )

(use-package lsp-mode
  :commands lsp
  :custom
  ;; debug
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  ;; general
  (lsp-auto-guess-root t)
  (lsp-prefer-flymake nil)
  ;; snippet
  (lsp-enable-snippet nil)
  ;; force disable highlight
  (lsp-enable-semantic-highlighting nil)
  (lsp-diagnostic-package :none)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (typescript-mode . lsp)
  (python-mode . lsp)
  :config
  (use-package lsp-ui
    :custom
    ;; lsp-ui-doc
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    ;; lsp-ui-flycheck
    (lsp-ui-flycheck-enable t)
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
  )

(use-package eldoc
  :ensure nil
  :config (global-eldoc-mode -1))

(use-package iedit
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

(use-package csv-mode)

(use-package avy
  :bind ("M-s" . avy-goto-char))

(use-package all-the-icons
  :config
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)
  )

(put 'upcase-region 'disabled nil)
(load custom-file)
