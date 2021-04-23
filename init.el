(setq gc-cons-threshold 60000000) ; 100mb

;; file-name-handler-list optimization
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq file-name-handler-alist my--file-name-handler-alist)))

;; Window System
;; (unless (display-graphic-p)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 1)
;; )

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

;; load theme earlier because of off-emacs org-mode conversion
(load-theme 'gruvbox-dark-hard t)
(set-face-attribute 'mode-line nil :background "#1d2021" :foreground "#fbf1c7" :box "#fe8019")

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

(defun prot/window-single-toggle ()
  "Toggle between multiple windows and single window.
This is the equivalent of maximising a window.  Tiling window
managers such as DWM, BSPWM refer to this state as 'monocle'."
  (interactive)
  (if (one-window-p)
      (when prot/window-configuration
        (set-window-configuration prot/window-configuration))
    (setq prot/window-configuration (current-window-configuration))
    (delete-other-windows)))

(defun prot/window-dired-vc-root-left ()
  (interactive)
  (let ((dir (if (eq (vc-root-dir) nil)
                 (dired-noselect default-directory)
               (dired-noselect (vc-root-dir)))))
    (display-buffer-in-side-window
     dir `((side . left)
           (slot . -1)
           (window-width . 0.16)
           (window-parameters
            . ((no-other-window . t)
               (no-delete-other-windows . t)
               (mode-line-format
                . (" "
                   mode-line-buffer-identification))))))
    (with-current-buffer dir
      (rename-buffer "*Dired-Side*")
      (setq-local window-size-fixed 'width)))
  (with-eval-after-load 'ace-window
    (when (boundp 'aw-ignored-buffers)
      (add-to-list 'aw-ignored-buffers "*Dired-Side*"))))

(defun ap/garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

(defun desperately-compile ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (when (locate-dominating-file default-directory "Makefile")
    (with-temp-buffer
      (cd (locate-dominating-file default-directory "Makefile"))
      (compile "make -k"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONFIGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; terminal bindings
(unless (display-graphic-p)
  (progn
    (define-key input-decode-map "\e[1;2A" [(shift up)])
    (define-key input-decode-map "\e[1;2B" [(shift down)])
    (define-key input-decode-map "\e[1;2C" [(shift right)])
    (define-key input-decode-map "\e[1;2D" [(shift left)])
    (define-key input-decode-map "\e[1;3A" [(alt up)])
    (define-key input-decode-map "\e[1;3B" [(alt down)])
    (define-key input-decode-map "\e[1;3C" [(alt right)])
    (define-key input-decode-map "\e[1;3D" [(alt left)])
    (define-key input-decode-map "\e[1;5A" [(control up)])
    (define-key input-decode-map "\e[1;5B" [(control down)])
    (define-key input-decode-map "\e[1;5C" [(control right)])
    (define-key input-decode-map "\e[1;5D" [(control left)])
    ))

;; load early
(require 'tramp)

;; bind-key for use-package
(require 'bind-key)

;; disable abbrev-mode
(abbrev-mode -1)

(remove-hook 'text-mode-hook #'abbrev-mode)
(remove-hook 'c-mode-hook #'abbrev-mode)
(remove-hook 'c++-mode-hook #'abbrev-mode)

;; don’t warn for large file
(setq large-file-warning-threshold nil)

;; don’t warn for symlink
(setq vc-follow-symlinks t)

;; Don’t warn when advice is added for functions
(setq ad-redefinition-action 'accept)

(add-hook 'text-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'c-mode-hook (lambda () (abbrev-mode -1)))
(add-hook 'c++-mode-hook (lambda () (abbrev-mode -1)))

(add-hook 'text-mode-hook (lambda () (TeX-PDF-mode -1)))
(add-hook 'c-mode-hook (lambda () (TeX-PDF-mode -1)))
(add-hook 'c++-mode-hook (lambda () (TeX-PDF-mode -1)))

(setq-default confirm-kill-emacs 'yes-or-no-p)
(setq-default cursor-in-non-selected-windows t)
(setq-default help-window-select t)
(setq-default x-stretch-cursor t)

(setq x-underline-at-descent-line t)
(setq underline-minimum-offset 1)

(global-subword-mode 1)

(add-hook 'focus-out-hook #'garbage-collect)

(setq use-file-dialog nil)

;; set fill columns
(setq-default fill-column 80)

;; always left-to-right text
(setq-default bidi-paragraph-direction 'left-to-right)

;; (setq garbage-collection-messages t)
(setq jit-lock-defer-time 0)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-dont-pause t)
(setq read-process-output-max (* 1024 1024))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (exec-path-from-shell-initialize))

(size-indication-mode t)
(setq auto-window-vscroll nil)

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
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-h h"))

;; compile
(global-set-key [f5] 'desperately-compile)

;; text marking
(delete-selection-mode t)
(transient-mark-mode t)
(setq-default select-enable-clipboard t)
(setq-default x-select-enable-clipboard t)

;; cursor type
(setq-default cursor-type 'box)

;; quiet startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(defun display-startup-echo-area-message ()
  (message ""))
(setq-default frame-title-format nil)
(setq-default ring-bell-function 'ignore)
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

;; column number
(column-number-mode 1)

;; highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Paste with middle mouse button doesn't move the cursor
(setq mouse-yank-at-point t)

;; Silence ad-handle-definition about advised functions getting redefined
(setq ad-redefinition-action 'accept)

;; Use 'fancy' ellipses for truncated strings
(setq truncate-string-ellipsis "…")

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
       (setq-default default-font-family "Sarasa Term K")
       (set-frame-font "Sarasa Term K-12")
       (setq-default default-frame-alist '((font . "Sarasa Term K-12")
					   (height . 75)))
       )
      )

(set-fontset-font "fontset-default" nil (font-spec :size 11 :name "Symbola"))
(set-fontset-font "fontset-default" 'unicode "DejaVu Sans")

(global-set-key (kbd "C-.") 'other-window)
(global-set-key (kbd "C-,") 'prev-window)

;; visual bell and less keystrokes
(setq-default echo-keystrokes 0.1)
(setq-default use-dialog-box nil)
(setq-default visible-bell t)

;; (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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

;; (fringe-mode '(nil . 0))
;; (set-fringe-mode 0)
(fringe-mode '(nil . 0))
(set-fringe-mode fringe-mode)

;; modeline
(setq-default mode-line-buffer-identification
              (let ((orig  (car mode-line-buffer-identification)))
                `(:eval (cons (concat ,orig (abbreviate-file-name default-directory))
                              (cdr mode-line-buffer-identification)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multiple-cursors
  :bind (("C-c m"   . mc/edit-lines)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


(use-package hungry-delete
  :diminish
  :config
  (setq hungry-delete-chars-to-skip " \t\r\f\v")

  (defun rvg/hungry-delete-off ()
    (hungry-delete-mode -1))

  ;; hungry delete mode doesnt play well with multiple cursors...
  (add-hook 'multiple-cursors-mode-enabled-hook #'rvg/hungry-delete-off)
  (add-hook 'multiple-cursors-mode-disabled-hook #'rvg/hungry-delete-off)

  (global-hungry-delete-mode)
  )

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery t)
  (setq show-paren-when-point-inside-paren nil)
  :hook (after-init-hook . show-paren-mode)
  )

(use-package electric
  :config
  (setq electric-pair-inhibit-predicate'electric-pair-conservative-inhibit)
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-pairs
        '((8216 . 8217)
          (8220 . 8221)
          (171 . 187)))
  (setq electric-pair-skip-self 'electric-pair-default-skip-self)
  (setq electric-pair-skip-whitespace nil)
  (setq electric-pair-skip-whitespace-chars
        '(9
          10
          32))
  (setq electric-quote-context-sensitive t)
  (setq electric-quote-paragraph t)
  (setq electric-quote-string nil)
  (setq electric-quote-replace-double t)
  :hook (after-init-hook . (lambda ()
                             (electric-indent-mode 1)
                             (electric-pair-mode 1)
                             (electric-quote-mode 1)))
  )

(use-package ansi-color
  :commands ansi-color-display
  :hook (compilation-filter-hook . colorize-compilation-buffer)
  :config
  (defun ansi-color-display (start end)
    "Display ansi colors in region or whole buffer."
    (interactive (if (region-active-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region start end)))

  ;; Colorize output of Compilation Mode, see
  ;; http://stackoverflow.com/a/3072831/355252
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package compile
  :config
  (setq compile-command "make -C .. all")
  (setq compile-read-command nil)

  ;; Always save before compiling
  (setq compilation-ask-about-save nil)
  ;; Just kill old compile processes before starting the new one
  (setq compilation-always-kill t)
  ;; Scroll with the compilation output
  ;; Set to 'first-error to stop scrolling on first error
  (setq compilation-scroll-output t))

(use-package epa
  :defer t
  :config
  ;; Always replace encrypted text with plain text version
  (setq epa-replace-original-text t))

(use-package epg
  :defer t
  :config
  ;; Let Emacs query the passphrase through the minibuffer
  (setq epg-pinentry-mode 'loopback))

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

  (setq search-default-mode 'char-fold-to-regexp)
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

(use-package window
  :ensure nil
  :init
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  :hook ((help-mode-hook . visual-line-mode)
         (custom-mode-hook . visual-line-mode))
  )

(use-package diary-lib
  :ensure nil
  :defer
  :config
  (setq diary-file "~/.config/emacs/diary")
  (setq diary-entry-marker "diary")
  (setq diary-show-holidays-flag t)
  (setq diary-header-line-flag nil)
  (setq diary-mail-addr "romeu.bizz@gmail.com")
  (setq diary-mail-days 3)
  (setq diary-number-of-entries 3)
  (setq diary-comment-start ";")
  (setq diary-comment-end "")
  (setq diary-date-forms
        '((day "/" month "[^/0-9]")
          (day "/" month "/" year "[^0-9]")
          (day " *" monthname " *" year "[^0-9]")
          (monthname " *" day "[^,0-9]")
          (monthname " *" day ", *" year "[^0-9]")
          (year "[-/]" month "[-/]" day "[^0-9]")
          (dayname "\\W"))))

(use-package calendar
  :ensure nil
  :defer
  :config
  (setq calendar-mark-diary-entries-flag t)
  (setq calendar-time-display-form
        '(24-hours ":" minutes
                   (when time-zone
                     (concat " (" time-zone ")"))))
  (setq calendar-week-start-day 1)      ; Monday
  (setq calendar-date-style 'iso)
  (setq calendar-christian-all-holidays-flag nil)
  (setq calendar-holiday-marker t)
  (setq calendar-holidays
        (append holiday-local-holidays  ; TODO set local holidays
                holiday-solar-holidays))

  (use-package solar
    :ensure nil
    :defer
    :config
    (setq calendar-latitude 40.641190
          calendar-longitude -8.653620))

  (use-package lunar
    :ensure nil
    :defer
    :config
    (setq lunar-phase-names
          '("New Moon"
            "First Quarter Moon"
            "Full Moon"
            "Last Quarter Moon")))

  :hook (calendar-today-visible-hook . calendar-mark-today))

(use-package which-func
  :defer 5
  :config (which-function-mode 1))

(use-package server
  :ensure nil
  :hook (after-init-hook . server-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init-hook . global-auto-revert-mode))

;; Remember location in file
(use-package saveplace
  :ensure nil
  :hook (after-init-hook . save-place-mode))

(use-package projectile
  :diminish
  :config
  (projectile-mode +1))

(use-package gruvbox-theme
  :defer 0
  :config
  (load-theme 'gruvbox-dark-hard)
  (set-face-attribute 'mode-line nil :background "#1d2021" :foreground "#fbf1c7" :box "#fe8019")
  )

(use-package so-long
  :config
  (global-so-long-mode))

(use-package cc-mode
  :config
  (setq-default c-default-style "bsd")
  (setq-default c-basic-offset 4)

  ; automatically indent when press RET
  (global-set-key (kbd "RET") 'newline-and-indent)

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
		  (if (string= (file-name-extension (buffer-name)) "h")
		      (message "Clang-format won’t work with this file format.")
		    (when (locate-dominating-file "." ".clang-format")
		      (clang-format-buffer))
		    )
		  ;; Return nil, to continue saving.
		  nil))))
  )

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-separator "/")
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (use-package cmake-mode)

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
    (magit-commit))
  )

(use-package git-commit
  :after magit
  :ensure nil
  :defer
  :config
  (setq git-commit-summary-max-length 100)
  (setq git-commit-known-pseudo-headers
        '("Signed-off-by"
          "Acked-by"
          "Modified-by"
          "Cc"
          "Suggested-by"
          "Reported-by"
          "Tested-by"
          "Reviewed-by"))
  (setq git-commit-style-convention-checks
        '(non-empty-second-line
          overlong-summary-line)))

(use-package magit-repos
  :after magit
  :ensure nil
  :defer
  :commands magit-list-repositories
  :config
  (setq magit-repository-directories
        '(("~/Documents/Projects/" . 1))))

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

(use-package px)

(use-package crux)

(use-package clean-aindent-mode
  :init
  (add-hook 'prog-mode-hook 'clean-aindent-mode))

(use-package org
  :commands org-indent-mode
  :config
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
    (org-indent-mode 1))

  (defun my-chromium (ppl)
    (start-process "fox" nil "open" "-a"
                   "chromium" (format "file://%s" my-org-publish-dir)))

  (defun my-git-publish (ppl)
    (let ((publish-script (format "%s/publish.sh" my-org-publish-dir)))
      (when (file-executable-p publish-script)
	(start-process-shell-command "pub" nil publish-script))))

  (defun my-publish (a b c)
    (setq org-export-with-toc t)
    (org-html-publish-to-html a b c)
    (setq org-export-with-toc nil)
    (org-ascii-publish-to-ascii a b c)
    (org-gfm-publish-to-gfm a b c))

  (defun rg/date-sha256 ()
    (secure-hash 'sha256 (format-time-string "%Y-%m-%d %a %H:%M"))
    )

  (defun rg/get-journal-file-month ()
    (let ((monthly-name (format-time-string "%Y%m")))
      (expand-file-name (concat my-org-journal-dir monthly-name ".org")))
    )

  (defun rg/get-journal-file-year ()
    (let ((yearly-name (format-time-string "%Y")))
      (expand-file-name (concat my-org-journal-dir yearly-name ".org")))
    )

  (defun my-org-inline-css-hook (exporter)
    "Insert custom inline css"
    (when (eq exporter 'html)
      (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
	     (path (concat dir "style.css"))
	     (homestyle (or (null dir) (null (file-exists-p path))))
	     (final (if homestyle "~/.config/emacs/org-style.css" path)))
	(setq org-html-head-include-default-style nil)
	(setq org-html-head (concat
			     "<style type=\"text/css\">\n"
			     "<!--/*--><![CDATA[/*><!--*/\n"
			     (with-temp-buffer
                               (insert-file-contents final)
                               (buffer-string))
			     "/*]]>*/-->\n"
			     "</style>\n")))))

  (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

  (defvar root-dir "/home/romeu/Documents/Org/")
  (defvar my-org-dir root-dir)
  (defvar my-org-publish-dir (concat root-dir "Publish/"))
  (defvar my-org-meta-dir (concat root-dir "Meta/"))
  (defvar my-org-archive-dir (concat my-org-meta-dir "Archive/"))
  (defvar my-org-journal-dir (concat root-dir "Journal/"))
  (defvar my-org-diary-file (concat root-dir "Diary/Diary.org"))

  ;; directories
  (setq org-directory my-org-dir)
  (setq org-metadir my-org-meta-dir)
  (setq org-archive-location my-org-archive-dir)
  (setq org-agenda-files (list
			  "/home/romeu/Documents/Org/Agenda/todo.org"
			  "/home/romeu/Documents/Org/Agenda/work.org"
			  "/home/romeu/Documents/Org/Agenda/birthdays.org"
                          ))
  (setq diary-file my-org-diary-file)

  ;; general configs
  (setq org-image-actual-width nil)
  (setq org-startup-indented t)
  (setq org-babel-min-lines-for-block-output 1)
  (setq org-startup-folded "showeverything")
  (setq org-startup-with-inline-images t)
  (setq org-use-speed-commands t)
  (setq org-confirm-babel-evaluate nil)
  (setq org-reverse-note-order nil)
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-hide-emphasis-markers t)
  (setq org-catch-invisible-edits 'show)
  (setq org-return-follows-link nil)
  (setq org-loop-over-headlines-in-active-region 'start-level)
  (setq org-imenu-depth 7)

  ;; selection
  (setq org-fast-tag-selection-single-key t)
  (setq org-use-fast-todo-selection t)
  (setq org-support-shift-select 'always)

  ;; refile
  (setq org-refile-targets
        '((org-agenda-files . (:maxlevel . 2))
          (nil . (:maxlevel . 2))))
  (setq org-refile-use-outline-path t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache t)

  ;; todo
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n!)" "INPROGRESS(p!)" "WAITING(w!)" "|" "DONE(D)" "CANCELED(C)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")
          (sequence "MEET(m)" "|" "MET(M)")
          (sequence "STUDY(s)" "|" "STUDIED(S)")))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "PROC" ((org-agenda-overriding-header "Process Tasks")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
          ;; (todo "TODO"
          ;;   ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
          ;;    (org-agenda-files `(,dw/org-inbox-path))
          ;;    (org-agenda-text-search-extra-files nil)))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("p" "Active Projects"
           ((agenda "")
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-max-todos 5)
                   (org-agenda-files org-agenda-files)))))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))

          ;; Projects on hold
          ("h" tags-todo "+LEVEL=2/+HOLD"
           ((org-agenda-overriding-header "On-hold Projects")
            (org-agenda-files org-agenda-files)))

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))))

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

  ;; Configure common tags
  (setq org-tag-alist
        '((:startgroup)
          ;; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)
          ("thinking" . ?t)
          ("recurring" . ?r)))

  (setq org-ellipsis " ▼ ")
  (setq org-hide-leading-stars t)

  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)

  (setq org-descriptive-links nil)

  (setq org-fontify-done-headline t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-fontify-whole-block-delimiter-line t)

  (setq org-track-ordered-property-with-tag t)
  (setq org-highest-priority ?A)
  (setq org-lowest-priority ?C)
  (setq org-default-priority ?A)

  (setq org-tags-column -110)
  (setq org-habit-graph-column 100)

  (setq org-babel-default-header-args (cons '(:noweb . "yes") (assq-delete-all :noweb org-babel-default-header-args)))
  (setq org-babel-default-header-args (cons '(:exports . "both") (assq-delete-all :exports org-babel-default-header-args)))
  (setq org-babel-default-header-args (cons '(:results . "output verbatim replace") (assq-delete-all :results org-babel-default-header-args)))

  ;; log
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-note-clock-out nil)
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)
  (setq org-read-date-prefer-future 'time)

  (custom-set-faces '(org-ellipsis ((t (:foreground "gray40" :underline nil)))))
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)

  ;; Org Capture
  ;; TODO: improve and add more templates
  (setq org-capture-templates
        '(
	  ("c" "Code")

	  ("cc" "Cpp")
	  ("ccs" "Cpp Snippets" entry (file+olp "Code/Cpp.org" "Cpp" "Snippets")
	   "* %? %t" :empty-lines 1)

	  ("ck" "Kotlin")
	  ("cks" "Kotlin Snippets" entry (file+olp "Code/Kotlin.org" "Kotlin" "Snippets")
	   "* %? %t" :empty-lines 1)
	  ("ckl" "Kotlin Libs")
	  ("ckln" "Kotlin Native" entry (file+headline "Code/Kotlin.org" "Kotlin Native")
           "* %? %t" :empty-lines 1)

	  ("cr" "Rust")
	  ("crs" "Rust Snippets" entry (file+olp "Code/Rust.org" "Rust" "Snippets")
	   "* %? %t" :empty-lines 1)

          ("j" "Personal Journal" entry (file+datetree rg/get-journal-file-year)
           "* Entry %(rg/date-sha256) %T %^G\n\n%?\n" :kill-buffer t :empty-lines 1)

          ("t" "Todo" entry (file "TODO.org")
           "* TODO %?\n%U" :empty-lines 1)

          ("s" "Songs" plain (file+headline "Songs.org" "Songs")
           "%^{Song: } %?\n")
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
     (latex . t)
     (perl . t)
     (emacs-lisp . t)
     (dot . t)))

  (setq org-latex-listings 'minted
	org-latex-packages-alist '(("newfloat" "minted"))
	org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


  ;; org crypt
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-tag-matcher "encrypt")
  (setq org-crypt-key "09852491") ;; TODO: make it a constant
  (add-to-list 'org-tags-exclude-from-inheritance (quote "encrypt"))
  (add-to-list 'org-tags-exclude-from-inheritance (quote "crypt"))

  :bind (("M-p" . #'org-publish))
  :hook
  (after-save-hook . my-after-save-hook)
  (org-mode-hook . my-org-mode-hook)
  )

;; "stolen" from Protesilaos Stavrou Config
(use-package org-agenda
  :after org
  :ensure nil
  :defer
  :config
  ;; Basic setup
  (setq org-agenda-span 14)
  (setq org-agenda-start-on-weekday 1)  ; Monday
  (setq org-agenda-confirm-kill t)
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-show-outline-path nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-skip-comment-trees t)
  (setq org-agenda-menu-show-matcher t)
  (setq org-agenda-menu-two-columns nil)
  (setq org-agenda-sticky nil)
  (setq org-agenda-custom-commands-contexts nil)
  (setq org-agenda-max-entries nil)
  (setq org-agenda-max-todos nil)
  (setq org-agenda-max-tags nil)
  (setq org-agenda-max-effort nil)

  ;; General view options
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo . " %i %-12:c")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-breadcrumbs-separator "->")
  (setq org-agenda-todo-keyword-format "%-1s")
  (setq org-agenda-diary-sexp-prefix nil)
  (setq org-agenda-fontify-priorities 'cookies)
  (setq org-agenda-category-icon-alist nil)
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-remove-timeranges-from-blocks nil)
  (setq org-agenda-compact-blocks nil)
  (setq org-agenda-block-separator ?—)

  (defun prot/org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading.

Slightly tweaked version of `org-agenda-format-date-aligned' that
produces dates with a fixed length."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date t))
	   (day (cadr date))
	   (day-of-week (calendar-day-of-week date))
	   (month (car date))
	   (monthname (calendar-month-name month t))
	   (year (nth 2 date))
	   (iso-week (org-days-to-iso-week
		      (calendar-absolute-from-gregorian date)))
	   (weekyear (cond ((and (= month 1) (>= iso-week 52))
			    (1- year))
			   ((and (= month 12) (<= iso-week 1))
			    (1+ year))
			   (t year)))
	   (weekstring (if (= day-of-week 1)
			   (format " (W%02d)" iso-week)
			 "")))
      (format "%s %2d %s %4d%s"
	      dayname day monthname year weekstring)))

  (setq org-agenda-format-date #'prot/org-agenda-format-date-aligned)

  ;; Marks
  (setq org-agenda-bulk-mark-char "#")
  (setq org-agenda-persistent-marks nil)

  ;; Diary entries
  (setq org-agenda-insert-diary-strategy 'date-tree)
  (setq org-agenda-insert-diary-extract-time nil)
  (setq org-agenda-include-diary t)

  ;; Follow mode
  (setq org-agenda-start-with-follow-mode nil)
  (setq org-agenda-follow-indirect t)

  ;; Multi-item tasks
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-agenda-todo-list-sublevels t)

  ;; Filters and restricted views
  (setq org-agenda-persistent-filter nil)
  (setq org-agenda-restriction-lock-highlight-subtree t)

  ;; Items with deadline and scheduled timestamps
  (setq org-agenda-include-deadlines t)
  (setq org-deadline-warning-days 5)
  (setq org-agenda-skip-scheduled-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-timestamp-if-deadline-is-shown t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (setq org-agenda-skip-scheduled-delay-if-deadline nil)
  (setq org-agenda-skip-additional-timestamps-same-entry nil)
  (setq org-agenda-skip-timestamp-if-done nil)
  (setq org-agenda-search-headline-for-time t)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-move-date-from-past-immediately-to-today t)
  (setq org-agenda-show-future-repeats t)
  (setq org-agenda-prefer-last-repeat nil)
  (setq org-agenda-timerange-leaders
        '("" "(%d/%d): "))
  (setq org-agenda-scheduled-leaders
        '("Scheduled: " "Sched.%2dx: "))
  (setq org-agenda-inactive-leader "[")
  (setq org-agenda-deadline-leaders
        '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
  ;; Time grid
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-timegrid-use-ampm nil)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-show-current-time-in-grid t)
  (setq org-agenda-current-time-string
        "—·—·—·—·—·—·—·—·—")
  (setq org-agenda-time-grid
        '((daily today require-timed)
          (0700 0800 0900 1000 1100
                1200 1300 1400 1500 1600
                1700 1800 1900 2000 2100)
          " -----" "—————————————————"))
  (setq org-agenda-default-appointment-duration nil)

  ;; Global to-do list
  (setq org-agenda-todo-ignore-with-date t)
  (setq org-agenda-todo-ignore-timestamp t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-time-comparison-use-seconds t)
  (setq org-agenda-tags-todo-honor-ignore-options nil)

  ;; Tagged items
  (setq org-agenda-show-inherited-tags t)
  (setq org-agenda-use-tag-inheritance
        '(todo search agenda))
  (setq org-agenda-hide-tags-regexp nil)
  (setq org-agenda-remove-tags nil)
  (setq org-agenda-tags-column -120)

  ;; Agenda entry
  ;;
  ;; NOTE I do not use this right now.  Leaving everything to its
  ;; default value.
  (setq org-agenda-start-with-entry-text-mode nil)
  (setq org-agenda-entry-text-maxlines 5)
  (setq org-agenda-entry-text-exclude-regexps nil)
  (setq org-agenda-entry-text-leaders "    > ")

  ;; Logging, clocking
  ;;
  ;; NOTE I do not use these yet, though I plan to.  Leaving everything to
  ;; its default value for the time being.
  (setq org-agenda-log-mode-items '(closed clock))
  (setq org-agenda-clock-consistency-checks
        '((:max-duration "10:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
                         ("4:00")
                         :default-face ; This should definitely be reviewed
                         ((:background "DarkRed")
                          (:foreground "white"))
                         :overlap-face nil :gap-face nil :no-end-time-face nil
                         :long-face nil :short-face nil)))
  (setq org-agenda-log-mode-add-notes t)
  (setq org-agenda-start-with-log-mode nil)
  (setq org-agenda-start-with-clockreport-mode nil)

  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2))
  (setq org-agenda-search-view-always-boolean nil)
  (setq org-agenda-search-view-force-full-words nil)
  (setq org-agenda-search-view-max-outline-level 0)
  (setq org-agenda-search-headline-for-time t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-cmp-user-defined nil)
  (setq org-sort-agenda-notime-is-late t)
  (setq org-sort-agenda-noeffort-is-high t)

  ;; Agenda column view
  ;;
  ;; NOTE I do not use these, but may need them in the future.
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-columns-show-summaries t)
  (setq org-agenda-columns-compute-summary-properties t)
  (setq org-agenda-columns-add-appointments-to-effort-sum nil)
  (setq org-agenda-auto-exclude-function nil)
  (setq org-agenda-bulk-custom-functions nil)

  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-'" . nil)
         ("C-," . nil)))

(use-package org-src
  :after org
  :ensure nil
  :config
  (setq org-src-window-setup 'current-window)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))

(use-package ol
  :defer
  :ensure nil
  :config
  (setq org-link-keep-stored-after-insertion t)
  )

(use-package org-id
  :after org
  :ensure nil
  :commands (contrib/org-get-id
             contrib/org-id-headlines)
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (defun contrib/org-get-id (&optional pom create prefix)
    "Get the CUSTOM_ID property of the entry at point-or-marker
POM. If POM is nil, refer to the entry at point. If the entry
does not have an CUSTOM_ID, the function returns nil. However,
when CREATE is non nil, create a CUSTOM_ID if none is present
already. PREFIX will be passed through to `org-id-new'. In any
case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          (setq id (org-id-new (concat prefix "h")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

  (defun contrib/org-id-headlines ()
    "Add CUSTOM_ID properties to all headlines in the current
file which do not already have one."
    (interactive)
    (org-map-entries (lambda ()
                       (contrib/org-get-id (point) 'create)))))

(use-package ox
  :after org
  :ensure nil
  :config
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-date nil)
  (setq org-export-time-stamp-file nil)
  (setq org-export-with-email t)
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-export-backends
        '(ascii html latex md))
  (setq org-export-dispatch-use-expert-ui nil)

  (setq org-html-validation-link nil)
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

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(use-package counsel)

(use-package ivy
  :defer 0
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")

  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)

  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x b") 'counsel-ibuffer)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-M-l") 'counsel-imenu)

  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)

  (global-set-key (kbd "C-c F") 'counsel-org-file)
  (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-c L") 'counsel-git-log)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c m") 'counsel-linux-app)
  (global-set-key (kbd "C-c n") 'counsel-fzf)
  (global-set-key (kbd "C-c J") 'counsel-file-jump)
  (global-set-key (kbd "C-c t") 'counsel-load-theme)

  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )

(use-package company
  :diminish company-mode
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  :init (global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  :config
  (defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
	(company-complete-common)
      (indent-according-to-mode)))

  (delete 'company-dabbrev company-backends)
  (delete 'company-oddmuse company-backends)
  (delete 'company-clang company-backends)
  (delete 'company-bbdb company-backends)
  (delete 'company-files company-backends)
  (delete '(company-dabbrev-code company-gtags company-etags company-keywords) company-backends)

  (setq company-require-match nil)
  (setq company-tooltip-idle-delay .25)

  (global-unset-key (kbd "C-SPC"))
  (global-set-key (kbd "C-SPC") 'company-complete)
  (global-set-key (kbd "<f9>") 'company-complete)

  ;; deactivate auto complete selection
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  )

(use-package eldoc
  :ensure nil
  :config (global-eldoc-mode -1))

(use-package csv-mode)

(use-package avy
  :bind
  ("M-s" . avy-goto-char)
  )

(use-package rg
  :ensure
  :defer
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")
  )

(use-package all-the-icons
  :config
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)
  )

(use-package auctex
  :mode (".tex" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-engine 'xelatex)
  (setq TeX-PDF-mode t)

  (setq TeX-command-default "Latexmk")
  )

(use-package auctex-latexmk
  :defer 0
  :init
  (auctex-latexmk-setup)
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
	      (push
	       '("LaTeXmk" "latexmk -pdf --synctex=1 -shell-escape -interaction=nonstopmode -file-line-error -synctex=1 %s" TeX-run-TeX nil t
		 :help "Run latexmk on file")
	       TeX-command-list)
              (push '("LuaLatex" "lualatex -pdf --synctex=1 -shell-escape -interaction=nonstopmode -file-line-error -synctex=1 %s" TeX-run-TeX nil t
		      :help "Run lualatex on file")
		    TeX-command-list)
              ))
  )

(put 'upcase-region 'disabled nil)
(load custom-file)
