;;;; Utilities
(defun g/safe-eval (form)
  (condition-case err (eval form)
    (error (warn "%s" (error-message-string err) nil))))

(defun g/require (feature &optional initializer)
  "Flexible require."
  (if (g/safe-eval '(require feature))
      (if initializer
	  (g/safe-eval initializer))))

(defun g/create-parents ()
  "Create parent directories"
  (unless (file-exists-p buffer-file-name)
    (let ((dir (file-name-directory buffer-file-name)))
      (message "%s" dir)
      (if (and (not (file-exists-p dir))
  	       (yes-or-no-p (concat "Do you want to create directory: " dir)))
	  (make-directory dir)))))

;;;; Configurations
(setq visible-bell nil)
(fset 'yes-or-no-p 'y-or-n-p)
(auto-image-file-mode)
(setq user-full-name "goldolphin")
(setq user-mail-address "goldolphin@gmail.com")
(setq default-major-mode 'text-mode)
(setq show-paren-style 'parenthesis)
(setq frame-title-format "%f -- %F")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq mouse-yank-at-point t)
(auto-compression-mode 1) 
(setq-default make-backup-files nil)
(add-to-list 'load-path "~/.emacs-lisp")
(setq initial-scratch-message "")
;; (global-hl-line-mode 1)
(display-time-mode t)
(setq vc-follow-symlinks t)
(setq indent-tabs-mode nil)

;; set encoding
(prefer-coding-system 'utf-8)

;; set browser
(setq browse-url-browser-function
  (if (string= system-type "cygwin")
    (quote browse-url-default-windows-browser)
    (quote browse-url-default-macosx-browser)
  )
)

;; create parent directory automatically
(add-hook 'before-save-hook 'g/create-parents)

;; global keys
(global-set-key "" (quote comment-region))

;; set windmove keys
(global-set-key [C-left] (quote windmove-left))
(global-set-key [C-right] (quote windmove-right))
(global-set-key [C-up] (quote windmove-up))
(global-set-key [C-down] (quote windmove-down))

;; set mac osx keys
(global-set-key [s-left] (quote move-beginning-of-line))
(global-set-key [s-right] (quote move-end-of-line))
(global-set-key [s-up] (quote beginning-of-buffer))
(global-set-key [s-down] (quote end-of-buffer))

;; ido
(require 'ido)
(ido-mode t)
(global-set-key (kbd "C-x C-r") (quote revert-buffer))

;; set ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Initialization for plugins from package system.
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;; linum
  (global-linum-mode)
  (setq linum-format "%d ")

  ;; multiple cursors
  (g/require 'multiple-cursors
	     '(progn
		(define-key mc/keymap (kbd "<return>") nil)
		(define-key mc/keymap (kbd "M-ESC ESC") 'multiple-cursors-mode)
		(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
		(global-set-key (kbd "C->") 'mc/unmark-next-like-this)
		(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
		(global-set-key (kbd "C-<") 'mc/unmark-previous-like-this)
		(global-set-key (kbd "C-c C-.") 'mc/mark-all-like-this)))

  ;; auto complete
  (g/require 'auto-complete-config
	     '(progn
		(ac-config-default)))

  ;; ac geiser
  (g/require 'ac-geiser
	     '(progn
		(add-hook 'geiser-mode-hook 'ac-geiser-setup)
		(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
		(eval-after-load "auto-complete"
		  '(add-to-list 'ac-modes 'geiser-repl-mode))))

  ;; org mode
  (g/require 'org
	     '(progn
	       (add-to-list 'load-path "/usr/share/emacs/site-lisp/org/")
	       (add-to-list 'org-export-backends 'md)
	       (add-to-list 'org-export-backends 'org)
	       (setq org-descriptive-links nil)
	       (setq org-export-publishing-directory "../export")
	       (require 'ox-gfm)
	       (setq org-md-src-style 'github-flavored)
	       (require 'ox-mediawiki)))

  ;; mediawiki mode
  (g/require 'mediawiki
	     '(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode)))

  ;; markdown mode
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

  ;; Load session
  (g/require 'session '(session-initialize))
)

;; company
;; (add-to-list 'load-path "~/.emacs-lisp/company")
;; (autoload 'company-mode "company" nil t)

;; set hippie-expand
(setq hippie-expand-try-functions-list
        '(
	  ;;senator-try-expand-semantic
	  try-expand-dabbrev
	  try-expand-dabbrev-visible
	  try-expand-dabbrev-all-buffers
	  try-expand-dabbrev-from-kill
	  try-expand-list
	  try-expand-list-all-buffers
	  try-expand-line
        try-expand-line-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        )
)

;; set color theme
;; (add-to-list 'load-path "~/.emacs-lisp/color-theme-6.6.0")
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-word-perfect)

;; set vtl minor mode
;; (require 'vtl)
;; (autoload 'turn-on-vtl-mode "vtl" nil t)
;; (add-hook 'html-mode-hook 'turn-on-vtl-mode t t)
;; (add-hook 'xml-mode-hook 'turn-on-vtl-mode t t)
;; (add-hook 'text-mode-hook 'turn-on-vtl-mode t t)
;; (setq auto-mode-alist (cons '("\\.vm\\'" . html-mode) auto-mode-alist))

;; set gud
;; (setq gdb-many-windows t)
;; (setq gdb-use-inferior-io-buffer t) 

;;EmacsWiki
;;(add-to-list 'load-path "~/.emacs-lisp/emacs-wiki")
;; Load emacs-wiki
;;(require 'emacs-wiki)
;;Source tags
;;(require 'emacs-wiki-srctag)

;; ;;LaTeX

;; ;;(load "auctex.el" nil t t)
;; ;;(load "preview-latex.el" nil t t)
;; ;;(require 'tex-site)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq LaTeX-math-mode t)
;; ;;(load "preview-latex")

;; (defun tex-indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\>")
;;       (TeX-complete-symbol)
;;     (indent-for-tab-command))
;;   )

;; (defun my-tex-mode-hook ()
;;   (define-key TeX-mode-map [(tab)] 'tex-indent-or-complete)
;;   (outline-minor-mode)
;; )


;; Setup GBK environment
;;(set-terminal-coding-system 'chinese-gbk)
;;(set-keyboard-coding-system 'chinese-gbk)
;;(set-language-environment 'chinese-gbk)
;;(setq locale-coding-system 'chinese-gbk)
;;(set-selection-coding-system 'chinese-gbk)
;;(set-clipboard-coding-system 'chinese-gbk)

;; Load tabbar
;;(require 'tabbar)
;;(tabbar-mode t)

;; Load Global
;; (require 'xgtags)
;; (defun xgtags-generate-tags ()
;;   "Generate gtags reference 1file for global."
;;   (interactive)
;;   (cd (read-from-minibuffer "Directory: " default-directory))
;;   (shell-command "gtags --gtagslabel gtags")
;;   (xgtags-make-complete-list)
;; )

;; (defun xgtags-init ()
;;   (setq xgtags-mode 1)
;;   (define-key c-mode-base-map [(f3)]  'xgtags-find-rtag)
;;   (define-key c-mode-base-map [(f4)]  'xgtags-find-tag)
;;   (define-key c-mode-base-map [(f5)] 'xgtags-find-symbol)
;;   (define-key c-mode-base-map [(f6)]  'xgtags-find-file)
;;   (define-key c-mode-base-map [(f7)]  'xgtags-find-pattern)
;;   (define-key c-mode-base-map [(f9)] 'xgtags-parse-file)
;;   (define-key c-mode-base-map [(f11)] 'xgtags-generate-tags)
;; )

;; Load CScope
;; (require 'xcscope)
;; (defun cscope-init ()
;;   (cscope-minor-mode)
;;   (define-key global-map [(f3)]  'cscope-find-this-symbol)
;;   (define-key global-map [(f4)]  'cscope-find-global-definition)
;;   ;; (define-key global-map [(f5)] 'cscope-prev-symbol)
;;   ;; (define-key global-map [(f6)]  'cscope-next-symbol)
;;   ;; (define-key global-map [(f7)]  'cscope-pop-mark)
;;   ;; (define-key global-map [(f9)] 'cscope-display-buffer)
;; )

;; ;; Load CEDET
;; ;(setq semantic-load-turn-useful-things-on t)
;; (load-file "/usr/share/emacs/site-lisp/cedet-common/cedet.el")
;; ;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; ;; Select one of the following
;; ;(semantic-load-enable-code-helpers)
;; ;(setq semanticdb-project-roots (list (expand-file-name "/")))
;; (autoload 'senator-try-expand-semantic "senator")

;; ;; Xrefactory configuration part ;;
;; (defvar xref-key-binding 'none)
;; (defvar xref-current-project nil) ;; can be also "my_project_name"
;; (defvar xref-key-binding 'global) ;; can be also 'local or 'none
;; (setq load-path (cons "/home/caofx/xref/emacs" load-path))
;; (setq exec-path (cons "/home/caofx/xref" exec-path))
;; (load "xrefactory")
;; ;; end of Xrefactory configuration part ;;

;; Xrefactory key bindings
;; (define-key global-map [(f11)] 'xref-refactor)
;; (define-key global-map [(f8)] 'xref-completion)
;; (define-key global-map [27 (f8)] 'xref-ide-compile-run)
;; (define-key global-map [(f7)] 'xref-delete-window)
;; (define-key global-map [(f6)] 'xref-push-and-goto-definition)
;; (define-key global-map [27 (f6)] 'xref-browse-symbol)
;; (define-key global-map [(f5)] 'xref-pop-and-return)
;; (define-key global-map [27 (f5)] 'xref-re-push)
;; (define-key global-map [(f4)] 'xref-next-reference)
;; (define-key global-map [27 (f4)] 'xref-alternative-next-reference)
;; (define-key global-map [(f3)] 'xref-previous-reference)
;; (define-key global-map [27 (f3)] 'xref-alternative-previous-reference)

(defun insert-current-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))
)

(define-key global-map [(f12)] 'insert-current-time)


(defun c-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    ;; (c-indent-command)
      (indent-for-tab-command)
    )
)

;; (define-key global-map [(9)] 'c-indent-or-complete)

(defun my-c-mode-hook ()
;;  (semantic-default-c-setup)
  (c-set-style "stroustrup")
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
;;  (xgtags-init)
  (hs-minor-mode t)
;;  (font-lock-add-keywords 'c-mode '("\\<#if 0\\>.*\\<#endif\\>" . font-lock-comment-face))
  (setq auto-mode-alist (cons '("\\.h\\'" . c++-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.inl\\'" . c++-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.inl\\'" . c-mode) auto-mode-alist))
  (define-key c-mode-base-map [(9)] 'c-indent-or-complete)
)

;; ;; (semantic-load-enable-guady-code-helpers)
;; ;; (semantic-load-enable-excessive-code-helpers)
;; ;; Enable this if you develop in semantic, or develop grammars
;; ;; (semantic-load-enable-semantic-debugging-helpers)

;; ;; Edit the path in the following line to reflect the
;; ;; actual location of the MATLAB root directory on your system.
;; (add-to-list 'load-path "/usr/local/matlab7/java/extern/EmacsLink/lisp")
;; (autoload 'matlab-eei-connect "matlab-eei" 
;;   "Connects Emacs to MATLAB's external editor interface.")
;; (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
;; (setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
;; (autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
;; (setq matlab-indent-function t); if you want function bodies indented
;; (setq matlab-verify-on-save-flag nil); turn off auto-verify on save

;; (defun matlab-indent-or-complete ()
;; (interactive)
;; (if (looking-at "\\>")
;;   (matlab-complete-symbol)
;;   (indent-for-tab-command))
;;     )
    
;; (defun my-matlab-mode-hook ()
;;   (setq fill-column 76)
;;   (define-key matlab-mode-map [(tab)] 'matlab-indent-or-complete)
;;   (imenu-add-to-menubar "Find")); where auto-fill should wrap
  
;; (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)


;; ;; Uncomment the next two lines to enable use of the mlint package provided
;; ;; with EmacsLink.     
;; (setq matlab-show-mlint-warnings t)
;; (setq matlab-highlight-cross-function-variables t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu 0.2)
 '(ac-quick-help-delay 0.0)
 '(column-number-mode t)
 '(display-time-mode t)
 '(geiser-mode-smart-tab-p t)
 '(geiser-mode-start-repl-p t)
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
 '(lazy-highlight-initial-delay 0)
 '(package-archives (quote (("melpa" . "http://melpa.milkbox.net/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "dark cyan")))))
