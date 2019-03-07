;;; .emacs --- .emacs of goldolphin

;;; Commentary:
;; .emacs of goldolphin.

;;; Code:

;;;; Utilities

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun read-from-file (filePath)
  "Return filePath's content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun g/safe-eval (form)
  (condition-case err (eval form)
    (error (warn "%s" (error-message-string err) nil))))

(defun g/require (feature &optional initializer)
  "Flexible require."
  (if (g/safe-eval '(require feature))
      (if initializer
	  (g/safe-eval initializer))))

(defun g/install (package)
  "Flexible install."
  (if (featurep package)
      (progn (message "Ignore installed package: %s." package) nil)
    (progn
      (message "Install package: %s." package)
      (package-install package)
      t)))

(defun g/install-batch (packages)
  "Install packages in a batch."
  (let ((installed (length (remove-if 'null (mapcar 'g/install packages))))
	(all (length packages)))
    (message "%d/%d packages are installed" installed all)))

(defun g/create-parents ()
  "Create parent directories"
  (unless (file-exists-p buffer-file-name)
    (let ((dir (file-name-directory buffer-file-name)))
      ;; (message "%s" dir)
      (if (and (not (file-exists-p dir))
  	       (yes-or-no-p (concat "Do you want to create directory: " dir)))
	  (make-directory dir t)))))

;;;; Company to Auto-Complete
;; (defmacro ac-company-define-source (name backend &rest overrides)
;;   "Define auto-complete source NAME from company BACKEND.
;; When OVERRIDES is specified, OVERRIDES is prepend to original source."
;;   `(defvar ,name
;;      '(,@overrides
;;        (candidates . (ac-company-candidates ',backend))
;;        (prefix . (ac-company-prefix ',backend))
;;        (document . (lambda (item) (ac-company-document ',backend item))))))
  
;; (defun ac-company-prefix (backend)
;;   (require backend nil t)
;;   (when (fboundp backend)
;;     (let ((prefix (funcall backend 'prefix)))
;;       (when (stringp prefix) 
;; 	(- (point) (length prefix))))))

;; (defun ac-company-candidates (backend)
;;   (funcall backend 'candidates ac-prefix))

;; (defun ac-company-meta-as-document (backend item)
;;   (funcall backend 'meta item))

;; (defun ac-company-doc-buffer-as-document (backend item)
;;   (with-current-buffer (funcall backend 'doc-buffer item)
;;     (buffer-string)))

;; (defun ac-company-document (backend item)
;;   (or (ac-company-doc-buffer-as-document backend item)
;;       (ac-company-meta-as-document backend item)))

;;;; Packages

(defvar g/packages '(
multiple-cursors
auto-complete
racket-mode
;; geiser
;; ac-geiser
org
;; puml-mode
company-racer
;; mediawiki
;; helm
markdown-mode
session
magit
smex
flycheck
;; ac-js2
;; skewer-mode
))

;; (progn (package-refresh-contents) (g/install-batch g/packages))

;;;; Configurations
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)
(auto-image-file-mode)
(setq user-full-name "goldolphin")
(setq user-mail-address "goldolphin@gmail.com")
;; (setq major-mode 'text-mode)
(setq frame-title-format "%f -- %F")
(setq mouse-yank-at-point t)
(auto-compression-mode 1) 
(setq-default make-backup-files nil)
(add-to-list 'load-path "~/.emacs-lisp")
(setq initial-scratch-message "")
(display-time-mode t)
(setq vc-follow-symlinks t)
(setq indent-tabs-mode nil)

;; set font
;(set-frame-font "-unknown-PragmataPro-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1")
(if (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset (font-spec :family "Microsoft Yahei" :size 16))))

;; set cursor
(if (display-graphic-p)
    (progn
      (setq-default cursor-type 'bar)
      ;; hl-line-mode
      (global-hl-line-mode)
      (set-face-background hl-line-face "#ffffd0")))

;; set encoding
(prefer-coding-system 'utf-8)

;; set browser
(setq browse-url-browser-function
  (if (or (string= system-type "cygwin") (string= system-type "windows-nt"))
    (quote browse-url-default-windows-browser)
    (quote browse-url-default-macosx-browser)
  )
)

;; create parent directory automatically
(add-hook 'before-save-hook 'g/create-parents)

;; global keys
(global-set-key (kbd "C-c C-c") (quote comment-region))
(global-set-key (kbd "C-x C-r") (quote revert-buffer))

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

;; set ibuffer
;; (require 'ibuffer)
;; (global-set-key (kbd "C-x C-b") 'ibuffer)

;; Initialization for plugins from package system.
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;; linum
  (global-linum-mode)

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
		(ac-config-default)
		(add-hook 'lisp-interaction-mode-hook 'ac-emacs-lisp-mode-setup)))

  ;; racket-mode
  (g/require 'racket-mode
  	     '(progn
		(defun ac-source-racket-mode-candidates ()
		  "Return a possibly-empty list of completions for the symbol at point."
		  (racket--complete-prefix ac-prefix))

		(defun ac-racket-mode-documentation (symbol)
		  (let ((file (racket--eval/sexpr (format ",describe %s" symbol))))
		    (read-from-file file)))
		
		(defvar ac-source-racket-mode
		  '((candidates . ac-source-racket-mode-candidates)
		    (symbol . "g")
;;		    (document . ac-racket-mode-documentation)
		    )
		  "Source for racket completion")

		(defun ac-racket-mode-setup ()
		  (add-to-list 'ac-sources 'ac-source-racket-mode)
		  (auto-complete-mode t))

  		(add-hook 'racket-mode-hook 'ac-racket-mode-setup)
		(add-hook 'racket-repl-mode-hook 'ac-racket-mode-setup)))

  ;; ;; haskell-mode
  ;; (g/require 'ghc
  ;; 	     '(progn
  ;; 		(autoload 'ghc-init "ghc" nil t)
  ;; 		(autoload 'ghc-debug "ghc" nil t)
  ;; 		(add-hook 'haskell-mode-hook (lambda () (ghc-init)))))

  ;; ;; sematic
  ;; (g/require 'semantic
  ;; 	     '(progn
  ;; 		(semantic-add-system-include "/usr/include/boost" 'c++-mode)))
  
  ;; ;; geiser
  ;; (g/require 'geiser
  ;; 	     '(eval-after-load 'geiser-mode
  ;; 		'(progn
  ;; 		   (define-key geiser-mode-map (kbd "C-.") nil))))
		
  ;; ;; ac geiser
  ;; (g/require 'ac-geiser
  ;; 	     '(progn
  ;; 		(add-hook 'geiser-mode-hook 'ac-geiser-setup)
  ;; 		(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
  ;; 		(eval-after-load "auto-complete"
  ;; 		  '(add-to-list 'ac-modes 'geiser-repl-mode))))

  ;; org mode
  (g/require 'org-table)
  (g/require 'org
  	     '(progn
		(setq org-export-allow-bind-keywords t)
		(add-to-list 'load-path "/usr/share/emacs/site-lisp/org/")
		(add-to-list 'org-export-backends 'md)
		(add-to-list 'org-export-backends 'org)
		(add-to-list 'org-src-lang-modes '("plantuml" . puml))
		(setq org-descriptive-links nil)
		(setq org-export-publishing-directory "../")
		(org-babel-do-load-languages 'org-babel-load-languages
					     '((plantuml . t)))
		(setq org-plantuml-jar-path "e:/Tools/plantuml.jar")
		(add-hook 'org-mode-hook (lambda ()
					   (setq truncate-lines nil)))
;;		(require 'ox-gfm)
  ;;		(setq org-md-src-style 'github-flavored)
		))
  ;; ;; mediawiki mode
  ;; (g/require 'mediawiki
  ;; 	     '(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode)))

  ;; markdown mode
  (g/require 'markdown-mode
	     '(progn
		(autoload 'markdown-mode "markdown-mode"
		  "Major mode for editing Markdown files" t)
		(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
		(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
		(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
		(add-hook 'markdown-mode-hook 'orgtbl-mode)))

  ;; puml-mode
  ;; (g/require 'puml-mode
  ;; 	     '(progn
  ;; 		))
  
  ;; smex
  (g/require 'smex
  	     '(progn
  		(global-set-key (kbd "M-x") 'smex)))

  ;; flycheck
  (g/require 'flycheck
	     '(global-flycheck-mode))

  ;; ;; js2
  ;; (g/require 'ac-js2
  ;; 	     '(progn
  ;; 		(add-hook 'js2-mode-hook 'ac-js2-mode)
  ;; 		(setq ac-js2-evaluate-calls t)))

  ;; ;; skewer mode
  ;; (g/require 'skewer-mode
  ;; 	     '(progn
  ;; 		(add-hook 'js2-mode-hook 'skewer-mode)
  ;; 		(add-hook 'css-mode-hook 'skewer-css-mode)
  ;; 		(add-hook 'html-mode-hook 'skewer-html-mode)))

  ;; Load session
  (g/require 'session '(session-initialize))
)

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
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(lazy-highlight-initial-delay 0)
 '(org-confirm-babel-evaluate nil)
 '(org-image-actual-width nil)
 '(org-support-shift-select t)
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (org plantuml-mode smex session racket-mode multiple-cursors markdown-mode magit flycheck company-racer auto-complete)))
 '(puml-plantuml-jar-path "e:/Tools/plantuml.jar")
 '(save-place t nil (saveplace))
 '(select-enable-clipboard t)
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "dark cyan")))))
