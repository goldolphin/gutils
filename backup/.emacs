(setq visible-bell t)
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
(add-hook 'before-save-hook (lambda ()
  (unless (file-exists-p buffer-file-name)
    (let ((dir (file-name-directory buffer-file-name)))
      (message "%s" dir)
      (if (and (not (file-exists-p dir))
  	       (yes-or-no-p (concat "Do you want to create directory: " dir)))
	  (make-directory dir))))))

;; global keys
(global-set-key "" (quote comment-region))

;; set windmove keys
(global-set-key [C-left] (quote windmove-left))
(global-set-key [C-right] (quote windmove-right))
(global-set-key [C-up] (quote windmove-up))
(global-set-key [C-down] (quote windmove-down))

;; ido
(require 'ido)
(ido-mode t)
(global-set-key (kbd "C-x C-r") (quote revert-buffer))

;; Initialization for plugins from package system.
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;; linum
  (global-linum-mode)
  (setq linum-format "%d ")

  ;; org mode
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/org/")
  (require 'org-publish)
  (add-to-list 'org-export-backends 'md)
  (add-to-list 'org-export-backends 'odt)
  (add-to-list 'org-export-backends 'org)
  (setq org-descriptive-links nil)
  (setq org-export-publishing-directory "../export")
  (require 'ox-gfm)
  (setq org-md-src-style 'github-flavored)
  (require 'ox-mediawiki)

  ;; multiple cursors
  (require 'multiple-cursors)
  (global-set-key (kbd "C-.") 'mc/mark-next-like-this)
  (global-set-key (kbd "C->") 'mc/unmark-next-like-this)
  (global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-<") 'mc/unmark-previous-like-this)
  (global-set-key (kbd "C-c C-.") 'mc/mark-all-like-this)
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

;; mediawiki mode
(require 'mediawiki)
(add-to-list 'auto-mode-alist '("\\.mw\\'" . mediawiki-mode))

;; markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; set ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

;; Load session
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)

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

(define-key global-map [(9)] 'c-indent-or-complete)

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
 '(LaTeX-mode-hook (quote (LaTeX-preview-setup preview-mode-setup turn-on-reftex my-tex-mode-hook)) t)
 '(TeX-PDF-mode t)
 '(TeX-command-list (quote (("TeX" "%(PDF)%(tex) %S%(PDFout) \"%(mode)\\input %t\"" TeX-run-TeX nil (plain-tex-mode texinfo-mode ams-tex-mode) :help "Run plain TeX") ("LaTeX" "%l \"%(mode)\\input{%t}\"" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX") ("Dvi To Pdf" "dvipdfmx %s.dvi" TeX-run-command nil t) ("View" "%V" TeX-run-discard t t :help "Run Viewer") ("View Pdf" "acroread %s.pdf" TeX-run-command nil t) ("Makeinfo" "makeinfo %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with Info output") ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil (texinfo-mode) :help "Run Makeinfo with HTML output") ("AmSTeX" "%(PDF)amstex %S%(PDFout) \"%(mode)\\input %t\"" TeX-run-TeX nil (ams-tex-mode) :help "Run AMSTeX") ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt once") ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil (context-mode) :help "Run ConTeXt until completion") ("ConTeXt Clean" "texutil --purgeall" TeX-run-interactive nil (context-mode) :help "Clean temporary ConTeXt files") ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX") ("Print" "%p" TeX-run-command t t :help "Print the file") ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command) ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file") ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file") ("Check" "lacheck %s" TeX-run-compile nil (latex-mode) :help "Check LaTeX file for correctness") ("Spell" "<ignored>" TeX-run-ispell-on-document nil t :help "Spell-check the document") ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-output-view-style (quote (("^dvi$" ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$") "%(o?)dvips -t landscape %d -o && gv %f") ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "." "acroread %o") ("^html?$" "." "netscape %o"))))
 '(TeX-style-path (quote ("style" "auto" "/usr/local/share/emacs/site-lisp/auctex/style" "/usr/local/var/auctex" "~/.emacs-lisp")))
 '(c-mode-common-hook (quote (my-c-mode-hook)))
 '(column-number-mode t)
 '(company-backends (quote (company-elisp company-nxml company-css company-eclim company-clang company-xcode company-files company-dabbrev)))
 '(display-time-mode t)
 '(ecb-options-version "2.32")
 '(global-font-lock-mode t nil (font-lock))
 '(inhibit-startup-screen t)
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
 )
