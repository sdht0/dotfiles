;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(global-display-line-numbers-mode)          ;; Always display line and column numbers
(column-number-mode t)
(scroll-bar-mode -1)
(setq fill-column 100)                      ;; Lines should be 100 characters wide, not 72
(fset 'yes-or-no-p 'y-or-n-p)               ;; y/n instead of yes/no
(show-paren-mode 1)
;; (setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

(customize-set-variable 'tool-bar-mode nil) ;; Remove the tool bar
(add-to-list 'default-frame-alist '(fullscreen . maximized))    ;; always maximize the frame

;; (ido-mode 1)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)

(setq tramp-default-method "ssh")

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "s-[") (lambda ()(interactive)(split-window-below)(windmove-down)))
(global-set-key (kbd "s-]") (lambda ()(interactive)(split-window-right)(windmove-right)))
(global-set-key (kbd "s-'") 'delete-other-windows)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)
(global-set-key (kbd "s-J") (lambda ()(interactive)(shrink-window 5)))
(global-set-key (kbd "s-H") (lambda ()(interactive)(enlarge-window 5)))
(global-set-key (kbd "s-L") (lambda ()(interactive)(shrink-window-horizontally 5)))
(global-set-key (kbd "s-K") (lambda ()(interactive)(enlarge-window-horizontally 5)))

;; do not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; ************* PACKAGES ************* ;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(require 'use-package)

;; (use-package neotree
;;     :ensure t
;;     :init
;;         (require 'neotree)
;;     :config
;;     (global-set-key [f8] 'neotree-toggle))

(use-package dracula-theme
  :ensure t
  :init
  (load-theme 'dracula t))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-cursor-color "white"))))

(use-package powerline-evil
  :ensure t
  :config
  (powerline-evil-center-color-theme))

;; (use-package tango-plus-theme
;;     :ensure t
;;     :init
;;         (add-to-list 'default-frame-alist '(background-color . "#fcfcfc"))
;;         (load-theme 'tango-plus t))

(use-package projectile
  :ensure t
  :after ivy
  :init
  (projectile-mode 1)
  :config
  (define-key projectile-mode-map (kbd "s-/") 'projectile-command-map)
  (setq projectile-completion-system 'ivy))

(use-package ivy
    :ensure t
    :init
        (ivy-mode 1)
    :config
        (use-package swiper
          :ensure t)
        (use-package counsel
          :ensure t
          :init
          (counsel-mode 1))
        (setq ivy-use-virtual-buffers t
                ivy-count-format "%d/%d ")
        (global-set-key "\C-s" 'swiper)
        (global-set-key (kbd "C-c C-r") 'ivy-resume)
        (global-set-key (kbd "<f6>") 'ivy-resume)
        (global-set-key (kbd "C-c g") 'counsel-git)
        (global-set-key (kbd "C-c j") 'counsel-git-grep)
        (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(use-package evil
    :ensure t
    :init
        (setq evil-search-module 'evil-search)
        (setq evil-ex-complete-emacs-commands nil)
        (setq evil-vsplit-window-right t)
        (setq evil-split-window-below t)
        (setq evil-shift-round nil)
        (setq evil-want-C-u-scroll t)
        (setq evil-default-state 'emacs)
    :config
        (evil-mode))

;; (use-package fill-column-indicator
;;   :ensure t
;;   :config
;;   (add-hook 'after-change-major-mode-hook 'fci-mode)
;;   (setq fci-rule-column 100))

(use-package smartparens-config
  :ensure smartparens
  :init
  (smartparens-global-mode 1))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (yas-reload-all)
  (use-package yasnippet-snippets         ; Collection of snippets
    :ensure t))

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

(use-package ibuffer
    :ensure t
    :bind ("C-x C-b" . ibuffer))

;; (use-package hydra
;;   :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package company
    :ensure t
    :config
        (global-company-mode t)
        (setq company-tooltip-align-annotations t)
        (setq company-tooltip-limit 20)
        (setq company-idle-delay .1)
        (setq company-echo-delay 0)
        (setq company-minimum-prefix-length 1))

(use-package lsp-mode
    :ensure t
    :config
        (use-package lsp-ui
            :ensure t
            :config
            (setq lsp-ui-sideline-ignore-duplicate t)
            (add-hook 'lsp-mode-hook 'lsp-ui-mode))
        (use-package company-lsp
          :ensure t
          :config
          (push 'company-lsp company-backends)))

(use-package rust-mode
  :ensure t
  :init
  (require 'rust-mode)
  :config
  (use-package lsp-rust
    :ensure t
    :after lsp-mode
    :config
    (add-hook 'rust-mode-hook 'lsp-mode)
    (add-hook 'rust-mode-hook #'lsp-rust-enable)
    (add-hook 'rust-mode-hook #'flycheck-mode))
  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode)))
;;         (use-package racer
;;             :ensure t
;;             :config
;;                 (add-hook 'rust-mode-hook #'racer-mode)
;;                 (add-hook 'racer-mode-hook #'eldoc-mode)
;;                 (add-hook 'racer-mode-hook #'company-mode)
;;                 (local-set-key (kbd "TAB") #'company-indent-or-complete-common))
;;         (use-package company-racer
;;             :ensure t)
;;         (use-package flycheck-rust
;;             :ensure t
;;             :config
;;                 (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package tex-site
  :ensure auctex
  :after (tex latex)
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (turn-on-reftex)
              (setq reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)))

  ;; Indentation
  (setq LaTeX-indent-level 4
        LaTeX-item-indent 0
        TeX-brace-indent-level 4
        TeX-newline-function 'newline-and-indent)

  ;; Some usefull hooks
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; to use pdfview with auctex
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)

  ;; to use pdfview with auctex
  (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
  (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (setq TeX-source-correlate-mode t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

(use-package bibtex
  :ensure t
  :defer t)

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t))

(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :init
  (global-display-line-numbers-mode 0)
  :config
  (setq mouse-wheel-follow-mouse t)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-resize-factor 1.10))

(use-package company-auctex
  :ensure t
  :defer t
  :hook
  (latex-mode . (company-auctex-init)))


(use-package company-bibtex
  :ensure t
  :defer t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-bibtex))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-bibtex)))))

(use-package company-reftex
  :ensure t
  :defer t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-reftex-labels company-reftex-citations))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-reftex-labels company-reftex-citations)))))

(use-package company-math
  :ensure t
  :defer t
  :hook
  (latex-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-math-symbols-unicode))))
  (org-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-math-symbols-unicode)))))

;; (setq TeX-save-query nil)
;; ;; Okular
;; (setq TeX-view-program-list '(("Okular" "okular --unique %o#src:%n%b")))
;; (add-hook 'LaTeX-mode-hook '(lambda ()
;;                   (add-to-list 'TeX-expand-list
;;                        '("%u" Okular-make-url))))
;; (defun Okular-make-url () (concat
;;                "file://"
;;                (expand-file-name (funcall file (TeX-output-extension) t)
;;                          (file-name-directory (TeX-master-file)))
;;                "#src:"
;;                (TeX-current-line)
;;                (expand-file-name (TeX-master-directory))
;;                "./"
;;                (TeX-current-file-name-master-relative)))
;; (setq TeX-view-program-selection '((output-pdf "Okular")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-max-width 100)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-sideline-enable nil)
 '(package-selected-packages
   (quote
    (yasnippet-snippets smartparens smartparens-config powerline-evil counsel use-package swiper projectile powerline pdf-tools magit lsp-ui lsp-rust evil dracula-theme company-reftex company-math company-lsp company-bibtex company-auctex cargo)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-symbol ((t (:foreground "white" :box (:line-width -1 :color "dim gray") :height 0.99)))))
