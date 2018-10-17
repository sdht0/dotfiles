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
(setq fill-column 80)                       ;; Lines should be 80 characters wide, not 72
(fset 'yes-or-no-p 'y-or-n-p)               ;; y/n instead of yes/no
(show-paren-mode 1)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

(customize-set-variable 'tool-bar-mode nil) ;; Remove the tool bar
(add-to-list 'default-frame-alist '(fullscreen . maximized))    ;; always maximize the frame

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(setq tramp-default-method "ssh")

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; do not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; ************* PACKAGES ************* ;;

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
    (require 'use-package))

(use-package neotree
    :ensure t
    :init
        (require 'neotree)
    :config
        (global-set-key [f8] 'neotree-toggle))

(use-package tango-plus-theme
    :ensure t
    :init
        (add-to-list 'default-frame-alist '(background-color . "#fcfcfc"))
        (load-theme 'tango-plus t))

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

(use-package ibuffer
    :ensure t
    :bind ("C-x C-b" . ibuffer))

(use-package magit
    :ensure t)

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

;; ;; latex support
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
;; (require 'reftex)
;; (setq TeX-auto-save t)
;; (setq TeX-save-query nil)
;; (setq TeX-parse-self t)
;; (setq TeX-PDF-mode t)
;; (setq TeX-source-correlate-mode t)
;; (setq-default TeX-master nil)
;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
;; (setq reftex-plug-into-AUCTeX t)
;; (latex-preview-pane-enable)
;; (require 'company-auctex)
;; (company-auctex-init)
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

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

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
    (tango-plus use-package racer magit flycheck-rust company-racer cargo)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background "gold"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "white"))))
 '(lsp-ui-sideline-global ((t (:background "gold"))))
 '(lsp-ui-sideline-symbol ((t (:foreground "white" :box (:line-width -1 :color "dim gray") :height 0.99)))))
