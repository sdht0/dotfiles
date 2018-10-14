;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Always display line and column numbers
(global-display-line-numbers-mode)
(column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Remove the tool bar
(customize-set-variable 'tool-bar-mode nil)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(show-paren-mode 1)
(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(setq tramp-default-method "ssh")

;; always maximize the frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; do not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)


;; ************* PACKAGES ************* ;;

(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
    (require 'use-package))

(use-package tango-plus-theme
    :ensure t
    :init
        (add-to-list 'default-frame-alist '(background-color . "#ffffff"))
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

(use-package rust-mode
    :ensure t
    :init
        (require 'rust-mode)
    :config
        (use-package company-racer
                    :ensure t)
        (use-package flycheck-rust
                    :ensure t
                    :config
                        (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
        (use-package cargo
                    :ensure t)
        (use-package racer
                    :ensure t
                    :config
                        (add-hook 'racer-mode-hook #'eldoc-mode)
                        (add-hook 'racer-mode-hook #'company-mode)
                        (local-set-key (kbd "TAB") #'company-indent-or-complete-common))
        (add-hook 'rust-mode-hook #'racer-mode)
        (add-hook 'rust-mode-hook 'cargo-minor-mode))

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
 '(package-selected-packages
   (quote
    (tango-plus use-package racer magit flycheck-rust company-racer cargo)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
