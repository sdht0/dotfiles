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
(setq fill-column 100)                      ;; Lines should be 100 characters wide, not 72
(fset 'yes-or-no-p 'y-or-n-p)               ;; y/n instead of yes/no
(show-paren-mode 1)
(setq-default show-trailing-whitespace t)
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

(use-package ivy
    :ensure t
    :init
        (ivy-mode 1)
    :config
        (setq ivy-use-virtual-buffers t
                ivy-count-format "%d/%d ")
        (global-set-key "\C-s" 'swiper)
        (global-set-key (kbd "C-c C-r") 'ivy-resume)
        (global-set-key (kbd "<f6>") 'ivy-resume)
        (global-set-key (kbd "M-x") 'counsel-M-x)
        (global-set-key (kbd "C-x C-f") 'counsel-find-file)
        (global-set-key (kbd "<f1> f") 'counsel-describe-function)
        (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
        (global-set-key (kbd "<f1> l") 'counsel-find-library)
        (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
        (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
        (global-set-key (kbd "C-c g") 'counsel-git)
        (global-set-key (kbd "C-c j") 'counsel-git-grep)
        (global-set-key (kbd "C-c k") 'counsel-ag)
        (global-set-key (kbd "C-x l") 'counsel-locate)
        (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
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
    :config
        (evil-mode))

(use-package ace-window
    :ensure t)

(use-package flycheck
    :ensure t
    :init (global-flycheck-mode))

(use-package ibuffer
    :ensure t
    :bind ("C-x C-b" . ibuffer))

(use-package hydra
    :ensure t
    :config
        (global-set-key
            (kbd "C-M-o")
            (defhydra hydra-window ()
                "Split: _v_ert _x_:horz
                Delete: _o_nly  _da_ce  _dw_indow  _db_uffer
                Move: _s_wap"
                ("h" windmove-left)
                ("j" windmove-down)
                ("k" windmove-up)
                ("l" windmove-right)
                ("|" (lambda ()
                        (interactive)
                        (split-window-right)
                        (windmove-right)))
                ("_" (lambda ()
                        (interactive)
                        (split-window-below)
                        (windmove-down)))
                ("v" split-window-right)
                ("x" split-window-below)
                ("o" delete-other-windows :exit t)
                ("s" ace-swap-window)
                ("da" ace-delete-window)
                ("dw" delete-window)
                ("db" kill-this-buffer))))

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
