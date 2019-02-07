;;; .emacs --- Emacs init file
;;; Code:

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

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode t)
(scroll-bar-mode -1)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(setq-default fill-column 80)               ;; Lines should be 80 characters wide, not 72
(fset 'yes-or-no-p 'y-or-n-p)               ;; y/n instead of yes/no
(show-paren-mode 1)
;; (setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)
(setq sentence-end-double-space nil)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(customize-set-variable 'tool-bar-mode nil) ;; Remove the tool bar
(add-to-list 'default-frame-alist '(fullscreen . maximized))    ;; always maximize the frame

(setq-default tramp-default-method "ssh")

(define-key global-map [f2] 'toggle-truncate-lines)
(define-key global-map [f3] 'display-line-numbers-mode)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "s-,") 'switch-to-buffer)
(global-set-key (kbd "s-.") 'projectile--find-file)
(global-set-key (kbd "s-;") 'kill-this-buffer)
(global-set-key (kbd "s-'") 'delete-window)
(global-set-key (kbd "s-\\") 'delete-other-windows)

(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-,") 'eval-buffer)
(global-set-key (kbd "C-.") 'eval-region)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-r") 'query-replace-regexp)
(global-set-key (kbd "C-R") 'replace-regexp)

(global-set-key (kbd "s-[") (lambda ()(interactive)(split-window-below)(windmove-down)))
(global-set-key (kbd "s-]") (lambda ()(interactive)(split-window-right)(windmove-right)))
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

;;; Commentary:

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

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-dracula t)
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (set-cursor-color "white")))))

(use-package powerline
  :ensure t
  :requires projectile)

(setq powerline-default-separator 'curve)
(defface cjp-powerline-yellow '((t (:background "darkorange" :foreground "black" :inherit mode-line)))
  "Yellow" :group 'powerline)
(defface cjp-powerline-red '((t (:background "red" :foreground "white" :inherit mode-line)))
  "Red" :group 'powerline)
(defface cjp-powerline-green '((t (:background "#afd700" :foreground "#2d2d2d" :inherit mode-line)))
  "" :group 'powerline)
(defface cjp-powerline-blue '((t (:background "#0087ff" :foreground "white" :inherit mode-line)))
  "Blue" :group 'powerline)
(defun my-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
        '("%e"
          (:eval
           (let* ((active (powerline-selected-window-active))
                  (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                  (mode-line (if active 'mode-line 'mode-line-inactive))
                  (face-blue (if active 'cjp-powerline-blue 'mode-line-inactive))
                  (face-red 'cjp-powerline-red)
                  (face-green 'cjp-powerline-green)
                  (face-yellow 'cjp-powerline-yellow)
                  (separator-left (intern (format "powerline-%s-%s"
                                                  (powerline-current-separator)
                                                  (car powerline-default-separator-dir))))
                  (separator-right (intern (format "powerline-%s-%s"
                                                   (powerline-current-separator)
                                                   (cdr powerline-default-separator-dir))))
                  (lhs (list (powerline-raw evil-mode-line-tag face-red)
                             (funcall separator-left face-red face-yellow)
                             (powerline-raw (concat "[" (projectile-project-name) "]") face-yellow 'l)
                             (powerline-raw "%*" face-yellow 'l)
                             (powerline-buffer-id face-yellow 'l)
                             (powerline-vc face-yellow 'l)
                             (powerline-raw " " face-yellow)
                             (funcall separator-right face-yellow face-green)
                             (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                               (powerline-raw erc-modified-channels-object face-green 'l))
                             (powerline-major-mode face-green 'l)
                             (powerline-raw " " face-green)
                             (funcall separator-left face-green face-blue)
                             (powerline-process face-blue 'l)
                             (powerline-minor-modes face-blue 'l)
                             (powerline-raw " " face-blue)
                             (funcall separator-left face-blue mode-line)))
                  (rhs (list (funcall separator-right mode-line face-blue)
                             (powerline-raw global-mode-string face-blue 'r)
                             (powerline-raw "%4l" face-blue 'r)
                             (powerline-raw ":" face-blue 'r)
                             (powerline-raw "%3c" face-blue 'r)
                             (powerline-raw "%4p" face-blue 'r)
                             (powerline-buffer-size face-blue 'r)
                             (powerline-raw mode-line-mule-info face-blue 'r)
                             (powerline-hud face-red mode-line))))
             (concat (powerline-render lhs)
                     (powerline-fill mode-line (powerline-width rhs))
                     (powerline-render rhs)))))))
(my-powerline-theme)

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
        ivy-count-format "%d/%d "
        ivy-wrap t
        ivy-height 25)
  (global-set-key (kbd "C-f") 'swiper)
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

;; (use-package elpy
;;   :commands elpy-enable
;;   :after flycheck
;;   :init (add-hook 'python-mode-hook #'elpy-enable)
;;   :config
;;   (progn
;;     (setq elpy-rpc-backend "jedi"
;;           elpy-rpc-project-specific 't)
;;     (when (fboundp 'flycheck-mode)
;;       (setq elpy-modules (delete 'elpy-module-flymake elpy-modules)))))
;; 
;; (use-package company-jedi
;;   :ensure t
;;   :defer t
;;   :hook
;;   (python-mode . (lambda () (add-to-list (make-local-variable 'company-backends) '(company-jedi)))))

(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets         ; Collection of snippets
    :ensure t)
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook #'yas-minor-mode)
  (add-hook 'bibtex-mode-hook #'yas-minor-mode))

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
  :requires ivy
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

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-ignore-duplicate t))
(use-package company-lsp
  :ensure t
  :commands company-lsp)
(use-package lsp-mode
  :ensure t
  :requires lsp-ui company-lsp yasnippet
  :commands lsp
  :init
  (require 'lsp-clients)
  :config
  (setq lsp-prefer-flymake nil))

(use-package rust-mode
  :ensure t
  :requires lsp-mode
  :config
  (add-hook 'rust-mode-hook #'lsp)
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

(setq ispell-dictionary "american")

(use-package tex-site
  :ensure auctex
  :after (tex latex)
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq-default TeX-auto-save t
                TeX-parse-self t
                TeX-master nil
                TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (turn-on-reftex)
              (setq-default reftex-plug-into-AUCTeX t)
              (reftex-isearch-minor-mode)
              (display-line-numbers-mode t)))
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; Indentation
  (setq-default LaTeX-indent-level 4
                LaTeX-item-indent 0
                TeX-brace-indent-level 4
                TeX-newline-function 'newline-and-indent)

  ;; Some usefull hooks
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)

  ;; Build latex after saving
  (add-hook 'LaTeX-mode-hook
            (lambda() (add-hook 'after-save-hook
                                (lambda ()(TeX-command-run-all nil)) nil 'make-it-local)))

  ;; to use pdfview with auctex
  (add-hook 'LaTeX-mode-hook 'pdf-tools-install)

  ;; to use pdfview with auctex
  (setq-default TeX-view-program-selection '((output-pdf "pdf-tools"))
                TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view"))
                TeX-source-correlate-mode t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode))

(use-package bibtex
  :ensure t
  :defer t
  :bind (:map bibtex-mode-map
         ("M-q" . bibtex-fill-entry))
  :config
  (setq bibtex-align-at-equal-sign t))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args nil))

(use-package pdf-tools
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install t)
  (setq mouse-wheel-follow-mouse t)
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-resize-factor 1.10))

(use-package company-reftex
  :ensure t
  :defer t
  :hook
  (LaTeX-mode . (lambda () (add-to-list 'company-backends '(company-reftex-labels
                                                            company-reftex-citations)))))
(use-package company-auctex
  :ensure t
  :defer t)
(use-package company-math
  :ensure t
  :defer t
  :requires company-reftex company-auctex
  :hook
  (LaTeX-mode . (lambda () (add-to-list 'company-backends
                                        '(company-math-symbols-latex
                                          company-auctex-macros
                                          company-auctex-environments)))))

; (use-package company-bibtex
;   :ensure t
;   :defer t
;   :hook
;   (LaTeX-mode . (lambda () (add-to-list 'company-backends 'company-bibtex)))
;   (org-mode . (lambda () (add-to-list 'company-backends 'company-bibtex))))

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
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-include-signature nil)
 '(lsp-ui-doc-max-width 100)
 '(lsp-ui-doc-position (quote top))
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-sideline-enable nil)
 '(package-selected-packages
   (quote
    (visual-regexp-steroids spaceline doom-themes doom-theme yasnippet-snippets smartparens smartparens-config powerline-evil counsel use-package swiper projectile powerline pdf-tools magit lsp-ui lsp-rust evil dracula-theme company-reftex company-math company-lsp company-bibtex company-auctex cargo)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-sideline-symbol ((t (:foreground "white" :box (:line-width -1 :color "dim gray") :height 0.99)))))

(provide '.emacs)

;;; emacs.init.el ends here
