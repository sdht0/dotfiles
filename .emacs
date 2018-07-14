(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(package-initialize)

;; utf-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load-theme 'whiteboard t)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)
;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
;; Remove the tool bar
(customize-set-variable 'tool-bar-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)

(setq-default indent-tabs-mode nil)

G(setq tramp-default-method "ssh")

;; always maximize the frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; do not create backup files
(setq make-backup-files nil)

;; latex support
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'reftex)
(setq TeX-auto-save t)
(setq TeX-save-query nil)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq TeX-source-correlate-mode t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)
(latex-preview-pane-enable)
(require 'company-auctex)
(company-auctex-init)
;; Okular
(setq TeX-view-program-list '(("Okular" "okular --unique %o#src:%n%b")))
(add-hook 'LaTeX-mode-hook '(lambda ()
                  (add-to-list 'TeX-expand-list
                       '("%u" Okular-make-url))))
(defun Okular-make-url () (concat
               "file://"
               (expand-file-name (funcall file (TeX-output-extension) t)
                         (file-name-directory (TeX-master-file)))
               "#src:"
               (TeX-current-line)
               (expand-file-name (TeX-master-directory))
               "./"
               (TeX-current-file-name-master-relative)))
(setq TeX-view-program-selection '((output-pdf "Okular")))

;; rust support
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(require 'rust-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-align-annotations t)

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#373b41" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
 '(beacon-color "#cc6666")
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (github-modern-theme intellij-theme color-theme-sanityinc-tomorrow solarized-theme color-theme-modern company-auctex latex-preview-pane haskell-mode company racer)))
 '(show-paren-mode t)
 '(tramp-terminal-type "xterm")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal)))))

