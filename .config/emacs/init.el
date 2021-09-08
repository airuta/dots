;; Make emacs startup faster
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Load literate config
(org-babel-load-file
  (expand-file-name
    "config.org"
    user-emacs-directory))

;; Customize stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-mode flx dracula-theme emacs-dracula solaire-mode helpful ivy-rich rainbow-delimiters ivy-prescient counsel ivy projectile diminish yascroll minimap general simple-modeline mini-modeline mood-line which-key doom-themes parinfer-rust-mode evil-collection evil use-package undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
