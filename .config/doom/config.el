;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Valerii Praid"
      user-mail-address "valerii.praid@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Make which-key more responsive
(after! which-key
  (setq! which-key-idle-delay 0.1
         which-key-idle-secondary-delay 0.2))

;; Remove icons from treemacs
(after! treemacs
  (setq treemacs-no-png-images t))

;; Set up org mode
(after! org
  (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5))
    (set-face-attribute face nil :height 1.0 :background nil :weight 'semi-light))
  (setq! org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "SOMEDAY" "PROJ" "|" "DONE" "CANCELED"))
         org-directory "~/Drive/org"
         org-default-notes-file (concat org-directory "/inbox.org")
         org-roam-directory "~/Drive/garden"
         org-journal-dir "~/Drive/journal"
         org-journal-date-prefix "#+TITLE: "
         org-journal-time-prefix "* "
         org-journal-date-format "%a %Y-%m-%d"
         org-journal-file-format "%Y-%m-%d.org")
  (setq org-agenda-files
     (seq-filter
       (lambda(x) (not (string-match "/code/" (file-name-directory x))))
       (append
         (list "~/Drive/org/inbox.org")
         (directory-files-recursively "~/Drive/garden" "\\.org$")
         (directory-files-recursively "~/Drive/journal" "\\.org$")
         (directory-files-recursively "~/Drive/articles" "\\.org$")
         (directory-files-recursively "~/Drive/books" "\\.org$")
         (directory-files-recursively "~/Drive/courses" "\\.org$"))))
  (setq org-capture-templates
    '(("i" "Inbox" entry  (file org-default-notes-file) "* TODO %?"
       :prepend t
       :kill-buffer t)))
  (setq org-agenda-prefix-format
    '((agenda . " %i %-12:c%?-12t% s")
      (todo   . " ")
      (tags   . " %i %-12:c")
      (search . " %i %-12:c"))))

;; Set up capturing
(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))
(map!
  :after org
  :leader
  :desc "Capture"
  "x" #'org-capture-inbox)

;; Add org-transclusion
;; In ~/.doom.d/config.el
(use-package! org-transclusion
  :defer
  :after org
  :init
  (map!
    :map global-map "<f12>" #'org-transclusion-add
    :leader
    :prefix "n"
    :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

;; Set up todo highlighting colors
(after! hl-todo
  (setq hl-todo-keyword-faces
    '(("TODO"      . "#E6B168")
      ("NEXT"      . "#FC5358")
      ("WAITING"   . "#439EEA")
      ("SOMEDAY"   . "#B05ACC")
      ("PROJ"      . "#CF7039")
      ("DONE"      . "#88B453")
      ("CANCELED"  . "#998CD9"))))


;; Change font settings
(setq doom-font (font-spec :family "Lekton Nerd Font Mono" :size 14 :weight 'semi-light))
(setq-default line-spacing 0.7)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
