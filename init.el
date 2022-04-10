
;;; TODO
;; Create shortcut for reloading init.el/config
;; Set up persistent undo
;; Add user shortcuts (e.g. leader key) - General.el? (linting, file explorer etc)
;; Migrate todo to ORG + create shortcut to open todo.org
;; Automate code-formatting (shortcut)?
;; Add diff functionality
;; Add JSON formatting functionality
;; Add Markdown functionality
;; Add JavaScript functionality
;; Add TypeScript functionality
;; FileType config: global/defaults
;; FileType config: clj / html / css / js / ts
;; git
;; comment plugin? For nice comment shortcuts

;;; Core
(auto-save-mode -1)

(setq ns-right-alternate-modifier (quote none)) ; Use right-alt for "#"

;;; Package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages
  '(base16-theme
    better-defaults
    rainbow-delimiters
    general
    projectile
    counsel
    company
    clojure-mode
    cider
    flycheck-clj-kondo
    paredit
    neotree
    magit
    evil))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; SHELL / Path

;; Note this package had to be installed manually as the package wasn't found on MELPA for some reason
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; Backups
(setq
   backup-by-copying t                    ; don't clobber symlinks
   backup-directory-alist
   '(("." . "~/.emacs.d/auto-backups/"))  ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t                      ; use versioned backups
)

(setq auto-save-list-file-prefix "~/.emacs.d/auto-saves/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/" t)))

;;; Theme
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq base16-distinct-fringe-background nil)
(setq base16-highlight-mode-line "contrast")

(load-theme 'base16-nord t)

(add-to-list 'default-frame-alist
             '(font . "Fantasque Sans Mono-16"))


;;; Shortcuts (not working yet)
(require 'general)
;; * Global Keybindings
;; `general-define-key' acts like `global-set-key' when :keymaps is not
;; specified (because ":keymaps 'global" is the default)
;; kbd is not necessary and arbitrary amount of key def pairs are allowed
(general-define-key
 "C-c e" '(find-file "~/.emacs.d/init.el"))

;;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;(setq projectile-completion-system 'ivy) ; from old config, not sure if required

;; Company
(global-company-mode)
(add-hook 'after-init-hook 'global-company-mode)
;; aligns annotation to the right hand side
; (setq company-tooltip-align-annotations t) ; from old config, not sure if required

;; delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; Language Specific Stuff

;;; Clojure(Script) / LISP

;;; cider
(setq cider-font-lock-dynamically '(macro core function var))
(setq cider-repl-pop-to-buffer-on-connect nil)

(add-hook 'before-save-hook 'cider-format-buffer t t)

;;; paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                   #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook  #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                         #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                         #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook             #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                       #'enable-paredit-mode)
(add-hook 'clojure-mode-hook                      #'enable-paredit-mode)
(add-hook 'clojurescript-mode-hook                #'enable-paredit-mode)

;; flycheck-clj-kondo
(require 'flycheck-clj-kondo)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; other
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(general projectile paredit ivy cider better-defaults))
 '(safe-local-variable-values
   '((cider-shadow-cljs-default-options . "app")
     (cider-default-cljs-repl . shadow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
