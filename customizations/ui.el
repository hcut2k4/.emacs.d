;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Show line numbers
;;(global-linum-mode)
(setq column-number-mode t)

;; Turn off line wrapping
(set-default 'truncate-lines t)

;; Change wheel scroll amount and make it smooth
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Remove ugly graphical toolbar
 (when (fboundp 'tool-bar-mode)
   (tool-bar-mode -1))

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'monokai t)

;; increase font size for better readability
(set-face-attribute 'default nil :height 110)

;; set font to something not ugly
(set-face-attribute 'default nil :font "Consolas" )

;; Start in full screen. If you prefer maximized, comment fullscreen and uncomment maximized
(toggle-frame-fullscreen)
;;(toggle-frame-maximized)

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
;; (blink-cursor-mode 0)

;; full path in title bar
;; (setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
;; (global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
 (setq ring-bell-function 'ignore)

;; Enable NeoTree
(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Switch to previous window
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))

;; Scroll window using ALT-arrow
(global-set-key (kbd "ESC <down>") 'gcm-scroll-down)
(global-set-key (kbd "ESC <up>") 'gcm-scroll-up)
(global-set-key (kbd "M-<down>") 'gcm-scroll-down)
(global-set-key (kbd "M-<up>") 'gcm-scroll-up)

;; resize windows quickly
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


