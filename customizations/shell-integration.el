;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
;;(when (memq window-system '(mac ns))
;;  (exec-path-from-shell-initialize)
;;  (exec-path-from-shell-copy-envs
;;   '("PATH")))

;; Setting up environment for Git bash on Windows
(when (equal system-type 'windows-nt)
		(setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
		(setq shell-file-name explicit-shell-file-name)
        (add-to-list 'exec-path "C:/Program Files/Git/bin"))

(setenv "HOME" "C:/Users/khoneycutt")
(add-to-list 'exec-path "~/bin")
(setenv "PATH" (mapconcat #'identity exec-path path-separator))
