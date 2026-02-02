;;; custom.el --- customization file -*-lexical-binding: t-*- ;; -*-Emacs-Lisp-*-

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "androidsu")
      tramp-androidsu-connection-local-default-profile tramp-adb-connection-local-default-shell-profile tramp-adb-connection-local-default-ps-profile)
     ((:application tramp :protocol "adb")
      tramp-adb-connection-local-default-shell-profile tramp-adb-connection-local-default-ps-profile)
     ((:application vc-git)
      vc-git-connection-default-profile)
     ((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile tramp-flatpak-connection-local-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-androidsu-connection-local-default-profile
      (tramp-remote-path "/system/bin" "/system/xbin"))
     (tramp-adb-connection-local-default-ps-profile
      (tramp-process-attributes-ps-args)
      (tramp-process-attributes-ps-format
       (user . string)
       (pid . number)
       (ppid . number)
       (vsize . number)
       (rss . number)
       (wchan . string)
       (pc . string)
       (state . string)
       (args)))
     (tramp-adb-connection-local-default-shell-profile
      (shell-file-name . "/system/bin/sh")
      (shell-command-switch . "-c"))
     (vc-git-connection-default-profile
      (vc-git--program-version))
     (tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
                               (tramp-kubernetes--container
                                (car tramp-current-connection))
                               104
                               (tramp-kubernetes--pod
                                (car tramp-current-connection))
                               120
                               (tramp-kubernetes--context-namespace
                                (car tramp-current-connection))))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-enabled-themes '(fontaine) nil nil "Customized with use-package imenu")
 '(custom-safe-themes t nil nil "Customized with use-package imenu")
 '(flycheck-flake8-maximum-line-length 200)
 '(native-comp-async-report-warnings-errors nil)
 '(package-selected-packages
   '(git-gutter mic-paren powerline cond-let eglot eldoc jsonrpc svg-lib track-changes vertico-prescient anzu compat ace-window async avy cfrs consult corfu dash elisp-refs eval-in-repl f font-utils ht hydra jeison list-utils llama lv magit magit-section paredit pcache persistent-soft pfuture posframe prescient pythonic queue s spinner transient treemacs ucs-utils undo-tree vertico with-editor markdown-ts-mode yaml-mode ws-butler volatile-highlights unicode-fonts undo-fu treesit-fold treemacs-magit textsize smooth-scrolling smartparens shift-number rainbow-delimiters powerthesaurus pos-tip perl-ts-mode paradox orderless modern-sh modern-cpp-font-lock minions marginalia loccur langtool kind-icon jinx indent-bars helpful flycheck ergoemacs-mode emojify diminish ctrlf csv-mode cpp-auto-include corfu-prescient consult-eglot clean-aindent-mode clang-capf cape bm blamer bazel bash-completion anaconda-mode))
 '(paradox-github-token t)
 '(rainbow-x-colors nil)
 '(safe-local-variable-values
   '((eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (enable-local-variables: . all)))
 '(tab-width 4)
 '(tool-bar-position 'top)
 '(undo-tree-history-directory-alist '(("." . "~/.config/emacs/undo-tree/"))))

(message "setting faces...")

;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
