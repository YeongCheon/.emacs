(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)
(setq package-check-signature nil) ;; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(org-agenda-files nil)
 '(package-selected-packages
   '(prettier-mode fly-check rust-mode add-node-modules-path js2-mode dap-go org-appear mixed-pitch company-posframe org-superstar prettier ox-gfm iedit kotlin-mode wgrep which-key counsel projectile-ripgrep editorconfig protobuf-mode typescript-mode lsp-java lsp-mode yasnippet treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil dap-mode helm-lsp lsp-treemacs company-lsp flycheck lsp-ui treemacs company flymake-go markdown-mode restclient tide multiple-cursors yaml-mode magit flycheck-golangci-lint go-rename exec-path-from-shell web-mode company-go go-mode projectile neotree)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "D2Coding" :foundry "RIXF" :slant normal :weight normal :height 128 :width normal))))
 '(markdown-code-face ((t (:inherit consolas)))))
(set-fontset-font t 'hangul (font-spec :family "D2Coding"))
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)

(use-package exec-path-from-shell
  :ensure t
  )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GOPATH")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PATH")

(require 'linum)
(global-linum-mode)

;; (editorconfig-mode 1)

;; 
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)
;; (set-language-environment "UTF-8")
(setq default-korean-keyboard "3")
(setq default-input-method "korean-hangul390")
(global-set-key (kbd "S-SPC") 'toggle-input-method)
(global-set-key (kbd "<kana>") 'toggle-input-method)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)
;; make indentation commands use space only (never tab character)
;; (setq indent-tabs-mode t)
;; set default tab char's display width to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

(use-package counsel
  :ensure t
  )

(use-package prettier
  :ensure t
  )

(use-package prettier-js
  :ensure t
  )

(use-package flycheck
  :ensure t
  )

(use-package wgrep
  :ensure t
  )

(use-package iedit
  :ensure t
  )

(use-package which-key
  :ensure t
  :init
  (which-key-mode)

  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-S-s") 'swiper-all)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c j") 'counsel-rg)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package multiple-cursors
  :ensure t
  :init
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)
	(define-key treemacs-mode-map (kbd "C-f") #'treemacs-toggle-node)
	)
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  )

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package projectile
  :ensure t
  )
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; (use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  )

(lsp-treemacs-sync-mode 1)

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook (go-mode . lsp-deferred))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  ;; (setq company-idle-delay 100)
  (setq company-minimum-prefix-length 1))

(global-set-key [C-tab] 'company-complete)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook
  (go-mode . yas-minor-mode)
  (rust-mode . yas-minor-mode)
  )

(defun my-go-mode-hook ()
  ; Use goimports instead of go-fmt
  ;(setq gofmt-command "goimports")
  ; Call Gofmt before saving
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ;  Godef jump key binding
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "<f8>") 'dap-breakpoint-toggle)
  (add-hook 'before-save-hook 'lsp-format-buffer t t)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(use-package lsp-mode
  :hook (typescript-mode . lsp)
  (web-mode . lsp)
  :commands lsp)
(setq typescript-indent-level 2)

(use-package lsp-mode
  :hook (scss-mode . lsp)
  :commands lsp)
(setq scss-indent-level 2)

(setq compilation-window-height 14)
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
	(let* ((w (split-window-vertically))
	       (h (window-height w)))
	  (select-window w)
	  (switch-to-buffer "*compilation*")
	  (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(setq compilation-scroll-output t)


(defun my/window-visible (b-name)
  "Return whether B-NAME is visible."
  (-> (-compose 'buffer-name 'window-buffer)
      (-map (window-list))
      (-contains? b-name)))

(defun my/show-debug-windows (session)
  "Show debug windows."
  (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
    (save-excursion
      ;; display locals
      (unless (my/window-visible dap-ui--locals-buffer)
        (dap-ui-locals))
      ;; display sessions
      (unless (my/window-visible dap-ui--sessions-buffer)
        (dap-ui-sessions)))))

(add-hook 'dap-stopped-hook 'my/show-debug-windows)

(defun my/hide-debug-windows (session)
  "Hide debug windows when all debug sessions are dead."
  (unless (-filter 'dap--session-running (dap--get-sessions))
    (and (get-buffer dap-ui--sessions-buffer)
         (kill-buffer dap-ui--sessions-buffer))
    (and (get-buffer dap-ui--locals-buffer)
         (kill-buffer dap-ui--locals-buffer))))

;; (add-hook 'dap-terminated-hook 'my/hide-debug-windows)


;; (setenv "NODE_PATH" "/usr/local/lib/node_modules")
(defun setup-typescript-mode ()
  (interactive)
  ;; (tide-setup)
  ;; (setq lsp-disabled-clients '(angular-ls))
  (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (local-set-key (kbd "<f8>") 'dap-breakpoint-toggle)
  ;; (add-hook 'before-save-hook 'lsp-format-buffer)
  ;; (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (prettier-mode +1)
)

(add-hook 'typescript-mode-hook #'setup-typescript-mode)
(add-hook 'typescript-mode-hook #'add-node-modules-path)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(use-package web-mode
  :ensure t
  :init
  ;; (setq web-mode-enable-auto-indentation nil)
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  :hook
  (web-mode . prettier-js-mode)
  (web-mode . add-node-modules-path)
  )

(put 'set-goal-column 'disabled nil)

(editorconfig-mode 1)

(show-paren-mode 1)


(defun my-hide-compilation-buffer (proc)
  "Hide the compile buffer `PROC' is ignored."
  (let* ((window (get-buffer-window "*compilation*"))
         (frame (window-frame window)))
    (ignore-errors
      (delete-window window))))

(add-hook 'compilation-start-hook 'my-hide-compilation-buffer)

(setq lsp-clients-angular-language-server-command
  '("node"
    "/usr/local/lib/node_modules/@angular/language-server"
    "--ngProbeLocations"
    "/usr/local/lib/node_modules"
    "--tsProbeLocations"
    "/usr/local/lib/node_modules"
    "--stdio"))

(add-to-list 'lsp-disabled-clients '(typescript-mode . angular-ls))

;;; ricing-org-mode start
;;; https://lucidmanager.org/productivity/ricing-org-mode/
(use-package org
  :ensure t
  :init
  (add-hook 'org-mode-hook
			(lambda ()
			  (setq line-spacing 6))))

(setq
	 org-adapt-indentation nil
	 org-src-fontify-natively t
	 org-src-window-setup 'current-window
	 org-src-preserve-indentation nil
	 org-edit-src-content-indentation 0
	 org-src-tab-acts-natively t)

(setcar org-emphasis-regexp-components " \t('\"{[:multibyte:]")
(setcar (nthcdr 1 org-emphasis-regexp-components) "[:multibyte:]- \t.,:!?;'\")}\\")
(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

;; Set default, fixed and variabel pitch fonts
;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "D2Coding" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "D2Coding")
  (set-face-attribute 'variable-pitch nil :font "D2Coding")
  )

;; Required for proportional font
(use-package company-posframe
  :ensure t
  :config
  (company-posframe-mode 1))

;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Nice bullets
(use-package org-superstar
  :ensure t
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

;; Increase size of LaTeX fragment previews
(plist-put org-format-latex-options :scale 2)

;; Distraction-free screen
(use-package olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free)))

;;; ricing-org-mode end

(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  )

(use-package protobuf-mode
  :ensure t
  )

(use-package add-node-modules-path
  :ensure t
  )

(defun my-rust-mode-hook ()
  (local-set-key (kbd "M-*") 'pop-tag-mark)
  (add-hook 'before-save-hook 'lsp-format-buffer t t)
)
(add-hook 'rust-mode-hook 'my-rust-mode-hook)

(use-package
  rustic
  :ensure t
  )

(use-package rust-mode
  :ensure t
  :hook
  (rust-mode . lsp)
  :config
  (setq indent-tabs-mode nil)
  (setq rust-indent-level 4)
  (setq rust-format-on-save t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  ;; (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-reborrow-hints nil)

  ;; rust lsp-clients-extract-signature-on-hover
  (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
  (-let* (((&hash "value") contents)
          (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
          (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                         (-third-item groups)
                       (car groups)))
          (sig (--> sig_group
                    (--drop-while (s-equals? "```rust" it) it)
                    (--take-while (not (s-equals? "```" it)) it)
                    (--map (s-trim it) it)
                    (s-join " " it))))
    (lsp--render-element (concat "```rust\n" sig "\n```"))))
  )

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)


(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package lsp-tailwindcss
  :ensure t
  )
