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
 '(custom-enabled-themes (quote (misterioso)))
 '(package-selected-packages
   (quote
	(wgrep which-key counsel projectile-ripgrep editorconfig protobuf-mode typescript-mode lsp-java lsp-mode yasnippet treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil dap-mode helm-lsp lsp-treemacs company-lsp flycheck lsp-ui treemacs company flymake-go markdown-mode restclient tide multiple-cursors yaml-mode magit flycheck-golangci-lint go-rename exec-path-from-shell web-mode company-go go-mode projectile neotree))))

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "D2Coding" :foundry "RIXF" :slant normal :weight normal :height 128 :width normal))))
 '(markdown-code-face ((t (:inherit consolas)))))
(set-fontset-font t 'hangul (font-spec :family "D2Coding"))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "GOPATH")

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PATH")


(require 'linum)
(global-linum-mode)

(editorconfig-mode 1)

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
(setq indent-tabs-mode t)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 4) ; emacs 23.1, 24.2, default to 8
;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 4)

(require 'which-key)
(which-key-mode)

(require 'wgrep)

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
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
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
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

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
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode)

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
)


;; (setq lsp-gopls-staticcheck t)
;; (setq lsp-eldoc-render-all t)
;; (setq lsp-gopls-complete-unimported t)


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

(require 'dap-go)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
)
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

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
  (setq company-minimum-prefix-length 3))

(global-set-key [C-tab] 'company-complete)

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

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
)
(add-hook 'go-mode-hook 'my-go-mode-hook)

(use-package lsp-mode
  :hook (typescript-mode . lsp)
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

(add-hook 'dap-terminated-hook 'my/hide-debug-windows)




(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)






(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(use-package org
  :config
  ;; org-mode는 기본적으로 강조문(굵게, 이탤릭 등)을 하나의 단어에 대해서만 적용하도록 하고 있습니다. 예컨대 *이렇게*는 굵게 글씨를 쓸 수 없습니다. 조사가 들어가는 한중일 언어에 쓰기에는 부적절한 정책이며, 이를 부분적으로 구현하기 위해, 유니코드 문자 중에 '보이지 않는 스페이스'를 사용하여 편법으로 부분 강조를 가능하게 합니다.
  ;; 참고: https://emacs.stackexchange.com/questions/18499/mark-up-only-part-of-a-word/18511
  (defun my/insert-zero-width-space ()
    (interactive)
    (insert-char #x200b))
  ;; Ctrl + * 를 누르면 강조문자 앞뒤에 해당 문자를 넣을 수 있습니다.
  (define-key org-mode-map (kbd "C-*") 'my/insert-zero-width-space)
  ;; 해당 문자를 스페이스와 같은 취급을 하도록 설정을 바꿔줍니다.
  (setq org-emphasis-regexp-components '(" \t('\"{\x200B" "- \t.,:!?;'\")}\\[\x200B" " \t\r\n,\"'" "." 1)))



