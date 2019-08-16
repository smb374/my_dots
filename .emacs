
(if (window-system)
 (set-face-attribute
  'default nil :font (font-spec :family "MesloLGMDZ Nerd Font Mono" :style "RegularForPowerline" :size 12))
 )
;;
(if (window-system)
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "Noto Sans CJK TC"  :size 12)))
)
(if (window-system)
(dolist (charset '(symbol))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "MesloLGMDZ Nerd Font Mono"  :size 12)))
)
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
;; (global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(setq inhibit-compacting-font-caches t)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(setq-default select-enable-clipboard t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "80365dd15f97396bdc38490390c23337063c8965c4556b8f50937e63b5e9a65c" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "256bd513a9875cd855077162cdfee8d75b0ad7e18fe8b8cbc10412561fbef892" "b73a23e836b3122637563ad37ae8c7533121c2ac2c8f7c87b381dd7322714cd0" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "84da7b37214b4ac095a55518502dfa82633bee74f64daf6e1785322e77516f96" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (company-lua lua-mode sly sublimity base16-theme base16-emacs spaceline-config evil-nerd-commenter evil-leader auto-compile autocompile git-gutter evil-vimish-fold evil undo-tree swiper use-package spaceline)))
 '(zoom-size (quote (0.618 . 0.618))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(whitespace-tab ((t (:foreground "#636363")))))
;; env-customization - TAB hacks
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(defvar custom-tab-width 4)
;; indent
(setq-default c-basic-offset 4) ; indents 4 chars
(setq tab-width 4)              ; and 4 char wide for TAB
(setq-default indent-tabs-mode nil)     ; And force use of spaces
(setq-default evil-shift-width custom-tab-width)
(infer-indentation-style)
(setq backward-delete-char-untabify-method 'hungry)
;; WARNING: This will change your life
;; (OPTIONAL) Visualize tabs as a pipe character - "|"
;; This will also show trailing characters as they are useful to spot.
(setq whitespace-style '(face tabs tab-mark trailing))

(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
;; linum
(global-display-line-numbers-mode)
;; keys
(global-set-key (kbd "M-p") 'previous-buffer)
(global-set-key (kbd "M-n") 'next-buffer)
(global-set-key (kbd "C-<up>") 'shrink-window)
(global-set-key (kbd "C-<down>") 'enlarge-window)
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)

;; page-break-lines
(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode)
  )
;; smart backspace
(use-package smart-hungry-delete
  :ensure t
  :bind
  (("<backspace>" . smart-hungry-delete-backward-char)
   ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config
  (smart-hungry-delete-add-default-hooks)
  )
(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  )
;; ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'ace-window)
  )
;; anzu
(use-package anzu
  :ensure t
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode)
  )
;; auto-pair
(use-package autopair
  :ensure t
  :config
  (require 'autopair)
  (autopair-global-mode)
  )
;; evil-mode
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  )
(use-package goto-chg
  :ensure t
  )
(use-package evil
  :ensure t
  :init
  :config
  (evil-set-initial-state 'eshell-mode 'emacs)
  (setq-default evil-search-module 'evil-search)
  (define-key evil-insert-state-map [?\C-?] 'smart-hungry-delete-backward-char)
  (evil-mode 1)
  (use-package evil-leader
    :ensure t
    :config
    (require 'evil-leader)
    (global-evil-leader-mode)
    (setq evil-leader/leader ";")
    (evil-leader/set-key
      "n" 'evil-next-buffer
      "m" 'evil-previous-buffer
      "e" 'counsel-find-file
      "b" 'counsel-ibuffer
      "k" 'kill-buffer
      "r" 'eval-buffer
      "2" 'split-window-below
      "3" 'split-window-right
      "hf" 'helpful-callable
      "hv" 'helpful-variable
      "hk" 'helpful-key
      "hs" 'eshell
      )
    )
  (use-package evil-vimish-fold
    :ensure t
    :config
    (evil-vimish-fold-mode 1)
    )
  (use-package evil-nerd-commenter
    :ensure t
    :config
    ;; Emacs key bindings
    (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
    (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
    (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
    (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
    ;; Vim key bindings
    (evil-leader/set-key
      "cc" 'evilnc-comment-or-uncomment-lines
      "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
      "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
      "ci" 'evilnc-copy-and-comment-lines
      "cp" 'evilnc-comment-or-uncomment-paragraphs
      "cr" 'comment-or-uncomment-region
      "cv" 'evilnc-toggle-invert-comment-line-by-line
      "."  'evilnc-copy-and-comment-operator
      "\\" 'evilnc-comment-operator ; if you prefer backslash key
      )
    )
  (use-package evil-surround
    :ensure t
    :init
    (global-evil-surround-mode 1)
    )
  )
;; snippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets
    :ensure t
    )
  )
;; company
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (add-to-list 'company-backends 'company-elisp)))
  (defun add-pcomplete-to-capf ()
    "A hook"
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-backends
        '((company-files
           company-keywords
           company-capf
           company-yasnippet)
          (company-abbrev company-dabbrev))))
(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode)
  )
;; flycheck
(use-package flycheck
  :ensure t
  :config
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
#b00111000
#b01111100
#b11111110
#b11111110
#b01111100
#b00111000
#b00000000
            ))

  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
    (vector
#b011111111110000
#b011111111111000
#b011111111111100
#b011111111111110
#b011111111111100
#b011111111111000
#b011111111110000
            ))
  (flycheck-define-error-level 'warning
    :severity 100
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-arrow
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )
;; ivy & swiper
(use-package ivy
  :ensure t
  :bind
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
  (use-package counsel
    :ensure t
    :config
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    )
  (use-package swiper
    :ensure t
    :config
    (setq ivy-use-selectable-prompt t)
    (evil-leader/set-key "s" 'counsel-grep-or-swiper)
    (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
    )
  (use-package amx
    :ensure t
    )
  (use-package flx
    :ensure t
    )
  )
;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-set-navigator t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook)
  )
;; Project
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )
;; Nix
(use-package json-mode
  :ensure t
  )
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  )
;; (use-package company-nixos-options
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-nixos-options)
;;   )
;; Common Lisp
;; (use-package slime-company
;;   :ensure t
;;   )
(use-package sly
  :ensure t
  :hook((common-lisp-mode . sly-editing-mode))
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  ;; (add-hook 'slime-mode-hook (lambda () (add-to-list 'company-backends '(slime-company))))
  (setq sly-complete-symbol-function 'sly-flex-completions)
  )
;; Lua
(use-package lua-mode
  :ensure t
  :config
  (use-package company-lua
    :ensure t
    :config
    ;; (add-hook 'lua-mode-hook (lambda () (add-to-list 'company-backends '(company-lua))))
    (defun my-lua-mode-company-init ()
      (setq-local company-backends '((company-lua
                                      company-etags
                                      company-dabbrev-code
                                      company-yasnippet))))
    (add-hook 'lua-mode-hook #'my-lua-mode-company-init)
    )
  )
;; C/C++
(use-package irony
  :ensure t
  :hook((c-mode . irony-mode)
        (c++-mode . irony-mode))
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (use-package company-irony-c-headers
    :ensure t
    )
  (use-package company-irony
    :ensure t
    :config
    (add-hook 'irony-mode-hook (lambda () (add-to-list 'company-backends 'company-irony)))
    )
  (use-package flycheck-irony
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
    )
  (use-package irony-eldoc
    :ensure t
    :config
    (add-hook 'irony-mode-hook #'irony-eldoc)
    )
  )
(use-package rtags
  :ensure t
  :config
  (rtags-enable-standard-keybindings)
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (rtags-start-process-unless-running)
  )
(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup)
  )
(use-package clang-format
  :ensure t
  :init
  (setq clang-format-style-option "llvm")
  :config
  (add-hook 'c-mode-hook (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))
  (add-hook 'c++-mode-hook (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))
  )
;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  (use-package intero
    :ensure t
    :hook
    ((haskell-mode . intero-mode))
    :config
    (intero-global-mode 1)
    )
  (use-package company-ghci
    :ensure t
    :hook((haskell-mode . company-mode)
          (intero-mode . company-mode))
    :config
    (add-hook 'haskell-mode-hook (lambda () (add-to-list 'company-backends '(company-ghci company-intero))))
    )
  (use-package flycheck-haskell
    :ensure t
    :init
    (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)
    )
  (use-package hindent
    :ensure t
    :init
    (when (file-executable-p "~/.local/bin/hindent")
      (setq hindent-process-path  "~/.local/bin/hindent"))
    (when (file-executable-p "~/.local/bin/brittany")
      (setq hindent-process-path "~/.local/bin/brittany"))
    (add-hook 'haskell-mode-hook #'hindent-mode)
    :config
    )
  )
;; Rust
(use-package rust-mode
  :ensure t
  :config
  (use-package racer
    :ensure t
    :hook((rust-mode . racer-mode)
          (racer-mode . eldoc-mode))
    :config
    (setq racer-cmd "/home/thomas/.cargo/bin/racer")
    )
  (use-package company-racer
    :ensure t
    :hook((rust-mode . company-mode)
          (racer-mode . company-mode))
    :config
    (add-hook 'haskell-mode-hook (lambda () (add-to-list 'company-backends 'company-racer)))
    )
  (use-package flycheck-rust
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    )
  (use-package cargo
    :ensure t
    :config
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    )
  )
;; Perl Mode
(use-package cperl-mode
  :ensure t
  :config
  (require 'cperl-mode)
  (defalias 'perl-mode 'cperl-mode)
  (use-package company-plsense
    :ensure t
    :init
    (add-hook 'cperl-mode-hook
              (lambda () (add-to-list 'company-backends 'company-plsense)))
    (add-hook 'cperl-mode-hook 'company-mode)
    )
  )
;; shell
(use-package company-shell
  :ensure t
  :init
  (add-hook 'shell-mode-hook
            (lambda () (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell))))
  )
;; org-mode
(use-package dash
  :ensure t
  )
(use-package monitor
  :ensure t
  )
(use-package org
  :ensure t
  :config
  (require 'org)
  (setq org-latex-classes
        '(("article"
           "
\\documentclass[12pt,a4paper]{
  article}
\\usepackage[margin=2cm]{
  geometry}
\\usepackage{
  xeCJK}
\\usepackage{
  hyperref}
\\usepackage{
  amsmath}
\\usepackage{
  amsfonts}
\\usepackage{
  mathtools}
\\usepackage[math-style=ISO]{
  unicode - math}
\\setmainfont{
  Source Han Serif TW}
\\setmathfont{
  DejaVu Math TeX Gyre}
\\setCJKmainfont{
  AR PL New Kai}
"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ))
  ;; 把預設的 fontenc 拿掉
  ;; 經過測試 XeLaTeX 輸出 PDF 時有 fontenc[T1]的話中文會無法顯示。
  ;; hyperref 也拿掉，改從 classes 處就插入，原因見上面 org-latex-with-hyperref 的說明。
  (setq org-latex-default-packages-alist
        '(("" "graphicx" t)
          ("" "longtable" nil)
          ("" "float" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "textcomp" t)
          ("" "marvosym" t)
          ("" "wasysym" t)
          ("" "multicol" t)  ; 這是我另外加的，因為常需要多欄位文件版面。
          "\\tolerance=1000"))
  ;; Use XeLaTeX to export PDF in Org-mode
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))
  ;; 指定你要用什麼外部 app 來開 pdf 之類的檔案。我是偷懶所以直接用 kde-open，你也可以指定其他的。
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . "xdg-open %s")
                        ("\\.pdf\\'" . "zathura %s")
                        ("\\.jpg\\'" . "zathura %s")))
  (use-package org-evil
    :ensure t
    :config
    (require 'org-evil)
    )
  (use-package ox-ioslide
    :ensure t
    :config
    (require 'ox-ioslide)
    )
  (defun org-insert-src-block (src-code-type)
    "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
    (interactive
     (let ((src-code-types
            '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
              "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
              "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
              "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
              "scheme" "sqlite")))
       (list (ido-completing-read "Source code type: " src-code-types))))
    (progn
      (newline-and-indent)
      (insert (format "#+BEGIN_SRC %s\n" src-code-type))
      (newline-and-indent)
      (insert "#+END_SRC\n")
      (previous-line 2)
      (org-edit-src-code)))
  (evil-leader/set-key-for-mode 'org-mode
    "oo" 'org-edit-src-code
    "oi" 'org-insert-src-block
    "ol" 'org-latex-export-to-latex
    "op" 'org-latex-export-to-pdf
    "p" 'org-toggle-latex-fragment
    )
  (setq org-src-fontify-natively t)
  (require 'ox-md)
  )
;; all-the-icons
(use-package all-the-icons
  :ensure t
  )
;; mode line
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  )
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode t)
  )
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode t)
  )

(use-package spaceline
  :ensure t
  :config
  (use-package spaceline-config
    :ensure spaceline
    :init
    (setq-default powerline-default-separator 'utf-8)
    (setq-default spaceline-window-numbers-unicode 'true)
    (setq-default spaceline-workspace-number-p 'true)
    (setq-default spaceline-workspace-numbers-unicode 'true)
    (setq-default spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (setq evil-normal-state-tag "N")
    (setq evil-insert-state-tag "I")
    (setq evil-visual-state-tag "V")
    (setq evil-replace-state-tag "R")
    (setq evil-emacs-state-tag "E")
    (setq evil-motion-state-tag "M")
    (setq evil-operator-state-tag "O")
    ;; (spaceline-compile)
    )
  (spaceline-spacemacs-theme)
  )
;; tree
(use-package neotree
  :ensure t
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  (evil-set-initial-state 'neotree-mode 'emacs)
  (evil-leader/set-key "f" 'neotree-toggle)
  )
;; theme
(use-package highlight-parentheses
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  )
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-set-bar 'left)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-set-modified-marker t)
  (setq centaur-tabs-modified-marker "\uf111")
  (centaur-tabs-enable-buffer-reordering)
  (centaur-tabs-inherit-tabbar-faces)
  (centaur-tabs-mode t)
  :bind
  ("M-<left>" . centaur-tabs-backward)
  ("M-<right>" . centaur-tabs-forward)
  (:map evil-normal-state-map
        ("g t" . centaur-tabs-forward)
        ("g T" . centaur-tabs-backward)
        )
  )
(use-package zoom
  :ensure t
  :config
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618)))
  (zoom-mode t)
  )
(use-package kaolin-themes
  :ensure t
  :config
  (setq kaolin-themes-distinct-company-scrollbar t)
  (setq kaolin-themes-underline-wave nil)
  (load-theme 'kaolin-bubblegum t)
  ;; (kaolin-treemacs-theme)
  )
(use-package highlight-numbers
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  )
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )
(use-package sublimity
  :ensure t
  :config
  (sublimity-mode 1)
  )
(global-set-key (kbd "C-S-M-SPC") 'set-mark-command)
(provide '.emacs)
;;; .emacs ends here
