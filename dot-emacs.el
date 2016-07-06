; Load Common Lisp standard library
(progn
  (require 'cl))

; Install packages
(progn
  (require 'package)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)

  (let* ((packages
          '(pabbrev		  ; Auto complete files with tab
            org			  ; Create todo lists
            fill-column-indicator ; Adds a column to show where 80 chars is
            smart-tabs-mode	  ; Use tabs and spaces correctly
            frame-cmds            ; Commands to work with frames
            pdf-tools             ; Open PDF in Emacs
            multi-web-mode        ; Edit all-in-one HTML files
            lua-mode              ; Edit Lua files
            helm-lobsters         ; View lobsters in emacs
            js2-mode              ; Better javascript major mode?
            vline                 ; Show a vertical line where cursor is
            helm-dash             ; Browse documentation in Emacs
            f                     ; Advanced file manipulation
            markdown-mode         ; Markdown major mode
            markdown-mode+        ; More features for markdown-mode?
            web-mode              ; Better web-mode?
            ))
         (packages (remove-if 'package-installed-p packages)))
    (when packages
      (ignore-errors (package-refresh-contents)
                     (mapcar 'package-install packages)))))

; Fix defaults for Mac OS
(when (memq window-system '(mac ns))
  (setq ns-pop-up-frames nil
        mac-option-modifier 'meta
        mac-command-modifier 'meta
        x-select-enable-clipboard t)
  (when (boundp 'mac-pass-command-to-system)
    (setq mac-pass-command-to-system nil)))

; Sane defaults
(progn
  (setq inhibit-startup-message t ; Hide the startup message
        initial-scratch-message nil ; Empty scratch buffer
        ring-bell-function 'ignore  ; Don't ring the bell
        echo-keystrokes 0.1	    ; Instantly echo keystrokes
        mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; scroll 1 line at a time
        mouse-wheel-progressive-speed nil ; no acceleration
        mouse-wheel-follow-mouse 't ; scroll window under mouse
        scroll-step 1 ; keyboard scroll one at a time
        doc-view-ghostscript-program "/usr/local/bin/gs" ; DocView needs this
        doc-view-pdftotext-program "/usr/local/bin/pdftotext" ; And this
        tab-width 2)                ; I hate those big tabs, so wasteful


  (setq-default fill-column 80 	  ; Default fill column
                indent-tabs-mode nil) ; Use spaces instead of tabs

  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'scroll-left 'disabled nil)
  (put 'scroll-right 'disabled nil)

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (set-language-environment "UTF-8")) ; Use UTF-8 as preferred coding system

; Disabled useless modes
(dolist (mode
	 '(tool-bar-mode 	  ; No toolbars
	   scroll-bar-mode))	  ; No scroll bars
  (funcall mode 0))

; Enable used modes
(dolist (mode
         '(pabbrev-mode		  ; Simple auto-completion
           column-number-mode	  ; Show column number in mode line
           global-prettify-symbols-mode ; Make greek letters look nice
           show-paren-mode		; Highlight matching parentheses
           global-pabbrev-mode))	; Enable pabbrev mode
  (funcall mode 1))

; Prettify
(progn
  (setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                         ("delta" . ?Δ)
                                         ("gamma" . ?Γ)
                                         ("phi" . ?φ)
                                         ("psi" . ?ψ)
                                         ("function" . ?λ)))
  (set-face-attribute 'default nil :height 125))

; Setup fill column indicator
(progn
  (require 'fill-column-indicator)
  (setq fci-rule-column fill-column)
  (dolist (major-mode
           '(c-mode-hook
             emacs-lisp-mode-hook
             python-mode-hook))
    (add-hook major-mode 'fci-mode)))

; Setup multi-web-mode
(progn
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags
        '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
          (js2-mode  "<script[^>]*>" "</script>")
          (html-mode "<script[^>]*type=\"text/x-template\"[^>]*>" "</script>")
          (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions
        '("php" "htm" "html" "ctp" "phtml" "php4" "php5" "vue"))
  (multi-web-global-mode 0))

; Setup web-mode
(progn
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

  (setq web-mode-markup-indent-offset 4
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-block-padding 0
        web-mode-code-indent-offset 4
        web-mode-enable-control-block-indentation nil
        web-mode-enable-whitespace-fontification nil
        )

  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))


; Setup js2-mode
(progn
  (setq js2-basic-offset 2              ; I don't like deep indentations
        js2-bound-indent-p t            ; Bounce between indentations
        js2-highlight-external-variables nil ; Don't highlight globals
        js2-missing-semi-one-line-override t ; Missing semi-colons is 'kay
        js2-mode-indent-ignore-first-tab t   ; Ignore first tab if indented correctly
        js2-strict-trailing-comma-warning nil)) ; So annoying

; Setup js-mode
(progn
  (setq js-indent-level 0
        js-curly-indent-offset 2
        js-expr-indent-offset 4
        js-paren-indent-offset 2
        js-square-indent-offset 2
        js-switch-indent-offset 2))

; Setup helm-dash
(progn
  (setq helm-dash-common-docsets
        '("JavaScript" "HTML" "jQuery" "Bootstrap 3" "CSS" "jQuery" "D3JS")))

; Setup pabbrev-mode
(progn
  (setq pabbrev-idle-timer-verbose nil))

; Toggle maximize buffer
(progn
  (defun toggle-maximize-buffer () "Maximize buffer"
         (interactive)
         (if (= 1 (length (window-list)))
             (jump-to-register '_)
           (progn
             (window-configuration-to-register '_)
             (delete-other-windows)))))

; Remove trailing whitespace on save
(progn
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

; Fix terminal size when window size changes
(progn
  (defun comint-fix-window-size ()
    "Change process window size."
    (when (derived-mode-p 'comint-mode)
      (let ((process (get-buffer-process (current-buffer))))
        (when process
          (set-process-window-size process (window-height) (window-width))))))

  (defun comint-fix-window-size-hook ()
    (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t)
    (pabbrev-mode -1))

  (add-hook 'shell-mode-hook 'comint-fix-window-size-hook))

(progn
  (defun fix-frame-position ()
    (interactive)
    (move-frame-to-screen-right 0 (window-frame))
    (move-frame-to-screen-top 0 (window-frame))))

;; frame- or window-resizing function
;; from http://dse.livejournal.com/67732.html. Resizes either frame or window
;; to 80 columns. If the window can be sized to 80 columns wide, without
;; resizing the frame itself, it will resize the window. Otherwise, it will
;; resize the frame. You can use a prefix argument to specify a
;; different column width
(progn
  (defun fix-frame-horizontal-size (width)
    "Set the frame's size to 80 (or prefix arg WIDTH) columns wide."
    (interactive "P")
    (if window-system
        (set-frame-width (selected-frame)
                         (+ (frame-width)
                            (- (or width (1+ fill-column)) (window-width))))
      (error "Cannot resize frame horizontally: is a text terminal")))

  (defun fix-window-horizontal-size (width)
    "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
    (interactive "P")
    (enlarge-window (- (or width (1+ fill-column)) (window-width)) 'horizontal))

  (defun fix-frame-size (width)
    "Set the window's or frame's width to 80 (or prefix arg WIDTH)."
    (interactive "P")
    (condition-case nil
        (progn
          (fix-frame-horizontal-size width)
          (fix-window-horizontal-size width))
      (error "Cannot resize window or frame horizontally"))))

(progn
  (defun fix-frame-size-and-position (width)
    (interactive "P")
    (fix-frame-size width)
    (fix-frame-position)))

(progn
  (defun doc-view-add-keybindings ()
    (setq doc-view-continuous t))

  (add-hook 'doc-view-mode-hook 'doc-view-add-keybindings))

; Open all files in current directory
(progn
  (defun open-all-files-in-directory (prefix)
    (interactive "P")
    (defun kill-file (fname)
      (find-file fname)
      (kill-this-buffer))
    (let ((command "git ls-tree -r --name-only HEAD | xargs /usr/local/bin/grealpath"))
      (mapc (if prefix 'kill-file 'find-file)
            (split-string (eshell-command-result command))))))

; Key bindings
(progn
  (defvar custom-bindings-map (make-keymap))
  (define-key custom-bindings-map (kbd "C-x 9") 'toggle-maximize-buffer)
  (define-key custom-bindings-map (kbd "C-c C-SPC")
    'fix-frame-size)
  (define-key custom-bindings-map (kbd "<wheel-left>")
    (lambda () (interactive) (scroll-right 1)))
  (define-key custom-bindings-map (kbd "<wheel-right>")
    (lambda () (interactive) (scroll-left 1)))
  (define-key custom-bindings-map (kbd "M-v") 'yank)
  (define-key custom-bindings-map (kbd "M-c") 'kill-ring-save)
  (define-key custom-bindings-map (kbd "C-x C-l") 'helm-lobsters)
  (define-key custom-bindings-map (kbd "C-x <C-return>") 'vline-mode)
  (define-key custom-bindings-map (kbd "C-x C-\\") 'open-all-files-in-directory)

  (define-minor-mode custom-bindings-mode
    "A mode that activates custom-bindings"
    t nil custom-bindings-map)

  (custom-bindings-mode 1))

(progn
  (set-frame-height (window-frame) 50)
  (fix-frame-size nil)
  (shell))
