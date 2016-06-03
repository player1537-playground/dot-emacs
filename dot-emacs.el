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
        doc-view-pdftotext-program "/usr/local/bin/pdftotext") ; And this


  (setq-default fill-column 80 	  ; Default fill column
		indent-tabs-mode nil ; Use spaces instead of tabs
		)

  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'scroll-left 'disabled nil)
  (put 'scroll-right 'disabled nil)

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
          (js-mode  "<script[^>]*>" "</script>")
          (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions
        '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
  (multi-web-global-mode 1))

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

  (add-hook 'shell-mode-hook 'comint-fix-window-size-hook)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

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

  (defun fix-horizontal-size (width)
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
    (fix-horizontal-size width)
    (fix-frame-position)))

(progn
  (defun doc-view-add-keybindings ()
    (setq doc-view-continuous t))

  (add-hook 'doc-view-mode-hook 'doc-view-add-keybindings))

; Key bindings
(progn
  (defvar custom-bindings-map (make-keymap))
  (define-key custom-bindings-map (kbd "C-x 9") 'toggle-maximize-buffer)
  (define-key custom-bindings-map (kbd "C-c C-SPC")
    'fix-frame-size-and-position)
  (define-key custom-bindings-map (kbd "<wheel-left>")
    (lambda () (interactive) (scroll-right 1)))
  (define-key custom-bindings-map (kbd "<wheel-right>")
    (lambda () (interactive) (scroll-left 1)))
  (define-key custom-bindings-map (kbd "M-v") 'yank)
  (define-key custom-bindings-map (kbd "M-c") 'kill-ring-save)

  (define-minor-mode custom-bindings-mode
    "A mode that activates custom-bindings"
    t nil custom-bindings-map)

  (custom-bindings-mode 1))

(progn
  (set-frame-height (window-frame) 50)
  (fix-frame-size-and-position nil)
  (shell))
