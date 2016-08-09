(progn
  (cd "c:/Users/alastor/")
  (setq default-directory "C:/Users/alastor/" ))

(progn
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
  (package-initialize))

(progn
  (setq explicit-shell-file-name "C:/Program Files (x86)/Git/bin/bash.exe")
  (setq shell-file-name "bash")
  (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
  (setenv "SHELL" shell-file-name)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m))

(progn
  (setq eshell-cmpl-cycle-completions nil)

  (defun eshell-mode-hook-fun ()
    (local-set-key (kbd "<up>") 'previous-line)
    (local-set-key (kbd "<down>") 'next-line))

  (add-hook 'eshell-mode-hook 'eshell-mode-hook-fun))

(progn
  (setq doc-view-ghostscript-program "gswin32c"))

(progn
  (require 'neotree)
  (global-set-key (kbd "C-x C-l") 'neotree-toggle))

(progn
  ;; frame- or window-resizing function
  ;; from http://dse.livejournal.com/67732.html. Resizes either frame or window
  ;; to 81 columns. If the window can be sized to 81 columns wide, without
  ;; resizing the frame itself, it will resize the window. Otherwise, it will
  ;; resize the frame. You can use a prefix argument to specify a
  ;; different column width
  (defun fix-frame-horizontal-size (width)
    "Set the frame's size to 81 (or prefix arg WIDTH) columns wide."
    (interactive "P")
    (if window-system
	(set-frame-width (selected-frame) (or width 81))
      (error "Cannot resize frame horizontally: is a text terminal")))

  (defun fix-window-horizontal-size (width)
    "Set the window's size to 81 (or prefix arg WIDTH) columns wide."
    (interactive "P")
    (enlarge-window (- (or width 81) (window-width)) 'horizontal))

  (defun fix-window-and-frame-horizontal-size (width)
    (set-frame-width (selected-frame) (+ (frame-width)
					 (- (or width 81)
					    (window-width))))
    (enlarge-window (- (or width 81) (window-width)) 'horizontal))

  (defun fix-horizontal-size (width)
    "Set the window's or frame's width to 81 (or prefix arg WIDTH)."
    (interactive "P")
    (condition-case nil
	(fix-window-and-frame-horizontal-size width)
      (error
       (condition-case nil
	   (fix-window-horizontal-size width)
	 (error
	  (condition-case nil
	      (fix-frame-horizontal-size width)
	    (error
	     (error "Cannot resize window or frame horizontally"))))))))

  (global-set-key (kbd "C-c C-<return>") 'fix-horizontal-size))

(progn
  (global-set-key (kbd "C-c C-SPC") 'whitespace-mode))

(progn
  (smart-tabs-insinuate 'c 'java)

  (setq-default indent-tabs-mode nil)

  (defun c-mode-common-hook-fun ()
    (setq indent-tabs-mode t))

  (add-hook 'c-mode-common-hook 'c-mode-common-hook-fun))

(progn
  (defun java-mode-hook-fun ()
    (setq c-basic-offset 4
	  tab-width 4
	  indent-tabs-mode t)
    (c-set-offset 'inexpr-class 0)
    (c-set-offset 'substatement-open 0))

  (add-hook 'java-mode-hook 'java-mode-hook-fun))

(progn
  (setq default-buffer-file-coding-system 'utf-8-unix))

(progn
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(progn
  (global-set-key (kbd "C-x g") 'magit-status))
