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
          '(org			  ; Create todo lists
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
            yasnippet             ; Snippets
            emmet-mode            ; Easily insert HTML and CSS
            auto-complete         ; Better auto-complete?
            ox-gfm                ; Export github-flavored markdown
            yaml-mode             ; Edit YAML configuration files
            ledger-mode           ; Keep track of finances
            s                     ; Helpful string functions
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
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  (set-language-environment "UTF-8")) ; Use UTF-8 as preferred coding system

; Disabled useless modes
(dolist (mode
	 '(tool-bar-mode 	  ; No toolbars
	   scroll-bar-mode))	  ; No scroll bars
  (funcall mode 0))

;; Enable used modes
(progn
  (dolist (mode
           '(column-number-mode	  ; Show column number in mode line
             global-prettify-symbols-mode ; Make greek letters look nice
             show-paren-mode		; Highlight matching parentheses
             global-auto-revert-mode      ; revert files when they change
             ))
    (funcall mode 1)))

; Prettify
(progn
  (set-face-attribute 'default nil :height (cond ((eq system-type 'gnu/linux) 100)
                                                 ((eq system-type 'darwin) 115))))

; Docker stuff
(progn
  (add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . sh-mode)))

; Setup sh-mode
(progn
  (defun disable-here-mode-completion ()
    (sh-electric-here-document-mode -1))

  (add-hook 'sh-mode-hook #'disable-here-mode-completion))

; Setup org-mode
(progn
  (require 's)

  (defun my/org-add-ids-to-all ()
    (interactive)
    (org-map-entries 'org-id-get-create)
    (org-map-entries
     '(unless (org-entry-get nil "CUSTOM_ID")
        (org-entry-put nil "CUSTOM_ID" (org-id-get nil)))))

  (defun my/org-hide-etc ()
    (interactive)
    (save-excursion
      (org-cycle '(64))                   ; show everything including drawers
      (org-hide-block-all)                ; hide code blocks
      (org-map-entries #'(lambda () (org-cycle-hide-drawers t))) ; hide drawers
      (goto-char (point-min))                               ; goto top
      (while (re-search-forward "^ +- http" nil t)          ; search for lists
        (org-cycle))))                                       ; collapse list

  (defun my/org-show-next-heading-tidily ()
    "Show next entry, keeping other entries closed."
    (interactive)
    (if (save-excursion (end-of-line) (outline-invisible-p))
        (progn (org-show-entry) (show-children))
      (outline-next-heading)
      (unless (and (bolp) (org-on-heading-p))
        (org-up-heading-safe)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)
      (recenter-top-bottom 0))
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap (kbd "p") #'my/org-show-previous-heading-tidily)
      (define-key keymap (kbd "n") #'my/org-show-next-heading-tidily)
      (set-transient-map keymap t)))

  (defun my/org-show-previous-heading-tidily ()
    "Show previous entry, keeping other entries closed."
    (interactive)
    (let ((pos (point)))
      (outline-previous-heading)
      (unless (and (< (point) pos) (bolp) (org-on-heading-p))
        (goto-char pos)
        (hide-subtree)
        (error "Boundary reached"))
      (org-overview)
      (org-reveal t)
      (org-show-entry)
      (show-children)
      (recenter-top-bottom 0))
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap (kbd "p") #'my/org-show-previous-heading-tidily)
      (define-key keymap (kbd "n") #'my/org-show-next-heading-tidily)
      (set-transient-map keymap t)))

  (defun my/www-get-page-title (url)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
      (match-string 1)))

  (defun my/org-get-link ()
    "Insert org link where default description is set to html title."
    (interactive)
    (let* ((url (read-string "URL: "))
           (title (my/www-get-page-title url)))
      (concat "[[" url "][" title "]]")))

  ;; (defun my/org-open-at-point-save-excursion (&optional arg reference-buffer)
  ;;   (interactive "P")
  ;;   (save-excursion
  ;;     (org-open-at-point arg reference-buffer)))

  (defun my/org-html-format-drawer-function (name content)
      (concat "<div class=\"drawer " (downcase name) "\">\n"
              "<h6>" (capitalize name) "</h6>\n"
              (let* ((lines (s-split "\n" content t))
                     (props (mapcar (lambda (s) (s-split-up-to ": " s 1)) lines))
                     result)
                (dolist (prop props result)
                  (let ((name (car prop))
                        (value (cadr prop)))
                    (if (or (s-equals? name "LINK") (s-equals? name "BLOCKER"))
                        (let* ((re (rx "[[" (group (+? anything)) "]"
                                       "[" (group (+? anything)) "]]"))
                               (matches (s-match-strings-all re value)))
                          (setq
                           result
                           (s-concat
                            result
                            (s-wrap
                             (mapconcat
                              (lambda (match)
                                (let* ((href (cadr match))
                                       (title (or (caddr match) href))
                                       (a-tag (s-lex-format "<a href=\"${href}\">${title}</a>")))
                                  a-tag))
                              matches
                              "</li><li>")
                             "<ul><li>"
                             "</li></ul>"))))
                      (setq result (s-concat result "<pre>" name ": " value "</pre>"))))))
              "\n</div>"))

  (defun org-html-property-drawer (property-drawer contents info)
      "Transcode a PROPERTY-DRAWER element from Org to HTML.
  CONTENTS holds the contents of the drawer.  INFO is a plist
  holding contextual information."
      (and (org-string-nw-p contents)
           (my/org-html-format-drawer-function "properties" contents)))

  (defun my/org-entry-sort-tags ()
    (interactive)
    (let ((original (org-entry-get nil "TAGS")))
      (when original
        (let* ((split (s-split ":" original 'omit-nulls))
               (sorted (sort split #'s-less?))
               (reversed (reverse sorted)))
          (org-set-tags-to reversed)))))

  (defun my/org-all-entries-sort-tags ()
    (interactive)
    (org-map-entries #'my/org-entry-sort-tags))

  (defun my/org-entry-fix-link-description ()
    (interactive)
    (let ((level (length
                  (car (s-match
                        (rx line-start (one-or-more "*"))
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))))))
      (when (= 2 level)
        (with-demoted-errors "Error: %s"
          (let* ((link-value (org-entry-get nil "LINK"))
                 (re (rx "[[" (group (+? anything)) "]"
                         "[" (group (+? anything)) "]]"))
                 (match (s-match re link-value))
                 (href (cadr match))
                 (title (caddr match))
                 (real-title (www-get-page-title href))
                 (real-link-value (s-concat "[[" href "][" real-title "]]")))
            (org-entry-delete nil "LINK")
            (org-entry-put nil "LINK" real-link-value))))))

  (defun my/org-entry-fix-tags ()
    (interactive)
    (ignore-errors
      (let* ((lookup '(("official" . "@official")
                       ("stackoverflow" . "@stackoverflow")
                       ("mailinglist" . "@mailinglist")
                       ("blog" . "@blog")
                       ("github" . "@github")
                       ("emacswiki" . "@emacswiki")
                       ("reddit" . "@reddit")))
             (tags (org-entry-get nil "TAGS"))
             (split (s-split ":" tags t))
             (fixed (mapcar
                     (lambda (tag) (or (cdr-safe (assoc tag lookup)) tag))
                     split)))
        (org-set-tags-to fixed))))

  (defun my/org-all-entries-fix-tags ()
    (interactive)
    (org-map-entries #'my/org-entry-fix-tags))

  (defun my/org-fix-internal-link-description ()
    (interactive)
    (save-excursion
      (let (end)
        (beginning-of-buffer)
        (while (setq end (re-search-forward
                          (rx "["
                              "[#" (+? anything) "]"
                              (opt "[" (+? anything) "]")
                              "]")
                          nil
                          t))
          (let* ((start (re-search-forward (rx "[[") nil nil -1))
                 (string (buffer-substring-no-properties start end))
                 (match (s-with string
                          (s-match
                           (rx "["
                               "[#" (group (+? anything)) "]"
                               (opt "[" (+? anything) "]")
                               "]"))
                          cadr))
                 (headline (org-entry-get (org-find-entry-with-id match) "ITEM"))
                 (title (cadr (s-split-up-to (rx (+ "*") (+ space)) headline 1)))
                 (new-link (s-concat "[[#" match "][" title "]]")))
            (re-search-forward (rx "["
                                   "[" (+? anything) "]"
                                   (opt "[" (+? anything) "]")
                                   "]"))
            (replace-match new-link 'fixedcase 'literal))))))

  ;;; org-depend.el --- TODO dependencies for Org-mode
  ;; Copyright (C) 2008-2012 Free Software Foundation, Inc.
  ;;
  ;; Author: Carsten Dominik <carsten at orgmode dot org>
  ;; Keywords: outlines, hypermedia, calendar, wp
  ;; Homepage: http://orgmode.org
  ;; Version: 0.08
  ;;
  ;; This file is not part of GNU Emacs.
  ;;
  ;; This file is free software; you can redistribute it and/or modify
  ;; it under the terms of the GNU General Public License as published by
  ;; the Free Software Foundation; either version 3, or (at your option)
  ;; any later version.

  ;; GNU Emacs is distributed in the hope that it will be useful,
  ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
  ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  ;; GNU General Public License for more details.

  ;; You should have received a copy of the GNU General Public License
  ;; along with GNU Emacs; see the file COPYING.  If not, write to the
  ;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
  ;; Boston, MA 02110-1301, USA.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;;; Commentary:
  ;;
  ;; WARNING: This file is just a PROOF OF CONCEPT, not a supported part
  ;;          of Org-mode.
  ;;
  ;; This is an example implementation of TODO dependencies in Org-mode.
  ;; It uses the new hooks in version 5.13 of Org-mode,
  ;; `org-trigger-hook' and `org-blocker-hook'.
  ;;
  ;; It implements the following:
  ;;
  ;; Triggering
  ;; ----------
  ;;
  ;; 1) If an entry contains a TRIGGER property that contains the string
  ;;    "chain-siblings(KEYWORD)", then switching that entry to DONE does
  ;;    do the following:
  ;;    - The sibling following this entry switched to todo-state KEYWORD.
  ;;    - The sibling also gets a TRIGGER property "chain-sibling(KEYWORD)",
  ;;      property, to make sure that, when *it* is DONE, the chain will
  ;;      continue.
  ;;
  ;; 2) If an entry contains a TRIGGER property that contains the string
  ;;    "chain-siblings-scheduled", then switching that entry to DONE does
  ;;    the following actions, similarly to "chain-siblings(KEYWORD)":
  ;;    - The sibling receives the same scheduled time as the entry
  ;;      marked as DONE (or, in the case, in which there is no scheduled
  ;;      time, the sibling does not get any either).
  ;;    - The sibling also gets the same TRIGGER property
  ;;      "chain-siblings-scheduled", so the chain can continue.
  ;;
  ;; 3) If the TRIGGER property contains the string
  ;;    "chain-find-next(KEYWORD[,OPTIONS])", then switching that entry
  ;;    to DONE do the following:
  ;;    - All siblings are of the entry are collected into a temporary
  ;;      list and then filtered and sorted according to OPTIONS
  ;;    - The first sibling on the list is changed into KEYWORD state
  ;;    - The sibling also gets the same TRIGGER property
  ;;      "chain-find-next", so the chain can continue.
  ;;
  ;;    OPTIONS should be a comma separated string without spaces, and
  ;;    can contain following options:
  ;;
  ;;    - from-top      the candidate list is all of the siblings in
  ;;                    the current subtree
  ;;
  ;;    - from-bottom   candidate list are all siblings from bottom up
  ;;
  ;;    - from-current  candidate list are all siblings from current item
  ;;                    until end of subtree, then wrapped around from
  ;;                    first sibling
  ;;
  ;;    - no-wrap       candidate list are siblings from current one down
  ;;
  ;;    - todo-only     Only consider siblings that have a todo keyword
  ;;    -
  ;;    - todo-and-done-only
  ;;                    Same as above but also include done items.
  ;;
  ;;    - priority-up   sort by highest priority
  ;;    - priority-down sort by lowest priority
  ;;    - effort-up     sort by highest effort
  ;;    - effort-down   sort by lowest effort
  ;;
  ;;    Default OPTIONS are from-top
  ;;
  ;;
  ;; 4) If the TRIGGER property contains any other words like
  ;;    XYZ(KEYWORD), these are treated as entry id's with keywords.  That
  ;;    means Org-mode will search for an entry with the ID property XYZ
  ;;    and switch that entry to KEYWORD as well.
  ;;
  ;; Blocking
  ;; --------
  ;;
  ;; 1) If an entry contains a BLOCKER property that contains the word
  ;;    "previous-sibling", the sibling above the current entry is
  ;;    checked when you try to mark it DONE.  If it is still in a TODO
  ;;    state, the current state change is blocked.
  ;;
  ;; 2) If the BLOCKER property contains any other words, these are
  ;;    treated as entry id's.  That means Org-mode will search for an
  ;;    entry with the ID property exactly equal to this word.  If any
  ;;    of these entries is not yet marked DONE, the current state change
  ;;    will be blocked.
  ;;
  ;; 3) Whenever a state change is blocked, an org-mark is pushed, so that
  ;;    you can find the offending entry with `C-c &'.
  ;;
;;; Example:
  ;;
  ;; When trying this example, make sure that the settings for TODO keywords
  ;; have been activated, i.e. include the following line and press C-c C-c
  ;; on the line before working with the example:
  ;;
  ;; #+TYP_TODO: TODO NEXT | DONE
  ;;
  ;; * TODO Win a million in Las Vegas
  ;;   The "third" TODO (see above) cannot become a TODO without this money.
  ;;
  ;;   :PROPERTIES:
  ;;     :ID: I-cannot-do-it-without-money
  ;;   :END:
  ;;
  ;; * Do this by doing a chain of TODO's
  ;; ** NEXT This is the first in this chain
  ;;    :PROPERTIES:
  ;;      :TRIGGER: chain-siblings(NEXT)
  ;;    :END:
  ;;
  ;; ** This is the second in this chain
  ;;
  ;; ** This is the third in this chain
  ;;    :PROPERTIES:
  ;;      :BLOCKER: I-cannot-do-it-without-money
  ;;    :END:
  ;;
  ;; ** This is the forth in this chain
  ;;    When this is DONE, we will also trigger entry XYZ-is-my-id
  ;;   :PROPERTIES:
  ;;     :TRIGGER: XYZ-is-my-id(TODO)
  ;;   :END:
  ;;
  ;; ** This is the fifth in this chain
  ;;
  ;; * Start writing report
  ;;   :PROPERTIES:
  ;;     :ID: XYZ-is-my-id
  ;;   :END:
  ;;
  ;;

  (require 'org)
  (eval-when-compile
    (require 'cl))

  (defcustom org-depend-tag-blocked t
    "Whether to indicate blocked TODO items by a special tag."
    :group 'org
    :type 'boolean)

  (defcustom org-depend-find-next-options
    "from-current,todo-only,priority-up"
    "Default options for chain-find-next trigger"
    :group 'org
    :type 'string)

  (defmacro org-depend-act-on-sibling (trigger-val &rest rest)
    "Perform a set of actions on the next sibling, if it exists,
copying the sibling spec TRIGGER-VAL to the next sibling."
    `(catch 'exit
       (save-excursion
         (goto-char pos)
         ;; find the sibling, exit if no more siblings
         (condition-case nil
             (outline-forward-same-level 1)
           (error (throw 'exit t)))
         ;; mark the sibling TODO
         ,@rest
         ;; make sure the sibling will continue the chain
         (org-entry-add-to-multivalued-property
          nil "TRIGGER" ,trigger-val))))

  (defvar org-depend-doing-chain-find-next nil)

  (defun org-depend-trigger-todo (change-plist)
    "Trigger new TODO entries after the current is switched to DONE.
This does two different kinds of triggers:
- If the current entry contains a TRIGGER property that contains
  \"chain-siblings(KEYWORD)\", it goes to the next sibling, marks it
  KEYWORD and also installs the \"chain-sibling\" trigger to continue
  the chain.
- If the current entry contains a TRIGGER property that contains
  \"chain-siblings-scheduled\", we go to the next sibling and copy
  the scheduled time from the current task, also installing the property
  in the sibling.
- Any other word (space-separated) like XYZ(KEYWORD) in the TRIGGER
  property is seen as an entry id.  Org-mode finds the entry with the
  corresponding ID property and switches it to the state TODO as well."

    ;; Get information from the plist
    (let* ((type (plist-get change-plist :type))
           (pos (plist-get change-plist :position))
           (from (plist-get change-plist :from))
           (to (plist-get change-plist :to))
           (org-log-done nil) ; IMPROTANT!: no logging during automatic trigger!
           trigger triggers tr p1 kwd)
      (catch 'return
        (unless (eq type 'todo-state-change)
          ;; We are only handling todo-state-change....
          (throw 'return t))
        (unless (and (member from org-not-done-keywords)
                     (member to org-done-keywords))
          ;; This is not a change from TODO to DONE, ignore it
          (throw 'return t))

        ;; OK, we just switched from a TODO state to a DONE state
        ;; Lets see if this entry has a TRIGGER property.
        ;; If yes, split it up on whitespace.
        (setq trigger (org-entry-get pos "TRIGGER")
              triggers (and trigger (org-split-string trigger "[ \t]+")))

        ;; Go through all the triggers
        (while (setq tr (pop triggers))
          (cond
           ((and (not org-depend-doing-chain-find-next)
                 (string-match "\\`chain-find-next(\\b\\(.+?\\)\\b\\(.*\\))\\'" tr))
            ;; smarter sibling selection
            (let* ((org-depend-doing-chain-find-next t)
                   (kwd (match-string 1 tr))
                   (options (match-string 2 tr))
                   (options (if (or (null options)
                                    (equal options ""))
                                org-depend-find-next-options
                              options))
                   (todo-only (string-match "todo-only" options))
                   (todo-and-done-only (string-match "todo-and-done-only"
                                                     options))
                   (from-top (string-match "from-top" options))
                   (from-bottom (string-match "from-bottom" options))
                   (from-current (string-match "from-current" options))
                   (no-wrap (string-match "no-wrap" options))
                   (priority-up (string-match "priority-up" options))
                   (priority-down (string-match "priority-down" options))
                   (effort-up (string-match "effort-up" options))
                   (effort-down (string-match "effort-down" options)))
              (save-excursion
                (org-back-to-heading t)
                (let ((this-item (point)))
                  ;; go up to the parent headline, then advance to next child
                  (org-up-heading-safe)
                  (let ((end (save-excursion (org-end-of-subtree t)
                                             (point)))
                        (done nil)
                        (items '()))
                    (outline-next-heading)
                    (while (not done)
                      (if (not (looking-at org-complex-heading-regexp))
                          (setq done t)
                        (let ((todo-kwd (match-string 2))
                              (tags (match-string 5))
                              (priority (org-get-priority (or (match-string 3) "")))
                              (effort (when (or effort-up effort-down)
                                        (let ((effort (org-get-effort)))
                                          (when effort
                                            (org-duration-string-to-minutes effort))))))
                          (push (list (point) todo-kwd priority tags effort)
                                items))
                        (unless (org-goto-sibling)
                          (setq done t))))
                    ;; massage the list according to options
                    (setq items
                          (cond (from-top (nreverse items))
                                (from-bottom items)
                                ((or from-current no-wrap)
                                 (let* ((items (nreverse items))
                                        (pos (position this-item items :key #'first))
                                        (items-before (subseq items 0 pos))
                                        (items-after (subseq items pos)))
                                   (if no-wrap items-after
                                     (append items-after items-before))))
                                (t (nreverse items))))
                    (setq items (remove-if
                                 (lambda (item)
                                   (or (equal (first item) this-item)
                                       (and (not todo-and-done-only)
                                            (member (second item) org-done-keywords))
                                       (and (or todo-only
                                                todo-and-done-only)
                                            (null (second item)))))
                                 items))
                    (setq items
                          (sort
                           items
                           (lambda (item1 item2)
                             (let* ((p1 (third item1))
                                    (p2 (third item2))
                                    (e1 (fifth item1))
                                    (e2 (fifth item2))
                                    (p1-lt (< p1 p2))
                                    (p1-gt (> p1 p2))
                                    (e1-lt (and e1 (or (not e2) (< e1 e2))))
                                    (e2-gt (and e2 (or (not e1) (> e1 e2)))))
                               (cond (priority-up
                                      (or p1-gt
                                          (and (equal p1 p2)
                                               (or (and effort-up e1-gt)
                                                   (and effort-down e1-lt)))))
                                     (priority-down
                                      (or p1-lt
                                          (and (equal p1 p2)
                                               (or (and effort-up e1-gt)
                                                   (and effort-down e1-lt)))))
                                     (effort-up
                                      (or e1-gt (and (equal e1 e2) p1-gt)))
                                     (effort-down
                                      (or e1-lt (and (equal e1 e2) p1-gt))))))))
                    (when items
                      (goto-char (first (first items)))
                      (org-entry-add-to-multivalued-property nil "TRIGGER" tr)
                      (org-todo kwd)))))))
           ((string-match "\\`chain-siblings(\\(.*?\\))\\'" tr)
            ;; This is a TODO chain of siblings
            (setq kwd (match-string 1 tr))
            (org-depend-act-on-sibling (format "chain-siblings(%s)" kwd)
                                       (org-todo kwd)))
           ((string-match "\\`\\(\\S-+\\)(\\(.*?\\))\\'" tr)
            ;; This seems to be ENTRY_ID(KEYWORD)
            (setq id (match-string 1 tr)
                  kwd (match-string 2 tr)
                  p1 (org-find-entry-with-id id))
            (when p1
              ;; there is an entry with this ID, mark it TODO
              (save-excursion
                (goto-char p1)
                (org-todo kwd))))
           ((string-match "\\`chain-siblings-scheduled\\'" tr)
            (let ((time (org-get-scheduled-time pos)))
              (when time
                (org-depend-act-on-sibling
                 "chain-siblings-scheduled"
                 (org-schedule nil time))))))))))

  (defun org-depend-block-todo (change-plist)
    "Block turning an entry into a TODO.
This checks for a BLOCKER property in an entry and checks
all the entries listed there.  If any of them is not done,
block changing the current entry into a TODO entry.  If the property contains
the word \"previous-sibling\", the sibling above the current entry is checked.
Any other words are treated as entry id's. If an entry exists with the
this ID property, that entry is also checked."
    ;; Get information from the plist
    (let* ((type (plist-get change-plist :type))
           (pos (plist-get change-plist :position))
           (from (plist-get change-plist :from))
           (to (plist-get change-plist :to))
           (org-log-done nil) ; IMPROTANT!: no logging during automatic trigger
           blocker blockers bl p1
           (proceed-p
            (catch 'return
              ;; If this is not a todo state change, or if this entry is
              ;; DONE, do not block
              (when (or (not (eq type 'todo-state-change))
                        (member from (cons 'done org-done-keywords))
                        (member to (cons 'todo org-not-done-keywords))
                        (not to))
                (throw 'return t))

              ;; OK, the plan is to switch from nothing to TODO
              ;; Lets see if we will allow it.  Find the BLOCKER property
              ;; and split it on whitespace.
              (setq blocker (org-entry-get pos "BLOCKER")
                    blockers (and blocker (mapcar
                                           #'car
                                           (s-match-strings-all
                                            (rx "[["
                                                (+? anything)
                                                "]"
                                                (opt "[" (+? anything) "]")
                                                "]")
                                            blocker))))


              ;; go through all the blockers
              (while (setq bl (message (pop blockers)))
                (cond
                 ((equal bl "previous-sibling")
                  ;; the sibling is required to be DONE.
                  (catch 'ignore
                    (save-excursion
                      (goto-char pos)
                      ;; find the older sibling, exit if no more siblings
                      (condition-case nil
                          (outline-backward-same-level 1)
                        (error (throw 'ignore t)))
                      ;; check if this entry is not yet done and block
                      (unless (org-entry-is-done-p)
                        ;; return nil, to indicate that we block the change!
                        (org-mark-ring-push)
                        (throw 'return nil)))))
                 ((setq p1 (or (org-find-entry-with-id bl)
                               (org-find-entry-with-id
                                (s-with bl
                                  (s-match (rx "[[#"
                                               (group (+? anything))
                                               "]"
                                               (opt "[" (+? anything) "]")
                                               "]"))
                                  cadr))))
                  ;; there is an entry with this ID, check it out
                  (save-excursion
                    (goto-char p1)
                    (unless (org-entry-is-done-p)
                      ;; return nil, to indicate that we block the change!
                      (org-mark-ring-push)
                      (throw 'return nil))))))
              t ; return t to indicate that we are not blocking
              )))
      (when org-depend-tag-blocked
        (org-toggle-tag "blocked" (if proceed-p 'off 'on)))

      proceed-p))

  (add-hook 'org-trigger-hook 'org-depend-trigger-todo)
  (add-hook 'org-blocker-hook 'org-depend-block-todo)

  (provide 'org-depend)

;;; org-depend.el ends here

  (defun my/org-mode-hook ()
    (add-hook 'before-save-hook #'my/org-add-ids-to-all nil t)
    (add-hook 'before-save-hook #'my/org-all-entries-fix-tags nil t)
    (add-hook 'before-save-hook #'my/org-all-entries-sort-tags nil t)
    (add-hook 'before-save-hook #'my/org-fix-internal-link-description nil t)
    (auto-fill-mode)
    (setq org-log-into-drawer t))

  (add-hook 'org-mode-hook #'my/org-mode-hook)
  (add-hook 'org-capture-after-finalize-hook #'org-save-all-org-buffers)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (ledger . t)))

  (setq org-directory (cond ((eq system-type 'gnu/linux) "~/src/notes")
                            ((eq system-type 'darwin) "~/src/notes")
                            (t "~/Dropbox/orgzly")))

  (setq org-html-format-drawer-function #'my/org-html-format-drawer-function
        org-export-backends '(html gfm)
        org-default-notes-file (concat org-directory "/unfiled.org")
        browse-url-browser-function #'browse-url-generic
        browse-url-generic-program "x-www-browser"
        org-agenda-files (file-expand-wildcards (concat org-directory "/*.org"))
        org-goto-auto-isearch nil
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 1))
        org-capture-templates '(("r" "Reference" entry
                                 (file org-default-notes-file)
                                 "* %^{Name} %^G
  :PROPERTIES:
  :CREATED: %T
  :LINK: %(my/org-get-link)
  :END:

  %?
")))

  ;;(define-key org-mode-map (kbd "C-c C-o") #'my/org-open-at-point-save-excursion)
  (define-key org-mode-map (kbd "M-p") #'my/org-show-previous-heading-tidily)
  (define-key org-mode-map (kbd "M-n") #'my/org-show-next-heading-tidily)
  )

; Setup ledger-mode
(progn
  (setq
   ledger-binary-path (cond ((eq system-type 'gnu/linux) "/usr/bin/ledger")
                            (t "/usr/local/bin/ledger"))
   ledger-use-iso-dates t)

  (defun org-babel-execute:ledger (body params)
    "Execute a block of Ledger entries with org-babel.  This function is
called by `org-babel-execute-src-block'."
    (message "executing Ledger source code block")
    (let ((result-params (split-string (or (cdr (assoc :results params)) "")))
          (cmdline (cdr (assoc :cmdline params)))
          (in-file (org-babel-temp-file "ledger-"))
          (out-file (org-babel-temp-file "ledger-output-")))
      (with-temp-file in-file (insert body))
      (message "%s" (concat (or ledger-binary-path "ledger")
                            " -f " (org-babel-process-file-name in-file)
                            " " cmdline))
      (with-output-to-string
        (shell-command (concat (or ledger-binary-path "ledger")
                               " -f " (org-babel-process-file-name in-file)
                               " " cmdline
                               " > " (org-babel-process-file-name out-file))))
      (with-temp-buffer (insert-file-contents out-file) (buffer-string)))))

; Setup artist-mode
(progn
  (require 'artist)
  (define-key artist-mode-map (kbd "<down-mouse-3>") #'artist-mouse-choose-operation))

; Setup fill column indicator
(progn
  (require 'fill-column-indicator)
  (setq fci-rule-column fill-column)
  (dolist (major-mode
           '(c-mode-hook
             emacs-lisp-mode-hook
             python-mode-hook))
    (add-hook major-mode 'fci-mode)))

; Setup yasnippet
(progn
  (when nil
    (setq yas-snippet-dirs "~/src/dot-emacs/snippets/")
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-/") 'yas-expand)
    (yas-reload-all)
    (yas-global-mode)))

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

; Setup emmet-mode
(progn
  (require 'emmet-mode)
  (define-key emmet-mode-keymap (kbd "M-n") 'emmet-next-edit-point)
  (define-key emmet-mode-keymap (kbd "M-p") 'emmet-prev-edit-point)

  (defun emmet-name (input)
    "Parse a class or identifier name, e.g. news, footer, mainimage"
    (emmet-parse "\\([a-zA-Z$@:][a-zA-Z0-9$@_:-]*\\)" 2 "class or identifer name"
                 `((name . ,(emmet-split-numbering-expressions
                             (elt it 1))) . ,input))))

; Setup web-mode
(progn
  (require 'web-mode)
  (require 'auto-complete)
  (add-hook 'web-mode-hook 'emmet-mode)

  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-block-padding 0
        web-mode-code-indent-offset 2
        web-mode-enable-control-block-indentation nil
        web-mode-enable-whitespace-fontification nil
        )

  (setq web-mode-extra-snippets
        '(("none" . (("mod" . "<script>\nmodule(function(exports, {|}) {\n});\n</script>\n")
                     ("vue" . "<script type=\"text/html\" id=\"|\">\n</script>\n")))

          ))

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


; Setup markdown-mode
(progn
  (require 'markdown-mode)
  (setq markdown-command "/usr/local/bin/node node_modules/.bin/marked")
  (defun markdown-fix-paragraphs ()
    (setq paragraph-start (concat "\f"
                                  "\\|[ \t]*$"
                                  "\\|[ \t]*[-+*] +.+$"
                                  "\\|```.*$"
                                  "\\|\\$.*$")
          paragraph-separate (concat "[ \t\f]*$"
                                     "\\|\\$.*$")))


  (add-hook 'markdown-mode-hook #'markdown-fix-paragraphs))


;; swap-window-configurations
(progn
  (defun swap-window-configurations (&optional arg)
    (interactive "P")
    (if arg
        (progn
          (set-register ?_ (get-register ?+))
          (window-configuration-to-register ?+)
          (when (get-register ?_)
            (jump-to-register ?_)))
      (set-register ?_ (get-register ?-))
      (window-configuration-to-register ?-)
      (when (get-register ?_)
        (jump-to-register ?_)))))

;; Remove trailing whitespace on save
(progn
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; Fix terminal size when window size changes
(progn
  (defun comint-fix-window-size ()
    "Change process window size."
    (when (derived-mode-p 'comint-mode)
      (let ((process (get-buffer-process (current-buffer))))
        (when process
          (set-process-window-size process (window-height) (window-width))))))

  (defun comint-fix-window-size-hook ()
    (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))

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
  (define-key custom-bindings-map (kbd "C-x RET") #'swap-window-configurations)
  (define-key custom-bindings-map (kbd "C-x <C-return>") #'swap-window-configurations)
  (define-key custom-bindings-map (kbd "C-c C-SPC")
    'fix-frame-size)
  (define-key custom-bindings-map (kbd "<wheel-left>")
    (lambda () (interactive) (scroll-right 1)))
  (define-key custom-bindings-map (kbd "<wheel-right>")
    (lambda () (interactive) (scroll-left 1)))
  (define-key custom-bindings-map (kbd "M-v") 'yank)
  (define-key custom-bindings-map (kbd "M-c") 'kill-ring-save)
  (define-key custom-bindings-map (kbd "C-x l") 'helm-lobsters)
  (define-key custom-bindings-map (kbd "C-x C-\\") 'open-all-files-in-directory)
  (define-key custom-bindings-map (kbd "C-c c") #'org-capture)
  (define-key custom-bindings-map (kbd "C-c a") #'org-agenda)
  (unless (memq window-system '(mac ns))
    (define-key custom-bindings-map (kbd "<deletechar>") #'backward-kill-word))

  (define-minor-mode custom-bindings-mode
    "A mode that activates custom-bindings"
    t nil custom-bindings-map)

  (custom-bindings-mode 1))

(progn
  (when (memq window-system '(mac ns))
    (set-frame-height (window-frame) 50)
    (fix-frame-size nil))
  (dolist (filename org-agenda-files)
    (find-file filename))
  (shell "*shell*<notes>")
  (insert "make")
  (comint-send-input)
  (cd "~/")
  (shell "*shell*"))
