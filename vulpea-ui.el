;;; vulpea-ui.el --- Sidebar infrastructure and widget framework for vulpea notes -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/vulpea-ui
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (vulpea "0.3") (vui "0.1"))
;; Keywords: org-mode roam notes

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; vulpea-ui provides a customizable sidebar that displays contextual
;; information about the currently focused vulpea note, along with a
;; set of default widgets and an API for creating custom widgets.
;;
;; Features:
;; - Per-frame sidebar with configurable position and size
;; - Widget system built on vui components
;; - Default widgets: outline, backlinks, forward links, stats
;; - Easy API for creating custom widgets
;;
;; Usage:
;;   (require 'vulpea-ui)
;;   (vulpea-ui-sidebar-open)
;;
;; Or add to hooks:
;;   (add-hook 'org-mode-hook #'vulpea-ui-sidebar-open)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'org-element)
(require 'vulpea)
(require 'vui)


;;; Custom variables

(defgroup vulpea-ui nil
  "Sidebar infrastructure and widget framework for vulpea notes."
  :group 'vulpea
  :prefix "vulpea-ui-")

(defcustom vulpea-ui-sidebar-position 'right
  "Position of the sidebar in the frame.
One of `left', `right', `top', or `bottom'."
  :type '(choice (const :tag "Left" left)
          (const :tag "Right" right)
          (const :tag "Top" top)
          (const :tag "Bottom" bottom))
  :group 'vulpea-ui)

(defcustom vulpea-ui-sidebar-size 0.33
  "Size of the sidebar as a fraction of the frame.
A float between 0.0 and 1.0 representing the fraction of frame
width (for left/right position) or height (for top/bottom position)."
  :type 'float
  :group 'vulpea-ui)

(defcustom vulpea-ui-sidebar-widgets
  '(vulpea-ui-widget-stats
    vulpea-ui-widget-outline
    vulpea-ui-widget-backlinks
    vulpea-ui-widget-links)
  "Ordered list of widgets to display in the sidebar.
Each element should be a symbol naming a vui component function."
  :type '(repeat symbol)
  :group 'vulpea-ui)

(defcustom vulpea-ui-default-widget-collapsed nil
  "Default collapsed state for all widgets.
When non-nil, widgets start collapsed."
  :type 'boolean
  :group 'vulpea-ui)

(defcustom vulpea-ui-outline-max-depth nil
  "Maximum heading depth for the outline widget.
When nil, show all heading levels."
  :type '(choice (const :tag "Unlimited" nil)
          (integer :tag "Max depth"))
  :group 'vulpea-ui)

(defcustom vulpea-ui-sidebar-auto-hide t
  "Whether to auto-hide sidebar when switching to non-vulpea buffers.
When non-nil, sidebar is hidden when the main window displays a
non-vulpea buffer, and shown again when returning to a vulpea note.
When nil, sidebar remains visible with stale content."
  :type 'boolean
  :group 'vulpea-ui)


;;; Context

(defcontext vulpea-ui-note nil
  "The current vulpea note being displayed in the sidebar.")


;;; Faces

(defface vulpea-ui-widget-header-face
  '((t :inherit bold))
  "Face for widget headers."
  :group 'vulpea-ui)

(defface vulpea-ui-widget-count-face
  '((t :inherit shadow))
  "Face for widget item counts."
  :group 'vulpea-ui)

(defface vulpea-ui-outline-heading-face
  '((t :inherit org-level-1))
  "Face for outline headings."
  :group 'vulpea-ui)

(defface vulpea-ui-stats-face
  '((t :inherit shadow))
  "Face for statistics text."
  :group 'vulpea-ui)


;;; Major mode

(defvar vulpea-ui-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'vulpea-ui-sidebar-close)
    (define-key map (kbd "g") #'vulpea-ui-sidebar-refresh)
    (define-key map (kbd "TAB") #'vulpea-ui-widget-toggle-at-point)
    (define-key map (kbd "RET") #'vulpea-ui-follow-link-at-point)
    map)
  "Keymap for `vulpea-ui-sidebar-mode'.")

(define-derived-mode vulpea-ui-sidebar-mode special-mode "vulpea-ui"
  "Major mode for the vulpea-ui sidebar buffer.
\\{vulpea-ui-sidebar-mode-map}"
  :group 'vulpea-ui
  (setq-local buffer-read-only t)
  (setq-local cursor-type nil)
  (setq-local truncate-lines t))


;;; Sidebar state (frame-local)

(defvar vulpea-ui--sidebar-instances (make-hash-table :test 'eq)
  "Hash table mapping frames to their sidebar vui instances.")

(defvar vulpea-ui--sidebar-auto-hidden (make-hash-table :test 'eq)
  "Hash table tracking frames where sidebar was auto-hidden.")

(defvar vulpea-ui--rendering nil
  "Non-nil when sidebar is currently rendering.
Used to prevent re-entry during render.")

(defun vulpea-ui--sidebar-buffer-name (&optional frame)
  "Return the sidebar buffer name for FRAME.
If FRAME is nil, use the selected frame."
  (let ((frame (or frame (selected-frame))))
    (format "*vulpea-ui-sidebar:%s*" (frame-parameter frame 'name))))

(defun vulpea-ui--get-sidebar-buffer (&optional frame)
  "Get the sidebar buffer for FRAME, or nil if it doesn't exist."
  (get-buffer (vulpea-ui--sidebar-buffer-name frame)))

(defun vulpea-ui--get-sidebar-window (&optional frame)
  "Get the sidebar window for FRAME, or nil if it doesn't exist."
  (let ((buf (vulpea-ui--get-sidebar-buffer frame)))
    (when buf
      (get-buffer-window buf frame))))

(defun vulpea-ui--sidebar-visible-p (&optional frame)
  "Return non-nil if the sidebar is visible in FRAME."
  (not (null (vulpea-ui--get-sidebar-window frame))))


;;; Window management

(defun vulpea-ui--display-buffer-params ()
  "Return display-buffer parameters for the sidebar."
  (let ((side vulpea-ui-sidebar-position)
        (size vulpea-ui-sidebar-size))
    `((side . ,side)
      (slot . 0)
      (window-width . ,(if (memq side '(left right)) size nil))
      (window-height . ,(if (memq side '(top bottom)) size nil))
      (window-parameters . ((no-delete-other-windows . t)
                            (dedicated . t)
                            (no-other-window . nil))))))

(defun vulpea-ui--create-sidebar-window (buffer)
  "Create a sidebar window for BUFFER using side window mechanics."
  (display-buffer-in-side-window buffer (vulpea-ui--display-buffer-params)))

(defun vulpea-ui--get-main-window (&optional frame)
  "Get the most recently used non-sidebar window in FRAME."
  (let* ((frame (or frame (selected-frame)))
         (sidebar-win (vulpea-ui--get-sidebar-window frame))
         (selected (frame-selected-window frame)))
    ;; Prefer the currently selected window if it's a valid main window
    (if (and selected
             (not (eq selected sidebar-win))
             (not (window-minibuffer-p selected)))
        selected
      ;; Fallback to first valid window
      (or (seq-find (lambda (win)
                      (and (not (eq win sidebar-win))
                           (not (window-minibuffer-p win))))
                    (window-list frame nil))
          (frame-first-window frame)))))


;;; Content tracking

(defvar-local vulpea-ui--current-note nil
  "The vulpea note currently being displayed in the sidebar.")

(defun vulpea-ui--get-note-from-buffer (buffer)
  "Get the vulpea note from BUFFER, or nil if not a vulpea note."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        ;; Always get the file-level ID, not the entry at point
        (save-excursion
          (goto-char (point-min))
          (let ((id (org-entry-get nil "ID")))
            (when id
              (vulpea-db-get-by-id id))))))))

(defun vulpea-ui--should-update-p (note)
  "Return non-nil if sidebar should update for NOTE."
  (and note
       (not (equal (vulpea-note-id note)
                   (when vulpea-ui--current-note
                     (vulpea-note-id vulpea-ui--current-note))))))

(defun vulpea-ui--on-buffer-change (&optional _frame)
  "Handle buffer change events and update sidebar if needed.
Called from `window-buffer-change-functions'."
  ;; Skip minibuffer interactions and re-entry during render
  (unless (or (minibufferp) vulpea-ui--rendering)
    (let* ((frame (selected-frame))
           (sidebar-buf (vulpea-ui--get-sidebar-buffer frame))
           (auto-hidden-p (gethash frame vulpea-ui--sidebar-auto-hidden)))
      (when sidebar-buf
        (let* ((main-win (vulpea-ui--get-main-window frame))
               (main-buf (when main-win (window-buffer main-win)))
               (note (vulpea-ui--get-note-from-buffer main-buf))
               ;; Only auto-hide if we previously had a note displayed
               (had-note (buffer-local-value 'vulpea-ui--current-note sidebar-buf))
               (visible (vulpea-ui--sidebar-visible-p frame))
               ;; Compare IDs directly (had-note is from sidebar buffer)
               (same-note (and note had-note
                               (equal (vulpea-note-id note)
                                      (vulpea-note-id had-note)))))
          (cond
           ;; Non-vulpea buffer: auto-hide if enabled AND we had a note before
           ((and (null note)
                 had-note
                 vulpea-ui-sidebar-auto-hide
                 visible)
            (vulpea-ui--hide-sidebar-window frame)
            (puthash frame t vulpea-ui--sidebar-auto-hidden))
           ;; Vulpea buffer and was auto-hidden: show again
           ((and note auto-hidden-p)
            (remhash frame vulpea-ui--sidebar-auto-hidden)
            (vulpea-ui--show-sidebar-window frame)
            ;; Only re-render if note actually changed
            (unless same-note
              (vulpea-ui--render-sidebar note frame)))
           ;; Vulpea buffer and visible: update if needed
           ((and note visible (not same-note))
            (vulpea-ui--render-sidebar note frame))))))))

(defun vulpea-ui--hide-sidebar-window (&optional frame)
  "Hide the sidebar window in FRAME without killing the buffer."
  (let ((win (vulpea-ui--get-sidebar-window frame)))
    (when win
      (delete-window win))))

(defun vulpea-ui--show-sidebar-window (&optional frame)
  "Show the sidebar window in FRAME."
  (let ((buf (vulpea-ui--get-sidebar-buffer frame)))
    (when (and buf (not (vulpea-ui--sidebar-visible-p frame)))
      (vulpea-ui--create-sidebar-window buf))))

(defun vulpea-ui--setup-hooks ()
  "Set up hooks for sidebar content tracking."
  (add-hook 'window-buffer-change-functions #'vulpea-ui--on-buffer-change)
  (add-hook 'window-selection-change-functions #'vulpea-ui--on-buffer-change))

(defun vulpea-ui--teardown-hooks ()
  "Remove hooks for sidebar content tracking."
  (remove-hook 'window-buffer-change-functions #'vulpea-ui--on-buffer-change)
  (remove-hook 'window-selection-change-functions #'vulpea-ui--on-buffer-change))


;;; Utility functions

(defun vulpea-ui-current-note ()
  "Get the current note from context.
For use within widget components."
  (use-vulpea-ui-note))

(defun vulpea-ui-visit-note (note)
  "Navigate to NOTE in the main window, preserving sidebar layout."
  (when note
    (let ((main-win (vulpea-ui--get-main-window)))
      (when main-win
        (select-window main-win)
        (find-file (vulpea-note-path note))
        (when (> (vulpea-note-level note) 0)
          (goto-char (vulpea-note-pos note)))))))

(defun vulpea-ui-follow-link-at-point ()
  "Follow the link or item under point."
  (interactive)
  (let ((button (button-at (point))))
    (when button
      (button-activate button))))

(defun vulpea-ui-widget-toggle-at-point ()
  "Toggle the widget collapse state at point."
  (interactive)
  ;; This will be handled by vui button mechanics
  (let ((button (button-at (point))))
    (when button
      (button-activate button))))


;;; Widget wrapper component

(defcomponent vulpea-ui-widget (title count children)
  "Standard widget wrapper with collapsible header.
TITLE is the widget title string.
COUNT is an optional count to display in the header.
CHILDREN is a function returning the widget content."
  :state ((collapsed vulpea-ui-default-widget-collapsed))
  :render
  (vui-vstack
   :spacing 0
   ;; Header
   (vui-hstack
    :spacing 1
    (vui-button (if collapsed "▶" "▼")
      :on-click (lambda () (vui-set-state :collapsed (not collapsed)))
      :face 'vulpea-ui-widget-header-face)
    (vui-text title :face 'vulpea-ui-widget-header-face)
    (when count
      (vui-text (format "(%d)" count) :face 'vulpea-ui-widget-count-face)))
   ;; Content (when not collapsed)
   (unless collapsed
     (when children
       (vui-vstack
        :spacing 0
        :indent 2
        (funcall children))))))


;;; Shared components

(defcomponent vulpea-ui-note-link (note on-click)
  "Clickable link component for a vulpea note.
NOTE is the vulpea-note struct.
ON-CLICK is an optional callback (defaults to `vulpea-ui-visit-note')."
  :render
  (when note
    (vui-button (or (vulpea-note-title note) "(untitled)")
      :on-click (lambda ()
                  (funcall (or on-click #'vulpea-ui-visit-note) note)))))

(defcomponent vulpea-ui-note-preview (note max-lines strip-drawers strip-metadata)
  "Rendered preview of note content.
NOTE is the vulpea-note struct.
MAX-LINES limits the preview length (default: 10).
STRIP-DRAWERS removes property drawers (default: t).
STRIP-METADATA removes #+TITLE, #+FILETAGS, etc. (default: t)."
  :render
  (let* ((max-lines (or max-lines 10))
         (strip-drawers (if (null strip-drawers) t strip-drawers))
         (strip-metadata (if (null strip-metadata) t strip-metadata))
         (content (vulpea-ui--get-note-preview note max-lines strip-drawers strip-metadata)))
    (when content
      (vui-text content))))

(defun vulpea-ui--get-note-preview (note max-lines strip-drawers strip-metadata)
  "Get preview text for NOTE.
MAX-LINES limits the number of lines.
STRIP-DRAWERS removes property drawers when non-nil.
STRIP-METADATA removes org metadata lines when non-nil."
  (when (and note (vulpea-note-path note))
    (let ((path (vulpea-note-path note)))
      (when (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (vulpea-note-pos note))
          ;; Skip the heading line itself if it's a heading note
          (when (> (vulpea-note-level note) 0)
            (forward-line 1))
          (let ((start (point))
                (lines nil)
                (count 0))
            (while (and (< count max-lines)
                        (not (eobp)))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                ;; Filter lines based on settings
                (unless (or (and strip-drawers
                                 (or (string-match-p "^[ \t]*:PROPERTIES:$" line)
                                     (string-match-p "^[ \t]*:END:$" line)
                                     (string-match-p "^[ \t]*:[A-Z_]+:.*$" line)))
                            (and strip-metadata
                                 (string-match-p "^#\\+" line))
                            (string-empty-p (string-trim line)))
                  (push line lines)
                  (cl-incf count)))
              (forward-line 1))
            (when lines
              (string-join (nreverse lines) "\n"))))))))


;;; Stats widget

(defcomponent vulpea-ui-widget-stats ()
  "Widget displaying statistics about the current note."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let* ((stats (use-memo (note)
                      (vulpea-ui--compute-stats note)))
             (chars (plist-get stats :chars))
             (words (plist-get stats :words))
             (links (plist-get stats :links)))
        (vui-component 'vulpea-ui-widget
          :title "Stats"
          :children
          (lambda ()
            (vui-text
                (format "%s chars · %s words · %d links"
                        (vulpea-ui--format-number chars)
                        (vulpea-ui--format-number words)
                        links)
              :face 'vulpea-ui-stats-face)))))))

(defun vulpea-ui--compute-stats (note)
  "Compute statistics for NOTE.
Returns a plist with :chars, :words, and :links."
  (if (and note (vulpea-note-path note))
      (let ((path (vulpea-note-path note))
            (links (seq-filter (lambda (link)
                                 (equal "id" (plist-get link :type)))
                               (vulpea-note-links note))))
        (if (file-exists-p path)
            (with-temp-buffer
              (insert-file-contents path)
              (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                     (chars (length content))
                     (words (length (split-string content "\\W+" t))))
                (list :chars chars :words words :links (length links))))
          (list :chars 0 :words 0 :links (length links))))
    (list :chars 0 :words 0 :links 0)))

(defun vulpea-ui--format-number (n)
  "Format number N with thousands separators."
  (let ((s (number-to-string n)))
    (if (< n 1000)
        s
      (let ((result nil)
            (i 0))
        (dolist (c (reverse (string-to-list s)))
          (when (and (> i 0) (= (mod i 3) 0))
            (push ?, result))
          (push c result)
          (cl-incf i))
        (apply #'string result)))))


;;; Outline widget

(defcomponent vulpea-ui-widget-outline ()
  "Widget displaying the heading structure of the current note."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let ((headings (use-memo (note)
                        (vulpea-ui--parse-headings note))))
        (vui-component 'vulpea-ui-widget
          :title "Outline"
          :count (length headings)
          :children
          (lambda ()
            (if headings
                (seq-map
                 (lambda (heading)
                   (vulpea-ui--render-outline-heading heading note))
                 headings)
              (vui-text "No headings" :face 'shadow))))))))

(defun vulpea-ui--heading-archived-p (hl archive-tag)
  "Return non-nil if HL or any of its ancestors has ARCHIVE-TAG."
  (let ((current hl)
        (archived nil))
    (while (and current (not archived))
      (when (member archive-tag (org-element-property :tags current))
        (setq archived t))
      (setq current (org-element-property :parent current)))
    archived))

(defun vulpea-ui--parse-headings (note)
  "Parse headings from NOTE using org-element.
Returns a list of plists with :title, :level, and :pos."
  (when (and note (vulpea-note-path note))
    (let ((path (vulpea-note-path note))
          (max-depth vulpea-ui-outline-max-depth))
      (when (file-exists-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (org-mode)
          (let ((headings nil)
                (archive-tag org-archive-tag))
            (org-element-map (org-element-parse-buffer 'headline) 'headline
              (lambda (hl)
                (let ((level (org-element-property :level hl))
                      (title (org-element-property :raw-value hl))
                      (pos (org-element-property :begin hl)))
                  (when (and (not (vulpea-ui--heading-archived-p hl archive-tag))
                             (or (null max-depth) (<= level max-depth)))
                    (push (list :title title :level level :pos pos) headings)))))
            (nreverse headings)))))))

(defun vulpea-ui--render-outline-heading (heading note)
  "Render a single HEADING for outline widget.
NOTE is the parent note for navigation."
  (let* ((title (plist-get heading :title))
         (level (plist-get heading :level))
         (pos (plist-get heading :pos))
         (stars (make-string level ?*)))
    (vui-button (concat stars " " title)
      :face 'shadow
      :on-click (lambda ()
                  (vulpea-ui--jump-to-position note pos)))))

(defun vulpea-ui--jump-to-position (note pos)
  "Jump to position POS in NOTE's file."
  (when (and note pos)
    (let ((main-win (vulpea-ui--get-main-window)))
      (when main-win
        (select-window main-win)
        (find-file (vulpea-note-path note))
        (goto-char pos)
        (org-fold-show-entry)
        (recenter)))))


;;; Backlinks widget

(defcomponent vulpea-ui-widget-backlinks ()
  "Widget displaying notes that link to the current note."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let ((backlinks (use-memo (note)
                         (vulpea-ui--get-backlinks note))))
        (vui-component 'vulpea-ui-widget
          :title "Backlinks"
          :count (length backlinks)
          :children
          (lambda ()
            (if backlinks
                (vui-vstack
                 :spacing 0
                 (seq-map
                  (lambda (bl-note)
                    (vui-component 'vulpea-ui-note-link :note bl-note))
                  backlinks))
              (vui-text "No backlinks" :face 'shadow))))))))

(defun vulpea-ui--get-backlinks (note)
  "Get notes that link to NOTE."
  (when note
    (vulpea-db-query-by-links-some
     (list (cons "id" (vulpea-note-id note))))))


;;; Forward links widget

(defcomponent vulpea-ui-widget-links ()
  "Widget displaying notes that the current note links to."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let ((forward-links (use-memo (note)
                             (vulpea-ui--get-forward-links note))))
        (vui-component 'vulpea-ui-widget
          :title "Links"
          :count (length forward-links)
          :children
          (lambda ()
            (if forward-links
                (vui-vstack
                 :spacing 0
                 (seq-map
                  (lambda (link-note)
                    (vui-component 'vulpea-ui-note-link :note link-note))
                  forward-links))
              (vui-text "No links" :face 'shadow))))))))

(defun vulpea-ui--get-forward-links (note)
  "Get notes that NOTE links to."
  (when note
    (let* ((links (vulpea-note-links note))
           (id-links (seq-filter (lambda (link)
                                   (equal "id" (plist-get link :type)))
                                 links))
           (ids (seq-map (lambda (link) (plist-get link :dest)) id-links)))
      (when ids
        (vulpea-db-query-by-ids ids)))))


;;; Root component

(defcomponent vulpea-ui-sidebar-content ()
  "Content component for the sidebar (uses context)."
  :render
  (let ((note (use-vulpea-ui-note)))
    (if note
        (vui-vstack
         :spacing 1
         (seq-map (lambda (widget-sym)
                    (vui-component widget-sym :key widget-sym))
                  vulpea-ui-sidebar-widgets))
      (vui-text "No vulpea note selected" :face 'shadow))))

(defcomponent vulpea-ui-sidebar-root (note)
  "Root component for the sidebar with NOTE context."
  :render
  (vulpea-ui-note-provider note
    (vui-component 'vulpea-ui-sidebar-content)))


;;; Rendering

(defun vulpea-ui--render-sidebar (note &optional frame)
  "Render the sidebar with NOTE as context in FRAME."
  (let* ((vulpea-ui--rendering t)  ; Prevent re-entry
         (frame (or frame (selected-frame)))
         (buf-name (vulpea-ui--sidebar-buffer-name frame))
         (buf (get-buffer-create buf-name))
         (sidebar-win (vulpea-ui--get-sidebar-window frame))
         (original-window (selected-window)))
    ;; Select sidebar window before mount (vui-mount calls switch-to-buffer)
    (when sidebar-win
      (select-window sidebar-win t))
    (with-current-buffer buf
      ;; Mount fresh - vui-mount calls kill-all-local-variables
      (let ((new-instance
             (vui-mount
              (vui-component 'vulpea-ui-sidebar-root :note note)
              buf-name)))
        (puthash frame new-instance vulpea-ui--sidebar-instances))
      ;; Set current note AFTER mount (vui-mount kills local variables)
      (setq vulpea-ui--current-note note)
      (goto-char (point-min)))
    ;; Restore original window
    (when (window-live-p original-window)
      (select-window original-window t))))


;;; Commands

;;;###autoload
(defun vulpea-ui-sidebar-open ()
  "Open or show the vulpea-ui sidebar in the current frame."
  (interactive)
  (let* ((frame (selected-frame))
         (buf-name (vulpea-ui--sidebar-buffer-name frame))
         (buf (get-buffer-create buf-name)))
    ;; Set up the buffer
    (with-current-buffer buf
      (unless (derived-mode-p 'vulpea-ui-sidebar-mode)
        (vulpea-ui-sidebar-mode)))
    ;; Create window if not visible
    (unless (vulpea-ui--sidebar-visible-p frame)
      (vulpea-ui--create-sidebar-window buf))
    ;; Set up hooks
    (vulpea-ui--setup-hooks)
    ;; Initial render with current note
    (let* ((main-win (vulpea-ui--get-main-window frame))
           (main-buf (when main-win (window-buffer main-win)))
           (note (vulpea-ui--get-note-from-buffer main-buf)))
      (vulpea-ui--render-sidebar note frame))))

;;;###autoload
(defun vulpea-ui-sidebar-close ()
  "Close the vulpea-ui sidebar in the current frame."
  (interactive)
  (let* ((frame (selected-frame))
         (win (vulpea-ui--get-sidebar-window frame))
         (buf (vulpea-ui--get-sidebar-buffer frame)))
    (when win
      (delete-window win))
    (when buf
      (kill-buffer buf))
    ;; Clean up state
    (remhash frame vulpea-ui--sidebar-instances)
    (remhash frame vulpea-ui--sidebar-auto-hidden)
    ;; Teardown hooks if no more sidebars
    (when (hash-table-empty-p vulpea-ui--sidebar-instances)
      (vulpea-ui--teardown-hooks))))

;;;###autoload
(defun vulpea-ui-sidebar-toggle ()
  "Toggle the vulpea-ui sidebar visibility in the current frame."
  (interactive)
  (if (vulpea-ui--sidebar-visible-p)
      (vulpea-ui-sidebar-close)
    (vulpea-ui-sidebar-open)))

;;;###autoload
(defun vulpea-ui-sidebar-refresh ()
  "Force refresh the sidebar content."
  (interactive)
  (let* ((frame (selected-frame))
         (main-win (vulpea-ui--get-main-window frame))
         (main-buf (when main-win (window-buffer main-win)))
         (note (vulpea-ui--get-note-from-buffer main-buf)))
    (setq vulpea-ui--current-note nil)  ; Force update
    (vulpea-ui--render-sidebar note frame)))


(provide 'vulpea-ui)
;;; vulpea-ui.el ends here
