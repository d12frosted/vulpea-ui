;;; vulpea-ui.el --- Sidebar infrastructure and widget framework for vulpea notes -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Boris Buliga
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/vulpea-ui
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (vulpea "2.0.0") (vui "0.1"))
;; Keywords: outlines hypermedia

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
(require 'vui-components)


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

(defcustom vulpea-ui-backlinks-show-preview t
  "Whether to show content preview in backlinks widget.
When non-nil, shows a snippet of text around each backlink mention."
  :type 'boolean
  :group 'vulpea-ui)

(defcustom vulpea-ui-backlinks-preview-lines 2
  "Number of lines to show in backlink previews for prose context."
  :type 'integer
  :group 'vulpea-ui)

(defcustom vulpea-ui-backlinks-prose-chars-before 30
  "Number of characters to show before link in prose previews."
  :type 'integer
  :group 'vulpea-ui)

(defcustom vulpea-ui-backlinks-prose-chars-after 50
  "Number of characters to show after link in prose previews."
  :type 'integer
  :group 'vulpea-ui)

(defcustom vulpea-ui-backlinks-note-filter #'identity
  "Function to filter which notes appear in backlinks.
Called with a vulpea-note and should return non-nil to include it."
  :type 'function
  :group 'vulpea-ui)

(defcustom vulpea-ui-backlinks-context-types t
  "Context types to display in backlinks widget.
Either t for all types, or a list of allowed types:
meta, header, table, list, quote, code, footnote, prose."
  :type '(choice (const :tag "All types" t)
          (repeat :tag "Selected types"
           (choice (const meta)
            (const header)
            (const table)
            (const list)
            (const quote)
            (const code)
            (const footnote)
            (const prose))))
  :group 'vulpea-ui)

(defcustom vulpea-ui-backlinks-sort nil
  "How to sort backlinks in the widget.
nil means no sorting (order from database query).
`title-asc' means sort alphabetically by note title (A-Z).
`title-desc' means sort reverse alphabetically by note title (Z-A).
A function means use it as comparator (receives two group plists
with :file-note, :path, and :mentions)."
  :type '(choice (const :tag "No sorting" nil)
          (const :tag "By title A-Z" title-asc)
          (const :tag "By title Z-A" title-desc)
          (function :tag "Custom comparator"))
  :group 'vulpea-ui)

(defcustom vulpea-ui-fast-parse nil
  "Use fast `org-mode' initialization for parsing.
When non-nil, skip mode hooks when parsing org files for headings
and backlinks. This can significantly improve performance but may
cause issues if your org-element parsing depends on mode hooks.
Disabled by default for safety."
  :type 'boolean
  :group 'vulpea-ui)

(defcustom vulpea-ui-auto-refresh t
  "Automatically refresh sidebar content.
When non-nil, the sidebar will refresh:
- After saving a buffer (full refresh for backlinks/links)
- After idle time (stats and outline only)"
  :type 'boolean
  :group 'vulpea-ui)

(defcustom vulpea-ui-auto-refresh-delay 1.5
  "Delay in seconds before auto-refreshing on idle.
Only used when `vulpea-ui-auto-refresh' is non-nil."
  :type 'number
  :group 'vulpea-ui)


;;; Context

(vui-defcontext vulpea-ui-note nil
  "The current vulpea note being displayed in the sidebar.")


;;; Widget Registry

(defvar vulpea-ui--widget-registry (make-hash-table :test 'eq)
  "Registry of widgets available for the sidebar.
Keys are widget symbols, values are plists with:
  :component - the vui component symbol
  :predicate - function taking note, returns non-nil if widget shows
  :order - numeric order for sorting (lower = earlier)")

(defun vulpea-ui-register-widget (name &rest props)
  "Register a widget NAME with properties PROPS.

NAME is a symbol identifying the widget.

PROPS is a plist with:
  :component - (required) symbol naming the vui component
  :predicate - (optional) function (note) -> bool, widget shown when true
  :order - (optional) numeric order, default 100

Example:
  (vulpea-ui-register-widget \\='journal-nav
    :component \\='vulpea-journal-widget-nav
    :predicate #\\='vulpea-journal-note-p
    :order 50)"
  (let ((component (plist-get props :component))
        (predicate (plist-get props :predicate))
        (order (or (plist-get props :order) 100)))
    (unless component
      (error "Widget %s requires :component" name))
    (puthash name
             (list :component component
                   :predicate predicate
                   :order order)
             vulpea-ui--widget-registry)))

(defun vulpea-ui-unregister-widget (name)
  "Remove widget NAME from the registry."
  (remhash name vulpea-ui--widget-registry))

(defun vulpea-ui-widget-set (name prop value)
  "Set property PROP to VALUE for widget NAME."
  (when-let ((widget (gethash name vulpea-ui--widget-registry)))
    (puthash name (plist-put widget prop value) vulpea-ui--widget-registry)))

(defun vulpea-ui--get-widgets-for-note (note)
  "Return list of widget components to display for NOTE.
Widgets are filtered by predicate and sorted by order."
  (let ((widgets nil))
    ;; Collect applicable widgets
    (maphash
     (lambda (name props)
       (let ((predicate (plist-get props :predicate))
             (component (plist-get props :component))
             (order (plist-get props :order)))
         (when (or (null predicate)
                   (funcall predicate note))
           (push (list :name name :component component :order order) widgets))))
     vulpea-ui--widget-registry)
    ;; Sort by order
    (setq widgets (sort widgets (lambda (a b)
                                  (< (plist-get a :order)
                                     (plist-get b :order)))))
    ;; Return component symbols
    (mapcar (lambda (w) (plist-get w :component)) widgets)))


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

(defface vulpea-ui-backlink-preview-face
  '((t :inherit shadow))
  "Face for backlink preview text."
  :group 'vulpea-ui)

(defface vulpea-ui-backlink-heading-face
  '((t :inherit shadow))
  "Face for backlink heading path."
  :group 'vulpea-ui)

(defface vulpea-ui-backlink-meta-key-face
  '((t :inherit (shadow bold)))
  "Face for meta block keys in backlink previews."
  :group 'vulpea-ui)

(defface vulpea-ui-backlink-meta-value-face
  '((t :inherit shadow))
  "Face for meta block values in backlink previews."
  :group 'vulpea-ui)

(defface vulpea-ui-backlink-context-face
  '((t :inherit shadow))
  "Face for context type indicators (§, •, >, etc.) in backlink previews."
  :group 'vulpea-ui)


;;; Major mode

(defvar vulpea-ui-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'vulpea-ui-sidebar-close)
    (define-key map (kbd "g") #'vulpea-ui-sidebar-refresh)
    map)
  "Keymap for `vulpea-ui-sidebar-mode'.")

(define-derived-mode vulpea-ui-sidebar-mode vui-mode "vulpea-ui"
  "Major mode for the vulpea-ui sidebar buffer.
\\{vulpea-ui-sidebar-mode-map}"
  :group 'vulpea-ui
  (setq-local truncate-lines t)
  (mode-line-invisible-mode 1))


;;; Sidebar state (frame-local)

(defvar vulpea-ui--sidebar-instances (make-hash-table :test 'eq)
  "Hash table mapping frames to their sidebar vui instances.")

(defvar vulpea-ui--sidebar-auto-hidden (make-hash-table :test 'eq)
  "Hash table tracking frames where sidebar was auto-hidden.")

(defvar vulpea-ui--rendering nil
  "Non-nil when sidebar is currently rendering.
Used to prevent re-entry during render.")

(defvar vulpea-ui--idle-timer nil
  "Timer for auto-refreshing sidebar on idle.")

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
  (vulpea-ui--get-sidebar-window frame))


;;; Window management

(defun vulpea-ui--display-buffer-params ()
  "Return `display-buffer' parameters for the sidebar."
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
  (add-hook 'window-selection-change-functions #'vulpea-ui--on-buffer-change)
  ;; Auto-refresh hooks
  (when vulpea-ui-auto-refresh
    (add-hook 'after-save-hook #'vulpea-ui--on-save)
    (vulpea-ui--start-idle-timer)))

(defun vulpea-ui--teardown-hooks ()
  "Remove hooks for sidebar content tracking."
  (remove-hook 'window-buffer-change-functions #'vulpea-ui--on-buffer-change)
  (remove-hook 'window-selection-change-functions #'vulpea-ui--on-buffer-change)
  ;; Auto-refresh hooks
  (remove-hook 'after-save-hook #'vulpea-ui--on-save)
  (vulpea-ui--stop-idle-timer))

(defun vulpea-ui--start-idle-timer ()
  "Start the idle timer for auto-refresh."
  (vulpea-ui--stop-idle-timer)
  (setq vulpea-ui--idle-timer
        (run-with-idle-timer vulpea-ui-auto-refresh-delay t
                             #'vulpea-ui--on-idle)))

(defun vulpea-ui--stop-idle-timer ()
  "Stop the idle timer for auto-refresh."
  (when vulpea-ui--idle-timer
    (cancel-timer vulpea-ui--idle-timer)
    (setq vulpea-ui--idle-timer nil)))

(defun vulpea-ui--on-save ()
  "Handle buffer save - refresh sidebar if visible."
  (when (and (vulpea-ui--sidebar-visible-p)
             (vulpea-ui--get-note-from-buffer (current-buffer)))
    (vulpea-ui-sidebar-refresh)))

(defun vulpea-ui--on-idle ()
  "Handle idle timeout - refresh stats and outline."
  (when (vulpea-ui--sidebar-visible-p)
    (let* ((frame (selected-frame))
           (main-win (vulpea-ui--get-main-window frame))
           (main-buf (when main-win (window-buffer main-win)))
           (note (vulpea-ui--get-note-from-buffer main-buf)))
      (when note
        (vulpea-ui-sidebar-refresh)))))


;;; Utility functions

(defun vulpea-ui--setup-org-mode ()
  "Set up `org-mode' for parsing, respecting `vulpea-ui-fast-parse'.
When fast parsing is enabled, skip mode hooks for better performance."
  (if vulpea-ui-fast-parse
      (delay-mode-hooks (org-mode))
    (org-mode)))

(defun vulpea-ui-clean-org-markup (text)
  "Clean `org-mode' markup from TEXT for display purposes.

This function removes or simplifies various `org-mode' constructs:

- Links: [[url][title]] becomes title, [[url]] becomes url
- Drawers: :PROPERTIES:...:END: blocks are removed
- Metadata: #+TITLE:, #+FILETAGS:, etc. lines are removed
- Whitespace: multiple spaces/tabs are collapsed to single space

Returns the cleaned string, or nil if TEXT is nil."
  (when text
    (let ((result text))
      ;; Remove drawers (:PROPERTIES:...:END:, :LOGBOOK:...:END:, etc.)
      (setq result (replace-regexp-in-string
                    "^[ \t]*:[A-Z_]+:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?"
                    ""
                    result))
      ;; Remove metadata lines (#+TITLE:, #+FILETAGS:, etc.)
      (setq result (replace-regexp-in-string
                    "^[ \t]*#\\+[A-Za-z_]+:.*\n?"
                    ""
                    result))
      ;; Replace [[link][description]] with description (any link type)
      (setq result (replace-regexp-in-string
                    "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]"
                    "\\2"
                    result))
      ;; Replace bare [[link]] with the link target
      ;; For id: links, remove them; for URLs, keep the URL
      (setq result (replace-regexp-in-string
                    "\\[\\[id:[^]]+\\]\\]"
                    ""
                    result))
      (setq result (replace-regexp-in-string
                    "\\[\\[\\([^]]+\\)\\]\\]"
                    "\\1"
                    result))
      ;; Clean up multiple spaces/tabs
      (setq result (replace-regexp-in-string "[ \t]+" " " result))
      ;; Clean up multiple newlines (but keep paragraph breaks)
      (setq result (replace-regexp-in-string "\n\\{3,\\}" "\n\n" result))
      (string-trim result))))

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
  (let ((widget (widget-at (point))))
    (when widget
      (widget-apply widget :action))))

(defun vulpea-ui-widget-toggle-at-point ()
  "Toggle the widget collapse state at point."
  (interactive)
  (let ((widget (widget-at (point))))
    (when widget
      (widget-apply widget :action))))


;;; Widget wrapper component

(vui-defcomponent vulpea-ui-widget (title count)
  "Standard widget wrapper with collapsible header.
TITLE is the widget title string.
COUNT is an optional count to display in the header.
CHILDREN (implicit) is a function returning the widget content."
  :render
  (let ((display-title (if count
                           (format "%s (%s)" title count)
                         title)))
    (vui-collapsible
      :title display-title
      :initially-expanded (not vulpea-ui-default-widget-collapsed)
      :title-face 'vulpea-ui-widget-header-face
      :indent 2
      (when children
        (funcall children)))))


;;; Shared components

(vui-defcomponent vulpea-ui-note-link (note on-click)
  "Clickable link component for a vulpea note.
NOTE is the vulpea-note struct.
ON-CLICK is an optional callback (defaults to `vulpea-ui-visit-note')."
  :render
  (when note
    (vui-button (or (vulpea-note-title note) "(untitled)")
      :on-click (lambda ()
                  (funcall (or on-click #'vulpea-ui-visit-note) note))
      :help-echo nil)))

(vui-defcomponent vulpea-ui-note-preview (note max-lines strip-drawers strip-metadata)
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
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (vulpea-note-pos note))
        ;; Skip the heading line itself if it's a heading note
        (when (> (vulpea-note-level note) 0)
          (forward-line 1))
        (let ((lines nil)
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
            (string-join (nreverse lines) "\n")))))))


;;; Stats widget

(vui-defcomponent vulpea-ui-widget-stats ()
  "Widget displaying statistics about the current note."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let* ((stats (vui-use-memo (note)
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
Returns a plist with :chars, :words, and :links.
If the note's file is open in a buffer, reads from buffer for live stats.
Otherwise reads from disk."
  (if (and note (vulpea-note-path note))
      (let ((path (vulpea-note-path note))
            (links (seq-filter (lambda (link)
                                 (equal "id" (plist-get link :type)))
                               (vulpea-note-links note)))
            (existing-buf (find-buffer-visiting (vulpea-note-path note))))
        (let* ((content (if existing-buf
                            (with-current-buffer existing-buf
                              (buffer-substring-no-properties (point-min) (point-max)))
                          (with-temp-buffer
                            (insert-file-contents path)
                            (buffer-substring-no-properties (point-min) (point-max)))))
               (chars (length content))
               (words (length (split-string content "\\W+" t))))
          (list :chars chars :words words :links (length links))))
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

(vui-defcomponent vulpea-ui-widget-outline ()
  "Widget displaying the heading structure of the current note."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let ((headings (vui-use-memo (note)
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
              (vui-muted "No headings"))))))))

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
      (with-temp-buffer
        (insert-file-contents path)
        (vulpea-ui--setup-org-mode)
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
          (nreverse headings))))))

(defun vulpea-ui--render-outline-heading (heading note)
  "Render a single HEADING for outline widget.
NOTE is the parent note for navigation."
  (let* ((title (plist-get heading :title))
         (level (plist-get heading :level))
         (pos (plist-get heading :pos))
         ;; Indent based on level: level 1 = 0, level 2 = 5, etc.
         (indent (* (1- level) 5)))
    (vui-vstack
     :indent indent
     (vui-button (concat "· " title)
       :face 'shadow
       :no-decoration t
       :on-click (lambda ()
                   (vulpea-ui--jump-to-position note pos))
       :help-echo nil))))

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

(vui-defcomponent vulpea-ui-widget-backlinks ()
  "Widget displaying notes that link to the current note.
Groups backlinks by file and shows heading context with optional previews."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let* ((result (vui-use-memo (note)
                       (vulpea-ui--get-grouped-backlinks note)))
             (groups (plist-get result :groups))
             (filtered-count (plist-get result :filtered-count))
             (total-count (plist-get result :total-count))
             (count-display (if (= filtered-count total-count)
                                filtered-count
                              (format "%d/%d" filtered-count total-count))))
        (vui-component 'vulpea-ui-widget
          :title "Backlinks"
          :count count-display
          :children
          (lambda ()
            (if groups
                (vui-vstack
                 :spacing 1
                 (seq-map #'vulpea-ui--render-backlink-group groups))
              (vui-muted "No backlinks"))))))))

(defun vulpea-ui--get-grouped-backlinks (note)
  "Get backlinks to NOTE grouped by file.
Returns a plist with :groups, :filtered-count, and :total-count.
Each group has :file-note, :path, and :mentions.
Each mention has :heading-path, :pos, and :preview.
Applies `vulpea-ui-backlinks-note-filter' and
`vulpea-ui-backlinks-context-types'."
  (if (null note)
      (list :groups nil :filtered-count 0 :total-count 0)
    (let* ((target-id (vulpea-note-id note))
           (backlinks (vulpea-db-query-by-links-some
                       (list (cons "id" target-id))))
           ;; Group backlinks by file path
           (by-path (make-hash-table :test 'equal))
           (total-count 0))
      ;; Collect all mentions grouped by path (deduplicate by position)
      (let ((seen-positions (make-hash-table :test 'equal)))
        (dolist (bl backlinks)
          (let* ((path (vulpea-note-path bl))
                 (links (vulpea-note-links bl))
                 ;; Find links pointing to our target
                 (target-links (seq-filter
                                (lambda (link)
                                  (and (equal "id" (plist-get link :type))
                                       (equal target-id (plist-get link :dest))))
                                links)))
            (dolist (link target-links)
              (let* ((pos (plist-get link :pos))
                     (key (cons path pos)))
                ;; Only add if we haven't seen this path+position combo
                (unless (gethash key seen-positions)
                  (puthash key t seen-positions)
                  (cl-incf total-count)
                  (push (list :pos pos :source-note bl)
                        (gethash path by-path))))))))
      ;; Batch fetch file-level notes
      (let* ((paths (hash-table-keys by-path))
             (file-notes (when paths
                           (vulpea-db-query-by-file-paths paths 0)))
             (file-notes-by-path (make-hash-table :test 'equal)))
        ;; Index file notes by path
        (dolist (fn file-notes)
          (puthash (vulpea-note-path fn) fn file-notes-by-path))
        ;; Build grouped result with filtering
        (let ((result nil)
              (filtered-count 0))
          (dolist (path paths)
            (let* ((file-note (gethash path file-notes-by-path))
                   ;; Apply note filter
                   (note-allowed (funcall vulpea-ui-backlinks-note-filter file-note)))
              (when note-allowed
                (let* ((mentions (gethash path by-path))
                       ;; Sort mentions by position
                       (sorted-mentions (seq-sort
                                         (lambda (a b)
                                           (< (plist-get a :pos) (plist-get b :pos)))
                                         mentions))
                       ;; Enrich mentions with heading context and preview
                       ;; (context type filtering now happens inside this function)
                       (enriched (vulpea-ui--enrich-backlink-mentions
                                  path sorted-mentions target-id)))
                  (when enriched
                    (cl-incf filtered-count (length enriched))
                    (push (list :file-note file-note
                                :path path
                                :mentions enriched)
                          result))))))
          ;; Sort groups according to configuration
          (list :groups (vulpea-ui--sort-backlink-groups result)
                :filtered-count filtered-count
                :total-count total-count))))))

(defun vulpea-ui--sort-backlink-groups (groups)
  "Sort GROUPS according to `vulpea-ui-backlinks-sort'."
  (pcase vulpea-ui-backlinks-sort
    ('nil groups)
    ('title-asc (seq-sort (lambda (a b)
                            (string< (or (vulpea-note-title (plist-get a :file-note)) "")
                                     (or (vulpea-note-title (plist-get b :file-note)) "")))
                          groups))
    ('title-desc (seq-sort (lambda (a b)
                             (string> (or (vulpea-note-title (plist-get a :file-note)) "")
                                      (or (vulpea-note-title (plist-get b :file-note)) "")))
                           groups))
    ((pred functionp) (seq-sort vulpea-ui-backlinks-sort groups))
    (_ groups)))

(defun vulpea-ui--enrich-backlink-mentions (path mentions target-id)
  "Enrich MENTIONS with heading context and preview from file at PATH.
TARGET-ID is the ID of the note being linked to (for prose context extraction).
Filters by `vulpea-ui-backlinks-context-types' BEFORE expensive operations.
Deduplicates mentions with identical heading-path and preview text."
  (when (and path mentions)
    (with-temp-buffer
      (insert-file-contents path)
      (vulpea-ui--setup-org-mode)
      ;; First pass: detect context types (cheap) and filter early
      (let* ((mentions-with-type
              (seq-map
               (lambda (mention)
                 (let* ((pos (plist-get mention :pos))
                        (line (save-excursion
                                (goto-char pos)
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))))
                        (context-type (vulpea-ui--detect-context-type pos line)))
                   (list :pos pos :context-type context-type)))
               mentions))
             ;; Filter by context type BEFORE expensive operations
             (filtered
              (if (eq vulpea-ui-backlinks-context-types t)
                  mentions-with-type
                (seq-filter
                 (lambda (m)
                   (memq (plist-get m :context-type)
                         vulpea-ui-backlinks-context-types))
                 mentions-with-type))))
        ;; Only parse headings if we have any filtered mentions
        (when filtered
          (let ((headings (vulpea-ui--parse-all-headings))
                (seen (make-hash-table :test 'equal))
                (result nil))
            (dolist (mention filtered)
              (let* ((pos (plist-get mention :pos))
                     (heading-path (vulpea-ui--find-heading-path headings pos))
                     (preview (when vulpea-ui-backlinks-show-preview
                                (vulpea-ui--extract-preview pos target-id)))
                     ;; Create dedup key from heading-path and preview text
                     (preview-text (when preview (plist-get preview :text)))
                     (dedup-key (list heading-path preview-text)))
                ;; Only add if we haven't seen this heading+preview combo
                (unless (gethash dedup-key seen)
                  (puthash dedup-key t seen)
                  (push (list :pos pos
                              :heading-path heading-path
                              :preview preview)
                        result))))
            (nreverse result)))))))

(defun vulpea-ui--parse-all-headings ()
  "Parse all headings in current buffer.
Returns list of (:title :level :begin :end) plists sorted by position."
  (let ((headings nil)
        (archive-tag org-archive-tag))
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (hl)
        (unless (vulpea-ui--heading-archived-p hl archive-tag)
          (push (list :title (org-element-property :raw-value hl)
                      :level (org-element-property :level hl)
                      :begin (org-element-property :begin hl)
                      :end (org-element-property :end hl))
                headings))))
    (seq-sort (lambda (a b) (< (plist-get a :begin) (plist-get b :begin)))
              headings)))

(defun vulpea-ui--find-heading-path (headings pos)
  "Find the heading path for position POS given HEADINGS.
Returns a list of heading titles from outermost to innermost."
  (let ((path nil)
        (current-level 0))
    (dolist (h headings)
      (let ((begin (plist-get h :begin))
            (end (plist-get h :end))
            (level (plist-get h :level))
            (title (plist-get h :title)))
        (when (and (<= begin pos) (< pos end))
          ;; This heading contains our position
          (cond
           ;; New top-level heading, reset path
           ((= level 1)
            (setq path (list title)
                  current-level 1))
           ;; Deeper heading, add to path
           ((> level current-level)
            (setq path (append path (list title))
                  current-level level))
           ;; Same or shallower level, replace at this level
           ((<= level current-level)
            (setq path (append (seq-take path (1- level)) (list title))
                  current-level level))))))
    path))

(defun vulpea-ui--extract-preview (pos target-id)
  "Extract preview info around POS in current buffer.
TARGET-ID is the ID of the note being linked to.
Returns a plist with :type and type-specific content:
  - :type meta    -> :key :value
  - :type header  -> :text
  - :type table   -> :text
  - :type list    -> :text
  - :type quote   -> :text
  - :type code    -> :text
  - :type footnote -> :text
  - :type prose   -> :text"
  (save-excursion
    (goto-char pos)
    (let* ((line (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
           (context-type (vulpea-ui--detect-context-type pos line)))
      (pcase context-type
        ('meta (vulpea-ui--extract-meta pos line))
        ('header (vulpea-ui--extract-header line))
        ('table (vulpea-ui--extract-table pos))
        ('list (vulpea-ui--extract-list line))
        ('quote (vulpea-ui--extract-quote line))
        ('code (vulpea-ui--extract-code line))
        ('footnote (vulpea-ui--extract-footnote line))
        (_ (vulpea-ui--extract-prose pos target-id))))))

(defun vulpea-ui--detect-context-type (pos line)
  "Detect the context type at POS given LINE content."
  (cond
   ;; Meta block: - key :: value
   ((string-match-p "^[ \t]*- [^:]+[ \t]+::" line) 'meta)
   ;; Header: starts with *
   ((string-match-p "^\\*+ " line) 'header)
   ;; Table: starts with |
   ((string-match-p "^[ \t]*|" line) 'table)
   ;; Quote block: check if inside #+BEGIN_QUOTE
   ((vulpea-ui--inside-block-p pos "QUOTE") 'quote)
   ;; Code/src block
   ((or (vulpea-ui--inside-block-p pos "SRC")
        (vulpea-ui--inside-block-p pos "EXAMPLE"))
    'code)
   ;; Footnote: [fn:...]
   ((string-match-p "^\\[fn:" line) 'footnote)
   ;; List item (non-meta): - item or + item or 1. item
   ((string-match-p "^[ \t]*[-+*] [^:]" line) 'list)
   ((string-match-p "^[ \t]*[0-9]+[.)] " line) 'list)
   ;; Default: prose
   (t 'prose)))

(defun vulpea-ui--inside-block-p (pos block-type)
  "Return non-nil if POS is inside a block of BLOCK-TYPE."
  (save-excursion
    (goto-char pos)
    (let ((case-fold-search t)
          (begin-re (format "^[ \t]*#\\+BEGIN_%s" block-type))
          (end-re (format "^[ \t]*#\\+END_%s" block-type)))
      (and (re-search-backward begin-re nil t)
           (progn
             (re-search-forward end-re nil t)
             (> (point) pos))))))

(defun vulpea-ui--extract-meta (_pos line)
  "Extract meta block info from LINE."
  (when (string-match "^[ \t]*- \\([^:]+\\)[ \t]+:: *\\(.*\\)$" line)
    (let ((key (string-trim (match-string 1 line)))
          (value (vulpea-ui--clean-org-links (match-string 2 line))))
      (list :type 'meta :key key :value value))))

(defun vulpea-ui--extract-header (line)
  "Extract header info from LINE."
  (when (string-match "^\\*+ \\(.*\\)$" line)
    (list :type 'header
          :text (vulpea-ui--clean-org-links (match-string 1 line)))))

(defun vulpea-ui--extract-table (pos)
  "Extract table cell info around POS."
  (save-excursion
    (goto-char pos)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      ;; Find which cell contains the link
      (let ((cells (split-string line "|" t "[ \t]*")))
        (list :type 'table
              :text (vulpea-ui--clean-org-links
                     (string-join cells " | ")))))))

(defun vulpea-ui--extract-list (line)
  "Extract list item info from LINE."
  (let ((text (replace-regexp-in-string
               "^[ \t]*[-+*] \\|^[ \t]*[0-9]+[.)] "
               ""
               line)))
    (list :type 'list
          :text (vulpea-ui--clean-org-links (string-trim text)))))

(defun vulpea-ui--extract-quote (line)
  "Extract quote info from LINE."
  (list :type 'quote
        :text (vulpea-ui--clean-org-links (string-trim line))))

(defun vulpea-ui--extract-code (line)
  "Extract code/example info from LINE."
  (list :type 'code
        :text (string-trim line)))

(defun vulpea-ui--extract-footnote (line)
  "Extract footnote info from LINE."
  (let ((text (replace-regexp-in-string "^\\[fn:[^]]*\\] *" "" line)))
    (list :type 'footnote
          :text (vulpea-ui--clean-org-links (string-trim text)))))

(defun vulpea-ui--extract-prose (pos target-id)
  "Extract prose context around POS for link to TARGET-ID."
  (save-excursion
    (goto-char pos)
    ;; Find the paragraph boundaries
    (let* ((para-start (save-excursion
                         (backward-paragraph)
                         (skip-chars-forward " \t\n")
                         (point)))
           (para-end (save-excursion
                       (forward-paragraph)
                       (point)))
           (para-text (buffer-substring-no-properties para-start para-end))
           ;; Find the link position within paragraph
           (link-re (format "\\[\\[id:%s\\]\\(?:\\[[^]]*\\]\\)?\\]" target-id))
           (link-start (when (string-match link-re para-text)
                         (match-beginning 0)))
           (link-end (when link-start (match-end 0))))
      (if link-start
          ;; Extract context around the link (including the link itself)
          (let* ((context-start (max 0 (- link-start vulpea-ui-backlinks-prose-chars-before)))
                 (context-end (min (length para-text)
                                   (+ link-end vulpea-ui-backlinks-prose-chars-after)))
                 (context-text (substring para-text context-start context-end))
                 ;; Clean links and add ellipsis
                 (clean-text (vulpea-ui--clean-org-links context-text))
                 (ellipsis-before (if (> context-start 0) "..." ""))
                 (ellipsis-after (if (< context-end (length para-text)) "..." "")))
            (list :type 'prose
                  :text (format "%s%s%s"
                                ellipsis-before
                                (string-trim clean-text)
                                ellipsis-after)))
        ;; Fallback: just get the line
        (list :type 'prose
              :text (vulpea-ui--clean-org-links
                     (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))))))))

(defun vulpea-ui--clean-org-links (text)
  "Clean org link syntax from TEXT.
This is an internal wrapper around `vulpea-ui-clean-org-markup'."
  (vulpea-ui-clean-org-markup text))

(defun vulpea-ui--render-backlink-group (group)
  "Render a backlink GROUP with file note and mentions."
  (let* ((file-note (plist-get group :file-note))
         (mentions (plist-get group :mentions))
         (path (plist-get group :path))
         ;; Group mentions by heading path
         (grouped (vulpea-ui--group-mentions-by-heading mentions)))
    (vui-vstack
     :spacing 0
     ;; File-level note link
     (if file-note
         (vui-component 'vulpea-ui-note-link :note file-note)
       (vui-muted (file-name-nondirectory path)))
     ;; Mentions grouped by heading
     (when grouped
       (vui-vstack
        :spacing 1
        :indent 2
        (seq-map (lambda (hg) (vulpea-ui--render-heading-group hg path))
                 grouped))))))

(defun vulpea-ui--group-mentions-by-heading (mentions)
  "Group MENTIONS by their heading path.
Returns list of (:heading-path :depth :mentions) plists."
  (let ((groups (make-hash-table :test 'equal))
        (order nil))
    (dolist (m mentions)
      (let* ((heading-path (plist-get m :heading-path))
             (key (or heading-path 'file-level)))
        (unless (gethash key groups)
          (push key order))
        (push m (gethash key groups))))
    ;; Build result in original order
    (seq-map (lambda (key)
               (list :heading-path (if (eq key 'file-level) nil key)
                     :depth (if (eq key 'file-level) 0 (length key))
                     :mentions (nreverse (gethash key groups))))
             (nreverse order))))

(defun vulpea-ui--render-heading-group (hg path)
  "Render a heading group HG from file at PATH."
  (let* ((heading-path (plist-get hg :heading-path))
         (depth (plist-get hg :depth))
         (mentions (plist-get hg :mentions))
         ;; Extra indent for nested headings (2 per level after first)
         (heading-indent (if (> depth 1) (* (1- depth) 2) 0))
         ;; Only show the last heading in the path (cleaned of org links)
         (display-heading (when heading-path
                            (vulpea-ui--clean-org-links (car (last heading-path))))))
    (vui-vstack
     :spacing 0
     :indent heading-indent
     ;; Heading text (if any) - bold, not clickable
     (when display-heading
       (vui-text display-heading :face 'vulpea-ui-backlink-heading-face))
     ;; Mentions under this heading
     (vui-vstack
      :spacing 0
      :indent (if display-heading 2 0)
      (seq-map (lambda (m) (vulpea-ui--render-backlink-mention m path))
               mentions)))))

(defun vulpea-ui--render-backlink-mention (mention path)
  "Render a single backlink MENTION from file at PATH.
Renders the preview as a clickable button to jump to the mention."
  (let* ((preview (plist-get mention :preview))
         (pos (plist-get mention :pos)))
    (when preview
      (vulpea-ui--render-preview-button preview path pos))))

(defun vulpea-ui--render-preview-button (preview path pos)
  "Render PREVIEW as a clickable button to jump to PATH at POS."
  (let ((type (plist-get preview :type))
        (on-click (lambda () (vulpea-ui--jump-to-file-position path pos))))
    (pcase type
      ('meta
       (vui-button (concat "⊢ " (plist-get preview :key) ": "
                           (or (plist-get preview :value) ""))
         :face 'vulpea-ui-backlink-preview-face
         :no-decoration t
         :on-click on-click
         :help-echo nil))
      ('header
       ;; Header context means the link is IN the heading text - make it clickable
       (vui-button (concat "§ " (plist-get preview :text))
         :face 'vulpea-ui-backlink-preview-face
         :no-decoration t
         :on-click on-click
         :help-echo nil))
      ('table
       (vui-button (concat "▤ " (plist-get preview :text))
         :face 'vulpea-ui-backlink-preview-face
         :no-decoration t
         :on-click on-click
         :help-echo nil))
      ('list
       (vui-button (concat "· " (plist-get preview :text))
         :face 'vulpea-ui-backlink-preview-face
         :no-decoration t
         :on-click on-click
         :help-echo nil))
      ('quote
       (vui-button (concat "> " (plist-get preview :text))
         :face 'vulpea-ui-backlink-preview-face
         :no-decoration t
         :on-click on-click
         :help-echo nil))
      ('code
       (vui-button (concat "λ " (plist-get preview :text))
         :face 'vulpea-ui-backlink-preview-face
         :no-decoration t
         :on-click on-click
         :help-echo nil))
      ('footnote
       (vui-button (concat "† " (plist-get preview :text))
         :face 'vulpea-ui-backlink-preview-face
         :no-decoration t
         :on-click on-click
         :help-echo nil))
      (_
       (vui-button (or (plist-get preview :text) "")
         :face 'vulpea-ui-backlink-preview-face
         :no-decoration t
         :on-click on-click
         :help-echo nil)))))

(defun vulpea-ui--jump-to-file-position (path pos)
  "Jump to position POS in file at PATH."
  (when (and path pos)
    (let ((main-win (vulpea-ui--get-main-window)))
      (when main-win
        (select-window main-win)
        (find-file path)
        (goto-char pos)
        (org-fold-show-entry)
        (recenter)))))


;;; Forward links widget

(vui-defcomponent vulpea-ui-widget-links ()
  "Widget displaying notes that the current note links to."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let ((forward-links (vui-use-memo (note)
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
                  (lambda (link-info)
                    (let ((link-note (plist-get link-info :note))
                          (count (plist-get link-info :count)))
                      (vui-hstack
                       :spacing 1
                       (vui-component 'vulpea-ui-note-link :note link-note)
                       (vui-text (format "(%d)" count)
                         :face 'vulpea-ui-widget-count-face))))
                  forward-links))
              (vui-muted "No links"))))))))

(defun vulpea-ui--get-forward-links (note)
  "Get all notes linked from NOTE's file with counts.
Collects links from all headings in the file, not just the current note.
Returns a list of plists with :note and :count, sorted by title."
  (when note
    (let* ((path (vulpea-note-path note))
           ;; Get all notes in this file (file-level and all headings)
           (file-notes (vulpea-db-query-by-file-paths (list path)))
           ;; Collect all links from all notes
           (all-links (seq-mapcat #'vulpea-note-links file-notes))
           ;; Filter to id links only
           (id-links (seq-filter (lambda (link)
                                   (equal "id" (plist-get link :type)))
                                 all-links))
           ;; Count occurrences of each destination ID
           (id-counts (make-hash-table :test 'equal)))
      (dolist (link id-links)
        (let ((dest (plist-get link :dest)))
          (puthash dest (1+ (gethash dest id-counts 0)) id-counts)))
      ;; Fetch notes and build result with counts
      (let* ((ids (hash-table-keys id-counts))
             (notes (when ids (vulpea-db-query-by-ids ids)))
             (result (seq-map (lambda (n)
                                (list :note n
                                      :count (gethash (vulpea-note-id n) id-counts)))
                              notes)))
        ;; Sort by title
        (seq-sort (lambda (a b)
                    (string< (or (vulpea-note-title (plist-get a :note)) "")
                             (or (vulpea-note-title (plist-get b :note)) "")))
                  result)))))


;;; Root component

(vui-defcomponent vulpea-ui-sidebar-content ()
  "Content component for the sidebar (uses context)."
  :render
  (let ((note (use-vulpea-ui-note)))
    (if note
        (let ((widgets (vulpea-ui--get-widgets-for-note note)))
          (vui-vstack
           :spacing 1
           (seq-map (lambda (widget-sym)
                      (vui-component widget-sym :key widget-sym))
                    widgets)))
      (vui-muted "No vulpea note selected"))))

(vui-defcomponent vulpea-ui-sidebar-root (note)
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
         (original-window (selected-window))
         (existing-instance (gethash frame vulpea-ui--sidebar-instances)))
    ;; Select sidebar window before mount (vui-mount calls switch-to-buffer)
    (when sidebar-win
      (select-window sidebar-win t))
    (with-current-buffer buf
      (if (and existing-instance
               (vui-instance-buffer existing-instance)
               (buffer-live-p (vui-instance-buffer existing-instance)))
          ;; Reuse existing instance - update props, invalidate memos, re-render
          (vui-update existing-instance (list :note note))
        ;; Mount fresh - first render or instance was lost
        (let ((new-instance
               (vui-mount
                (vui-component 'vulpea-ui-sidebar-root :note note)
                buf-name)))
          (puthash frame new-instance vulpea-ui--sidebar-instances)))
      ;; Set current note AFTER render (vui-mount kills local variables)
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


;;; Built-in widget registration

(vulpea-ui-register-widget 'stats
                           :component 'vulpea-ui-widget-stats
                           :order 100)

(vulpea-ui-register-widget 'outline
                           :component 'vulpea-ui-widget-outline
                           :order 200)

(vulpea-ui-register-widget 'backlinks
                           :component 'vulpea-ui-widget-backlinks
                           :order 300)

(vulpea-ui-register-widget 'links
                           :component 'vulpea-ui-widget-links
                           :order 400)

(provide 'vulpea-ui)
;;; vulpea-ui.el ends here
