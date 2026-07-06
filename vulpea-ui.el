;;; vulpea-ui.el --- Sidebar infrastructure and widget framework for vulpea notes -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Boris Buliga
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/vulpea-ui
;; Version: 1.2.0
;; Package-Requires: ((emacs "29.1") (vulpea "2.5.0") (vui "1.3.0"))
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
;; - Default widgets: outline, backlinks, unlinked mentions, forward
;;   links, stats
;; - Easy API for creating custom widgets
;; - Standalone views: schema dashboard, collection view (a sortable
;;   table over a filtered set of notes with marks and bulk actions)
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
(require 'transient)
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
  "Size of the sidebar window.
This is the width when `vulpea-ui-sidebar-position' is \\='left or
\\='right, or the height when it is \\='top or \\='bottom.  It can be an
integer, a floating-point number, and more.  See the window height and
window width entries of Info node `(elisp) Buffer Display Action
Alists'."
  :type '(choice (float :tag "Fraction of frame (0.0-1.0)")
                 (integer :tag "Total lines or columns")
                 (cons :tag "Body size"
                       (choice (const body-lines) (const body-columns))
                       integer)
                 (function :tag "Window-adjusting function"))
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

(defcustom vulpea-ui-unlinked-mentions-note-filter #'identity
  "Function to filter which notes appear in the unlinked mentions widget.
Called with the mentioning vulpea-note and should return non-nil to
include it.  This is a presentation-only filter; to exclude notes from
the search itself (collection-wide) use `vulpea-mentions-note-filter'.
A common recipe is to hide notes carrying a particular tag, e.g.

  (setq vulpea-ui-unlinked-mentions-note-filter
        (lambda (note)
          (not (member \"index\" (vulpea-note-tags note)))))"
  :type 'function
  :group 'vulpea-ui)

(defcustom vulpea-ui-outgoing-mentions-note-filter #'identity
  "Function to filter which notes appear in the outgoing mentions widget.
Called with the candidate vulpea-note (the note you could link to) and
should return non-nil to include it.  This is a presentation-only filter;
to exclude notes from the search itself (collection-wide) use
`vulpea-mentions-note-filter'.  A common recipe is to hide notes carrying
a particular tag, e.g.

  (setq vulpea-ui-outgoing-mentions-note-filter
        (lambda (note)
          (not (member \"index\" (vulpea-note-tags note)))))"
  :type 'function
  :group 'vulpea-ui)

(defcustom vulpea-ui-fast-parse nil
  "Use fast `org-mode' initialization for parsing.
When non-nil, skip mode hooks when parsing org files for headings
and backlinks.  This can significantly improve performance but may
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

(defface vulpea-ui-mention-context-face
  '((t :inherit shadow))
  "Face for the context line of an unlinked mention."
  :group 'vulpea-ui)

(defface vulpea-ui-mention-action-face
  '((t :inherit link :underline nil))
  "Face for the link action buttons in the outgoing mentions widget."
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
  (when (fboundp 'mode-line-invisible-mode)
    (mode-line-invisible-mode 1)))


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

(defvar-local vulpea-ui--refresh-generation 0
  "Counter bumped on each explicit sidebar refresh.
Async widgets fold this into their `vui-use-async' cache key so that a
manual or save-triggered refresh (`vulpea-ui-sidebar-refresh', which
invalidates memos) re-runs them, while an idle soft-refresh
\(`vui-update-props', which does not bump it) reuses the cached result.")

(defun vulpea-ui--sidebar-buffer-name (&optional frame)
  "Return the sidebar buffer name for FRAME.
If FRAME is nil, use the selected frame."
  (let ((frame (or frame (selected-frame))))
    (format "*vulpea-ui-sidebar:%s*" (or (frame-parameter frame 'window-id) ""))))

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

(defun vulpea-ui--ensure-side-slot (slots side)
  "Return SLOTS with SIDE guaranteed at least one available slot.
SLOTS is a value shaped like `window-sides-slots': a list of four
elements limiting the number of side windows on the left, top, right
and bottom of a frame.  A nil element means an unlimited number of
slots and is left untouched.  A numeric element below one is raised to
one so `display-buffer-in-side-window' will not refuse to create the
sidebar window.  SIDE is one of `left', `top', `right' or `bottom'.
The input SLOTS is not modified."
  (let ((idx (pcase side
               ('left 0) ('top 1) ('right 2) ('bottom 3)
               (_ (error "Invalid side: %S" side))))
        (slots (copy-sequence slots)))
    (let ((cur (nth idx slots)))
      (when (and (numberp cur) (< cur 1))
        (setf (nth idx slots) 1)))
    slots))

(defun vulpea-ui--create-sidebar-window (buffer)
  "Create a sidebar window for BUFFER using side window mechanics.
The configured side is guaranteed at least one slot in a local copy of
`window-sides-slots', so the sidebar is still displayed when the user
has disabled side windows on that side.  Otherwise
`display-buffer-in-side-window' would return nil and the sidebar
buffer would clobber the selected window."
  (let ((window-sides-slots
         (vulpea-ui--ensure-side-slot window-sides-slots
                                      vulpea-ui-sidebar-position)))
    (display-buffer-in-side-window buffer (vulpea-ui--display-buffer-params))))

(defun vulpea-ui--get-main-window (&optional frame)
  "Get the most recently used main window in FRAME.
A main window is a live, non-minibuffer window that is neither the
sidebar nor any other side window.  Side windows created via
`display-buffer-in-side-window' (e.g. a *Help* buffer pinned to a side
by the user's `display-buffer-alist') are skipped: they are never the
main editing window, so focusing one must not be mistaken for switching
away from the vulpea note.  Treating such a window as the main one made
`vulpea-ui--on-buffer-change' auto-hide and then re-show the sidebar on
every focus change, which under `window-combination-resize' steadily
shrank the note window (see vulpea-ui#36)."
  (let* ((frame (or frame (selected-frame)))
         (sidebar-win (vulpea-ui--get-sidebar-window frame))
         (selected (frame-selected-window frame))
         (mainp (lambda (win)
                  (and (not (eq win sidebar-win))
                       (not (window-parameter win 'window-side))
                       (not (window-minibuffer-p win))))))
    ;; Prefer the currently selected window if it's a valid main window
    (if (and selected (funcall mainp selected))
        selected
      ;; Fallback to first valid window
      (or (seq-find mainp (window-list frame nil))
          (frame-first-window frame)))))


;;; Content tracking

(defvar-local vulpea-ui--current-note nil
  "The vulpea note currently being displayed in the sidebar.")

(defun vulpea-ui--get-note-from-buffer (buffer)
  "Get the vulpea note anchoring the sidebar for BUFFER, or nil.
Prefers the file-level note, regardless of the entry at point, so the
whole-file widgets (outline, backlinks, stats) stay file-scoped.  When
the file has no file-level ID - a heading-only file, where only
headings carry IDs - falls back to the earliest heading-level note by
position, so the sidebar still anchors on the file and the schema widget
can report on its headings.  The fallback only runs when there is no
file-level ID, so files with one behave exactly as before."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (let ((file-id (save-excursion
                         (goto-char (point-min))
                         (org-entry-get nil "ID"))))
          (if file-id
              ;; File has a file-level ID: resolve it, or nil when the DB
              ;; has not caught up yet.  Never fall through to a heading -
              ;; that would anchor the whole-file widgets on a heading and
              ;; break their file scope.  The gate is the ID's presence in
              ;; the buffer, not whether the DB currently resolves it.
              (vulpea-db-get-by-id file-id)
            ;; Heading-only file (IDs only on headings): anchor on the
            ;; earliest note by position, so the sidebar still lights up.
            ;; Match vulpea's stored path convention (`expand-file-name',
            ;; not a truename) so the query finds the file's notes.
            (when (and buffer-file-name
                       (fboundp 'vulpea-db-query-by-file-path))
              (car (sort (vulpea-db-query-by-file-path
                          (expand-file-name buffer-file-name))
                         (lambda (a b)
                           (< (or (vulpea-note-pos a) 0)
                              (or (vulpea-note-pos b) 0))))))))))))

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
  "Hide the sidebar window in FRAME without killing the buffer.
Only an actual side window is deleted.  When the sidebar buffer is
displayed in a regular window (for example because it could not be
shown in a side window), the window is left untouched rather than
risking deletion of a main or sole window."
  (let ((win (vulpea-ui--get-sidebar-window frame)))
    (when (and (window-live-p win)
               (window-parameter win 'window-side))
      (delete-window win))))

(defun vulpea-ui--show-sidebar-window (&optional frame)
  "Show the sidebar window in FRAME."
  (let ((buf (vulpea-ui--get-sidebar-buffer frame)))
    (when (and buf (not (vulpea-ui--sidebar-visible-p frame)))
      (vulpea-ui--create-sidebar-window buf))))

(defvar vulpea-ui--worker-refresh-pending nil
  "Non-nil when background extraction changed data during a busy burst.
Flushed into a single refresh when the worker goes idle, so a bulk
sync produces one refresh instead of thousands.")

(defun vulpea-ui--setup-hooks ()
  "Set up hooks for sidebar content tracking."
  (add-hook 'window-buffer-change-functions #'vulpea-ui--on-buffer-change)
  (add-hook 'window-selection-change-functions #'vulpea-ui--on-buffer-change)
  ;; Auto-refresh hooks
  (when vulpea-ui-auto-refresh
    (add-hook 'after-save-hook #'vulpea-ui--on-save)
    ;; Async extraction (vulpea 2.6+): refresh when background
    ;; results actually land in the database
    (when (boundp 'vulpea-db-worker-done-functions)
      (add-hook 'vulpea-db-worker-done-functions
                #'vulpea-ui--on-worker-done))
    (vulpea-ui--start-idle-timer)))

(defun vulpea-ui--teardown-hooks ()
  "Remove hooks for sidebar content tracking."
  (remove-hook 'window-buffer-change-functions #'vulpea-ui--on-buffer-change)
  (remove-hook 'window-selection-change-functions #'vulpea-ui--on-buffer-change)
  ;; Auto-refresh hooks
  (remove-hook 'after-save-hook #'vulpea-ui--on-save)
  (when (boundp 'vulpea-db-worker-done-functions)
    (remove-hook 'vulpea-db-worker-done-functions
                 #'vulpea-ui--on-worker-done))
  (setq vulpea-ui--worker-refresh-pending nil)
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

(defun vulpea-ui--on-worker-done (_path status _count)
  "Refresh the sidebar when background extraction lands data.

With vulpea's async extraction, the database changes when the worker
applies results - not when a file is saved - so the save-triggered
refresh shows data from before the save.  This handler runs on
vulpea's worker done hook: STATUS `applied' and `missing' mean the
database changed; the refresh is deferred while the worker is busy
\(bulk syncs) and flushed once when the burst drains."
  (when (memq status '(applied missing))
    (setq vulpea-ui--worker-refresh-pending t))
  (when (and vulpea-ui--worker-refresh-pending
             (or (not (fboundp 'vulpea-db-worker-busy-p))
                 (not (vulpea-db-worker-busy-p))))
    (setq vulpea-ui--worker-refresh-pending nil)
    (when (vulpea-ui--sidebar-visible-p)
      (vulpea-ui-sidebar-refresh))))

(defun vulpea-ui--on-idle ()
  "Handle idle timeout - soft refresh preserving memos.
Only widgets whose deps changed (e.g. `buffer-modified-tick' for
stats and outline) will recompute."
  (when (vulpea-ui--sidebar-visible-p)
    (let* ((frame (selected-frame))
           (main-win (vulpea-ui--get-main-window frame))
           (main-buf (when main-win (window-buffer main-win)))
           (note (vulpea-ui--get-note-from-buffer main-buf))
           (instance (gethash frame vulpea-ui--sidebar-instances)))
      (when (and note instance
                 (vui-instance-buffer instance)
                 (buffer-live-p (vui-instance-buffer instance)))
        (with-current-buffer (vui-instance-buffer instance)
          (setq vulpea-ui--current-note note))
        (vui-update-props instance (list :note note))))))


;;; Utility functions

(defun vulpea-ui--setup-org-mode ()
  "Set up `org-mode' for parsing, respecting `vulpea-ui-fast-parse'.
When fast parsing is enabled, skip mode hooks for better performance.
All startup actions (inline images, LaTeX previews, visibility
cycling) are inhibited since these buffers are used only for
parsing.  Using `org-inhibit-startup' prevents `org-mode' from
processing buffer-level #+STARTUP keywords (e.g. inlineimages)
which would otherwise override let-bound variable suppression."
  (let ((org-inhibit-startup t))
    (if vulpea-ui-fast-parse
        (delay-mode-hooks (org-mode))
      (org-mode))))

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
  (vui-activate))

(defun vulpea-ui-widget-toggle-at-point ()
  "Toggle the widget collapse state at point."
  (interactive)
  (vulpea-ui-follow-link-at-point))


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
      (let* ((note-buf (when (vulpea-note-path note)
                         (find-buffer-visiting (vulpea-note-path note))))
             (tick (when note-buf (buffer-modified-tick note-buf)))
             (stats (vui-use-memo (note tick)
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
      (let* ((note-buf (when (vulpea-note-path note)
                         (find-buffer-visiting (vulpea-note-path note))))
             (tick (when note-buf (buffer-modified-tick note-buf)))
             (headings (vui-use-memo (note tick)
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
          (max-depth vulpea-ui-outline-max-depth)
          (existing-buf (find-buffer-visiting (vulpea-note-path note))))
      (with-temp-buffer
        (if existing-buf
            (insert (with-current-buffer existing-buf
                      (buffer-substring-no-properties (point-min) (point-max))))
          (insert-file-contents path))
        (vulpea-ui--setup-org-mode)
        (let ((headings nil)
              (archive-tag org-archive-tag))
          (org-element-map (org-element-parse-buffer 'headline) 'headline
            (lambda (hl)
              (let ((level (org-element-property :level hl))
                    (title (vulpea-ui--clean-org-links
                            (org-element-property :raw-value hl)))
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
    (let ((indicator (pcase type
                       ('meta "⊢")
                       ('header "§")
                       ('table "▤")
                       ('list "·")
                       ('quote ">")
                       ('code "λ")
                       ('footnote "†")
                       (_ nil)))
          (text (pcase type
                  ('meta (concat (plist-get preview :key) ": "
                                 (or (plist-get preview :value) "")))
                  (_ (or (plist-get preview :text) "")))))
      (if indicator
          (vui-hstack
           :spacing 1
           (vui-text indicator :face 'vulpea-ui-backlink-context-face)
           (vui-button text
             :face 'vulpea-ui-backlink-preview-face
             :no-decoration t
             :on-click on-click
             :help-echo nil))
        (vui-button text
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


;;; Unlinked mentions widget

(vui-defcomponent vulpea-ui-widget-unlinked-mentions ()
  "Widget displaying notes that mention the current note without linking.

An unlinked mention is a place where another note writes this note's
title or one of its aliases as plain text, with no `id:' link pointing
back.  The search is delegated to `vulpea-note-unlinked-mentions-async'
\(ripgrep-backed) and runs asynchronously, so the widget shows a loading
state and fills in when the results arrive.  Results are cached until the
note changes or the sidebar is refreshed via `vulpea-ui-sidebar-refresh'.

This is the sidebar's first asynchronous widget; it relies on ripgrep
being available on `exec-path' and reports gracefully when it is not."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let* ((path (vulpea-note-path note))
             (last-ref (vui-use-ref nil))
             (result (vui-use-async
                         (list (vulpea-note-id note)
                               vulpea-ui--refresh-generation)
                       (apply-partially
                        #'vulpea-note-unlinked-mentions-async note)))
             (status (plist-get result :status))
             (fresh (when (eq status 'ready)
                      (vulpea-ui--filter-mentions
                       (plist-get result :data)
                       vulpea-ui-unlinked-mentions-note-filter)))
             (decision (vulpea-ui--mentions-display-data
                        status (vulpea-note-id note) fresh last-ref))
             (state (car decision))
             (data (cdr decision)))
        (vui-component 'vulpea-ui-widget
          :title "Unlinked Mentions"
          :count (when (eq state 'shown) (length data))
          :children
          (lambda ()
            (pcase state
              ('shown (vulpea-ui--render-unlinked-mentions-body data path))
              ('error
               (vui-muted (format "Unavailable: %s"
                                  (plist-get result :error))))
              (_ (vui-muted "Searching…")))))))))

(defun vulpea-ui--render-unlinked-mentions-body (data path)
  "Render incoming mention DATA as grouped context lines for PATH."
  (let ((groups (vulpea-ui--group-mentions data)))
    (if groups
        (vui-vstack
         :spacing 1
         (seq-map (lambda (group)
                    (vulpea-ui--render-mention-group group path))
                  groups))
      (vui-muted "No unlinked mentions"))))

(defun vulpea-ui--filter-mentions (mentions filter)
  "Return MENTIONS whose :note satisfies FILTER.
FILTER is a predicate on a `vulpea-note'.  Mentions with a nil :note are
dropped, since they cannot be grouped or acted on."
  (seq-filter (lambda (m)
                (let ((note (plist-get m :note)))
                  (and note (funcall filter note))))
              mentions))

(defun vulpea-ui--mentions-display-data (status note-id fresh last-ref)
  "Decide which mention data to render and keep LAST-REF in sync.

STATUS is the `vui-use-async' status for NOTE-ID.  FRESH is the freshly
loaded, already-filtered data, meaningful when STATUS is `ready'.
LAST-REF is a ref (a cons, see `vui-use-ref') holding (NOTE-ID . DATA)
from the previous successful render.

Returns one of:
  (shown . DATA)  render DATA - the fresh data on `ready', or the cached
                  data for the same note while a re-scan is `pending', so
                  the list (and point) stay put instead of blanking to the
                  loading state and throwing point to the top;
  (error)         render the error state;
  (loading)       render the loading state (nothing cached for this note).

Cached data is reused only for a matching NOTE-ID, so switching notes
never briefly shows another note's mentions."
  (pcase status
    ('ready
     (setcar last-ref (cons note-id fresh))
     (cons 'shown fresh))
    ('error '(error))
    (_ (let ((prev (car last-ref)))
         (if (and prev (equal (car prev) note-id))
             (cons 'shown (cdr prev))
           '(loading))))))

(defun vulpea-ui--group-mentions (mentions)
  "Group MENTIONS by their mentioning note.

MENTIONS is the list resolved by `vulpea-note-unlinked-mentions-async':
each is a plist with :note (the mentioning `vulpea-note'), :path, :line,
and :context.

Returns a list of group plists, one per mentioning note in
first-encounter order, each with :note, :path, and :mentions - a list of
\(:line :context) plists kept in their original order."
  (let ((meta (make-hash-table :test 'equal))
        (lists (make-hash-table :test 'equal))
        (order nil))
    (dolist (m mentions)
      (let* ((note (plist-get m :note))
             (id (and note (vulpea-note-id note))))
        (when id
          (unless (gethash id meta)
            (push id order)
            (puthash id (list :note note :path (plist-get m :path)) meta))
          (push (list :line (plist-get m :line)
                      :context (plist-get m :context))
                (gethash id lists)))))
    (mapcar (lambda (id)
              (let ((info (gethash id meta)))
                (list :note (plist-get info :note)
                      :path (plist-get info :path)
                      :mentions (nreverse (gethash id lists)))))
            (nreverse order))))

(defun vulpea-ui--render-mention-group (group source-path)
  "Render a mention GROUP: the mentioning note link and its context lines.

For each note link, there is an ignore button, which adds the mentioning
note id to the value of `vulpea-mentions-per-note-ignore-property-key'
in SOURCE-PATH."
  (let ((note (plist-get group :note))
        (path (plist-get group :path))
        (mentions (plist-get group :mentions)))
    (vui-vstack
     :spacing 0
     (vui-hstack
      (if note
          (vui-component 'vulpea-ui-note-link :note note)
        (vui-muted (file-name-nondirectory path)))
      (vui-button "ignore"
        :face 'vulpea-ui-mention-action-face
        :on-click (lambda ()
                    (vulpea-ui--ignore-mentions-from-note source-path note))
        :help-echo "Ignore mentions from this note."))
     (vui-vstack
      :spacing 0
      :indent 2
      (seq-map (lambda (m) (vulpea-ui--render-mention m path)) mentions)))))

(defun vulpea-ui--render-mention (mention path)
  "Render a single MENTION from PATH as a clickable context line.
Clicking jumps to the mention's line in the main window."
  (let ((line (plist-get mention :line))
        (context (plist-get mention :context)))
    (vui-button context
      :face 'vulpea-ui-mention-context-face
      :no-decoration t
      :on-click (lambda () (vulpea-ui--jump-to-file-line path line))
      :help-echo nil)))

(defun vulpea-ui--jump-to-file-line (path line)
  "Jump to LINE in the file at PATH in the main window."
  (when (and path line)
    (let ((main-win (vulpea-ui--get-main-window)))
      (when main-win
        (select-window main-win)
        (find-file path)
        (goto-char (point-min))
        (forward-line (1- line))
        (org-fold-show-entry)
        (recenter)))))

(defun vulpea-ui--ignore-mentions-from-note (path note)
  "Add NOTE id to the per note mention ignore property of note file PATH."
  (when-let ((buffer (find-buffer-visiting path))
             (id (vulpea-note-id note)))
    ;; Search for the property in the buffer
    (with-current-buffer buffer
      (goto-char (point-min))
      ;; If the property already exists, append the note id if no duplicates.
      (if (search-forward-regexp
           (rx line-start
               (eval (format ":%s:" vulpea-mentions-per-note-ignore-property-key)))
           nil t)
          (unless (search-forward (format "%s" id) nil t)
            (goto-char (pos-eol))
            (insert (format " %s" id)))
        ;; If the property is absent, insert it after the file level ID property
        (when (search-forward-regexp (rx line-start ":ID:") nil t)
          (goto-char (pos-eol))
          (insert (format "\n:%s: %s"
                          vulpea-mentions-per-note-ignore-property-key
                          id)))))))

;;; Outgoing mentions widget

(vui-defcomponent vulpea-ui-widget-outgoing-mentions ()
  "Widget displaying notes the current note mentions but does not link to.

An outgoing unlinked mention is a place where this note writes another
note's title or alias as plain text, with no `id:' link to it - a note
you may want to link out to.  The search is delegated to
`vulpea-buffer-unlinked-mentions-async' (ripgrep-backed) and runs over
the note's live buffer, so unsaved edits are included.  Like the unlinked
mentions widget it loads asynchronously, showing a loading state until
results arrive, and caches them until the note changes or the sidebar is
refreshed via `vulpea-ui-sidebar-refresh'.

It relies on ripgrep being available on `exec-path' and reports
gracefully when it is not, or when the note's file is not visited in a
buffer."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let* ((path (vulpea-note-path note))
             (buffer (and path (find-buffer-visiting path)))
             (last-ref (vui-use-ref nil))
             (result (vui-use-async
                         (list (vulpea-note-id note)
                               vulpea-ui--refresh-generation)
                       (lambda (resolve reject)
                         (if (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (vulpea-buffer-unlinked-mentions-async
                                resolve reject))
                           (funcall reject "note buffer is not open")))))
             (status (plist-get result :status))
             (fresh (when (eq status 'ready)
                      (vulpea-ui--filter-mentions
                       (plist-get result :data)
                       vulpea-ui-outgoing-mentions-note-filter)))
             (decision (vulpea-ui--mentions-display-data
                        status (vulpea-note-id note) fresh last-ref))
             (state (car decision))
             (data (cdr decision)))
        (vui-component 'vulpea-ui-widget
          :title "Outgoing Mentions"
          :count (when (eq state 'shown) (length data))
          :children
          (lambda ()
            (pcase state
              ('shown (vulpea-ui--render-outgoing-mentions-body data path))
              ('error
               (vui-muted (format "Unavailable: %s"
                                  (plist-get result :error))))
              (_ (vui-muted "Searching…")))))))))

(defun vulpea-ui--render-outgoing-mentions-body (data path)
  "Render outgoing mention DATA as grouped suggestions for PATH."
  (let ((groups (vulpea-ui--group-outgoing-mentions data)))
    (if groups
        (vui-vstack
         :spacing 1
         (seq-map (lambda (group)
                    (vulpea-ui--render-outgoing-group group path))
                  groups))
      (vui-muted "No outgoing mentions"))))

(defun vulpea-ui--group-outgoing-mentions (mentions)
  "Group outgoing MENTIONS by the candidate note they could link to.

MENTIONS is the list resolved by `vulpea-buffer-unlinked-mentions-async':
each is a plist with :note (a candidate `vulpea-note' to link to), :line,
:context, and :matched.  All matches are positions in the current note's
own file.

Returns a list of group plists, one per candidate note in first-encounter
order, each with :note and :mentions - a list of (:line :context) plists
kept in their original order.  Mentions without a candidate note are
skipped, and entries that share a note, line and context are
de-duplicated (upstream emits one entry per matched term, so a note's
title and alias hitting the same line would otherwise appear twice)."
  (let ((notes (make-hash-table :test 'equal))
        (lists (make-hash-table :test 'equal))
        (order nil))
    (dolist (m mentions)
      (let* ((note (plist-get m :note))
             (id (and note (vulpea-note-id note))))
        (when id
          (unless (gethash id notes)
            (push id order)
            (puthash id note notes))
          (push (list :line (plist-get m :line)
                      :context (plist-get m :context))
                (gethash id lists)))))
    (mapcar (lambda (id)
              (list :note (gethash id notes)
                    :mentions (delete-dups (nreverse (gethash id lists)))))
            (nreverse order))))

(defun vulpea-ui--render-outgoing-mention (mention note source-path)
  "Render one outgoing MENTION line with a link action.
The leading button converts the occurrence into an =id:= link to NOTE;
the context that follows jumps to the occurrence in SOURCE-PATH."
  (let ((line (plist-get mention :line))
        (context (plist-get mention :context)))
    (vui-hstack
     (vui-button "link"
       :face 'vulpea-ui-mention-action-face
       :on-click (lambda ()
                   (vulpea-ui--link-mention-action source-path line note))
       :help-echo "Insert an id: link at this mention")
     (vui-button context
       :face 'vulpea-ui-mention-context-face
       :no-decoration t
       :on-click (lambda () (vulpea-ui--jump-to-file-line source-path line))
       :help-echo nil))))

(defun vulpea-ui--render-outgoing-group (group source-path)
  "Render an outgoing mention GROUP.

The candidate note is shown as a link to visit it, with a \"link all\"
action that links every occurrence below.  SOURCE-PATH is the current
note's file, where each context line lives and where clicking it jumps to."
  (let ((note (plist-get group :note))
        (mentions (plist-get group :mentions)))
    (vui-vstack
     :spacing 0
     (vui-hstack
      (vui-component 'vulpea-ui-note-link :note note)
      (vui-button "link all"
        :face 'vulpea-ui-mention-action-face
        :on-click (lambda ()
                    (vulpea-ui--link-group-action source-path note mentions))
        :help-echo "Insert id: links for every occurrence below"))
     (vui-vstack
      :spacing 0
      :indent 2
      (seq-map (lambda (m)
                 (vulpea-ui--render-outgoing-mention m note source-path))
               mentions)))))


;;; Linking unlinked mentions

(defconst vulpea-ui--org-link-re
  "\\[\\[[^][]*\\]\\(?:\\[[^][]*\\]\\)?\\]"
  "Regexp matching an Org bracket link: [[target]] or [[target][desc]].
Targets or descriptions containing square brackets are not matched, which
is fine for the =id:= links this widget creates and detects.")

(defun vulpea-ui--note-link-terms (note)
  "Return NOTE's title and aliases as a list of non-empty strings.
These are the texts an outgoing mention may have matched, and the texts
to search for when converting a mention into a link."
  (seq-filter (lambda (s)
                (and (stringp s) (not (string-empty-p (string-trim s)))))
              (cons (vulpea-note-title note) (vulpea-note-aliases note))))

(defun vulpea-ui--line-link-spans (bound)
  "Return a list of (BEG . END) Org link spans between point and BOUND."
  (let ((spans nil))
    (save-excursion
      (while (re-search-forward vulpea-ui--org-link-re bound t)
        (push (cons (match-beginning 0) (match-end 0)) spans)))
    (nreverse spans)))

(defun vulpea-ui--pos-in-spans-p (pos spans)
  "Return non-nil if POS falls within any (BEG . END) span in SPANS."
  (seq-some (lambda (s) (and (>= pos (car s)) (< pos (cdr s)))) spans))

(defun vulpea-ui--link-mention-line (buffer line note)
  "Convert plain-text mentions of NOTE on LINE of BUFFER into id: links.

Searches LINE for NOTE's title and aliases (word-bounded and
case-insensitive, mirroring the ripgrep scan), skips any occurrence
already inside an Org link, and replaces each remaining occurrence with an
=id:= link to NOTE, preserving the matched text as the link description.

Re-validates against the live buffer instead of trusting the cached
position, so a stale or already-linked mention is simply a no-op.  Returns
the number of occurrences linked."
  (with-current-buffer buffer
    (let ((terms (vulpea-ui--note-link-terms note)))
      (if (null terms)
          0
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (let ((line-beg (line-beginning-position))
                (re (concat "\\b\\(?:"
                            (mapconcat #'regexp-quote terms "\\|")
                            "\\)\\b"))
                (case-fold-search t)
                (count 0)
                (scanning t))
            (while scanning
              (goto-char line-beg)
              (let ((spans (vulpea-ui--line-link-spans (line-end-position)))
                    (hit nil))
                (goto-char line-beg)
                (while (and (not hit)
                            (re-search-forward re (line-end-position) t))
                  (let ((beg (match-beginning 0))
                        (end (match-end 0)))
                    (unless (vulpea-ui--pos-in-spans-p beg spans)
                      (setq hit (cons beg end)))))
                (if (not hit)
                    (setq scanning nil)
                  (let* ((beg (car hit))
                         (end (cdr hit))
                         (text (buffer-substring-no-properties beg end))
                         (link (vulpea-utils-link-make-string note text)))
                    (goto-char beg)
                    (delete-region beg end)
                    (insert link)
                    (cl-incf count)))))
            count))))))

(defun vulpea-ui--link-mention-action (path line note)
  "Link occurrences of NOTE on LINE of the file at PATH.
Edits the note's buffer in place and reports the outcome.  Deliberately
does not refresh the sidebar: a refresh re-scans and would reset point to
the top, making it tedious to link several mentions in a row, so point is
left where it is and you press =g= when you want the list to catch up.  A
no-op (e.g. the buffer changed since the scan) is reported instead.
Intended as a mention button action."
  (let ((buffer (and path (find-buffer-visiting path))))
    (if (not (buffer-live-p buffer))
        (message "vulpea-ui: note buffer is not open")
      (let ((n (vulpea-ui--link-mention-line buffer line note)))
        (if (zerop n)
            (message "vulpea-ui: nothing to link here; press g to refresh")
          (message
           "vulpea-ui: linked %d occurrence%s of %s (press g to refresh)"
           n (if (= n 1) "" "s") (vulpea-note-title note)))))))

(defun vulpea-ui--link-group-action (path note mentions)
  "Link every MENTIONS line for NOTE in the file at PATH.
Sums the occurrences linked across all lines and reports the total.  Like
`vulpea-ui--link-mention-action' it leaves the sidebar unrefreshed so
point is preserved; press =g= to update the list.  Intended as the \"link
all\" button action for an outgoing-mention group."
  (let ((buffer (and path (find-buffer-visiting path))))
    (if (not (buffer-live-p buffer))
        (message "vulpea-ui: note buffer is not open")
      (let ((total 0))
        (dolist (m mentions)
          (cl-incf total (vulpea-ui--link-mention-line
                          buffer (plist-get m :line) note)))
        (if (zerop total)
            (message "vulpea-ui: nothing to link for %s; press g to refresh"
                     (vulpea-note-title note))
          (message
           "vulpea-ui: linked %d occurrence%s of %s (press g to refresh)"
           total (if (= total 1) "" "s") (vulpea-note-title note)))))))


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
  "Render the sidebar with NOTE as context in FRAME.
Does nothing when FRAME has no live sidebar window.  Mounting calls
`switch-to-buffer', so rendering without a side window would take over
whatever window is selected; bailing out keeps the sidebar from
clobbering an unrelated buffer."
  (let* ((vulpea-ui--rendering t)  ; Prevent re-entry
         (frame (or frame (selected-frame)))
         (sidebar-win (vulpea-ui--get-sidebar-window frame)))
    (when (window-live-p sidebar-win)
      (let* ((buf-name (vulpea-ui--sidebar-buffer-name frame))
             (buf (get-buffer-create buf-name))
             (original-window (selected-window))
             (existing-instance (gethash frame vulpea-ui--sidebar-instances)))
        ;; Select sidebar window before mount (vui-mount calls switch-to-buffer)
        (select-window sidebar-win t)
        (with-current-buffer buf
          (if (and existing-instance
                   (vui-instance-buffer existing-instance)
                   (buffer-live-p (vui-instance-buffer existing-instance)))
              ;; Reuse existing instance - update props, preserve memos
              (vui-update-props existing-instance (list :note note))
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
          (select-window original-window t))))))


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
         (buf (vulpea-ui--get-sidebar-buffer frame)))
    (vulpea-ui--hide-sidebar-window frame)
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
  "Force refresh the sidebar, invalidating all caches."
  (interactive)
  (let* ((frame (selected-frame))
         (main-win (vulpea-ui--get-main-window frame))
         (main-buf (when main-win (window-buffer main-win)))
         (note (vulpea-ui--get-note-from-buffer main-buf))
         (instance (gethash frame vulpea-ui--sidebar-instances)))
    (when (and note instance
               (vui-instance-buffer instance)
               (buffer-live-p (vui-instance-buffer instance)))
      (with-current-buffer (vui-instance-buffer instance)
        (setq vulpea-ui--current-note note)
        (cl-incf vulpea-ui--refresh-generation))
      (vui-update instance (list :note note)))))


;;; Schema health widget

(defcustom vulpea-ui-schema-health-ok-glyph "✓"
  "Glyph shown when the current note conforms to its schema(s).
A short string; the default is portable across fonts and terminals.
Set it to a nerd-font or all-the-icons glyph if you prefer."
  :type 'string
  :group 'vulpea-ui)

(defcustom vulpea-ui-schema-health-issue-glyph "✗"
  "Glyph shown in the summary line when the note violates its schema(s)."
  :type 'string
  :group 'vulpea-ui)

(defcustom vulpea-ui-schema-health-bullet "●"
  "Bullet shown before each individual schema violation."
  :type 'string
  :group 'vulpea-ui)

(defface vulpea-ui-schema-health-ok-face
  '((t :inherit success))
  "Face for the schema widget's healthy status line."
  :group 'vulpea-ui)

(defface vulpea-ui-schema-health-error-face
  '((t :inherit error))
  "Face for structural schema violations (missing, wrong type, bad ref)."
  :group 'vulpea-ui)

(defface vulpea-ui-schema-health-warning-face
  '((t :inherit warning))
  "Face for value schema violations (disallowed, failed check, bad target)."
  :group 'vulpea-ui)

(defface vulpea-ui-schema-health-field-face
  '((t :inherit bold))
  "Face for the field name of a schema violation."
  :group 'vulpea-ui)

(defface vulpea-ui-schema-health-message-face
  '((t :inherit shadow))
  "Face for the reason text of a schema violation."
  :group 'vulpea-ui)

(defface vulpea-ui-schema-health-action-face
  '((t :inherit link :underline nil))
  "Face for the quick-fix action buttons of schema violations."
  :group 'vulpea-ui)

(defface vulpea-ui-schema-health-note-face
  '((t :inherit bold))
  "Face for a note's title in the schema widget's per-file breakdown."
  :group 'vulpea-ui)

(declare-function vulpea-db-query-by-file-path "vulpea-db-query"
                  (file-path &optional level))

(defun vulpea-ui--schema-note-health (note)
  "Return schema health for a single NOTE, or nil when no schema is applicable.
The result is a plist with :note, :schemas (the applicable schema names)
and :violations (a list of `vulpea-violation' across them)."
  (when note
    (when-let* ((schemas (vulpea-schema-applicable note)))
      (list :note note
            :schemas schemas
            :violations (vulpea-schema-note-violations note)))))

(defun vulpea-ui--schema-file-notes (path)
  "Return every indexed note in the file at PATH, or nil.
Includes the file-level note and all heading-level notes, so the schema
widget can report on the whole file rather than only the note at point."
  (when (and path (fboundp 'vulpea-db-query-by-file-path))
    (vulpea-db-query-by-file-path path)))

(defun vulpea-ui--schema-file-health (notes)
  "Return a schema-health breakdown for NOTES (all notes in one file).
Keeps only the notes a schema applies to, ordered by buffer position so
the file-level note comes first and headings follow in document order.
The result is a plist:
  :entries       per-note health plists (see `vulpea-ui--schema-note-health')
  :violated      the entries that carry violations, in the same order
  :healthy-count number of schema-matching notes with no violations
  :issue-count   total number of violations across every entry"
  (let* ((entries (delq nil (mapcar #'vulpea-ui--schema-note-health notes)))
         (entries (sort entries
                        (lambda (a b)
                          (< (or (vulpea-note-pos (plist-get a :note)) 0)
                             (or (vulpea-note-pos (plist-get b :note)) 0)))))
         (violated (seq-filter (lambda (e) (plist-get e :violations)) entries))
         (issue-count (apply #'+ (mapcar (lambda (e)
                                           (length (plist-get e :violations)))
                                         entries))))
    (list :entries entries
          :violated violated
          :healthy-count (- (length entries) (length violated))
          :issue-count issue-count)))

(defun vulpea-ui--schema-violation-severity (type)
  "Return `error' or `warning' for a violation of TYPE.
A missing field, a wrong type or a broken reference is structural and
returns `error'; a value problem returns `warning'."
  (if (memq type '(missing-required wrong-type invalid-reference))
      'error
    'warning))

(defun vulpea-ui--schema-type-noun (type)
  "Return a human phrase naming the expected value TYPE."
  (pcase type
    ('number "a number")
    ('symbol "a symbol")
    ('note "a note")
    ('link "a link")
    (_ "a string")))

(defun vulpea-ui--schema-violation-reason (violation note)
  "Return a terse, field-free reason for VIOLATION on NOTE.
Resolves the violated field's spec to phrase the reason precisely - the
allowed values for a disallowed value, the expected type for a wrong
type - otherwise falls back to the violation's own message."
  (let* ((schema (ignore-errors
                   (vulpea-schema-get (vulpea-violation-schema violation))))
         (field (and schema
                     (cl-find (vulpea-violation-field violation)
                              (vulpea-schema-fields schema)
                              :key (lambda (f) (plist-get f :key))
                              :test #'equal))))
    (pcase (vulpea-violation-type violation)
      ('missing-required "required")
      ('wrong-type (format "expected %s"
                           (vulpea-ui--schema-type-noun (plist-get field :type))))
      ('invalid-reference "missing note")
      ('invalid-target "wrong target tags")
      ('disallowed-value
       (let ((allowed (let ((one-of (plist-get field :one-of)))
                        (if (functionp one-of) (funcall one-of note) one-of))))
         (if allowed
             (format "not one of %s"
                     (mapconcat (lambda (x) (format "%s" x)) allowed "/"))
           (format "invalid value %s" (vulpea-violation-value violation)))))
      (_ (or (vulpea-violation-message violation) "invalid")))))

(defun vulpea-ui--schema-note-end (note)
  "Return the end of NOTE's own section in the current buffer.
This bounds where NOTE's metadata and fields can live: the file's zeroth
section for a file-level note (up to its first heading), or a heading's
body up to its first child heading otherwise.  It deliberately stops
before any child heading - meta there belongs to the child note, not to
NOTE - so a missing-field target never leaks into a sub-heading.  Assumes
the current buffer is NOTE's file."
  (save-excursion
    (goto-char (vulpea-note-pos note))
    (if (> (vulpea-note-level note) 0)
        ;; A heading's own section runs to its first child heading, or to
        ;; the end of its subtree when it has none.  Every heading inside
        ;; the subtree is necessarily a child (deeper), so the first one
        ;; found after the heading line marks the section end.
        (let ((subtree-end (save-excursion (org-end-of-subtree t t) (point))))
          (forward-line 1)
          (if (re-search-forward "^\\*+ " subtree-end t)
              (line-beginning-position)
            subtree-end))
      (if (re-search-forward "^\\*+ " nil t)
          (line-beginning-position)
        (point-max)))))

(defun vulpea-ui--schema-meta-position (note)
  "Return where NOTE's metadata lives, or where it would be inserted.
With existing metadata, the first metadata line; otherwise the point
after NOTE's heading, property drawer and keywords, before its body.
Returns NOTE's own position when its file is not visited."
  (let* ((path (vulpea-note-path note))
         (buf (and path (find-buffer-visiting path))))
    (if (not buf)
        (vulpea-note-pos note)
      (with-current-buffer buf
        (save-excursion
          (goto-char (vulpea-note-pos note))
          (let ((end (vulpea-ui--schema-note-end note)))
            (if (re-search-forward "^[ \t]*-[ \t]+[^\n]+?[ \t]+::" end t)
                (line-beginning-position)
              (goto-char (vulpea-note-pos note))
              (when (> (vulpea-note-level note) 0)
                (forward-line 1))
              (when (looking-at-p "^[ \t]*:PROPERTIES:")
                (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
                  (forward-line 1)))
              (while (and (< (point) end) (looking-at-p "^[ \t]*#\\+"))
                (forward-line 1))
              (point))))))))

(defun vulpea-ui--schema-violation-position (violation note)
  "Return a position in NOTE's file to jump to for VIOLATION.
A value violation goes to the offending field's line - the line carrying
the violated value when VIOLATION names one, so distinct occurrences of a
multi-value field resolve to distinct lines instead of all landing on the
first.  A missing field (which has no line yet) goes to NOTE's metadata
block, or to where metadata would be inserted."
  (let* ((path (vulpea-note-path note))
         (buf (and path (find-buffer-visiting path)))
         (field (vulpea-violation-field violation))
         (value (vulpea-violation-value violation)))
    (or
     (when (and buf field
                (not (eq (vulpea-violation-type violation) 'missing-required)))
       (with-current-buffer buf
         (save-excursion
           (let* ((start (vulpea-note-pos note))
                  (end (vulpea-ui--schema-note-end note))
                  (field-re (format "^[ \t]*-[ \t]+%s[ \t]+::"
                                    (regexp-quote field))))
             (or
              ;; Prefer the line carrying the offending value.
              (when value
                (goto-char start)
                (and (re-search-forward
                      (format "%s[ \t]*%s[ \t]*$" field-re
                              (regexp-quote (format "%s" value)))
                      end t)
                     (line-beginning-position)))
              ;; Fall back to the first occurrence of the field.
              (progn
                (goto-char start)
                (when (re-search-forward field-re end t)
                  (line-beginning-position))))))))
     (vulpea-ui--schema-meta-position note))))

(declare-function vulpea-schema-fix-violation "vulpea" (violation &optional bound))

(defun vulpea-ui--schema-fix-violation-action (note violation)
  "Fix VIOLATION on NOTE, persist the change, and refresh the sidebar.
Prompt for a corrected value, write it to NOTE's file (scoped to NOTE's
heading when it is one), re-index, and re-render.  In the per-file
breakdown, anchor point on NOTE's title row before refreshing, so when
the fixed violation clears vui's cursor restoration lands on that note
\(when it keeps other issues) or the nearest surviving row (when its
title vanishes with its last issue).  In single-note mode there is no
title row to anchor; vui's own tree recovery keeps point on a surviving
row there.  Do nothing when the prompt is skipped or NOTE's buffer is not
visiting a file."
  (when-let* ((path (vulpea-note-path note))
              (buf (find-buffer-visiting path)))
    (when (with-current-buffer buf
            (vulpea-schema-fix-violation
             violation
             (when (> (vulpea-note-level note) 0)
               (vulpea-note-pos note))))
      (with-current-buffer buf (save-buffer))
      (vulpea-db-update-file path)
      (when (fboundp 'vui-goto-key)
        (vui-goto-key (list 'schema-note (vulpea-note-id note))))
      (vulpea-ui-sidebar-refresh))))

(defun vulpea-ui--render-schema-violation (violation note index)
  "Render one row for VIOLATION on NOTE at per-note INDEX.
The row is a severity bullet, an optional quick-fix button, the field
name as a button that jumps to the offending field, and a terse reason.
NOTE's id, the field, the type and INDEX key the buttons so point keeps
its identity across a re-render even when several notes - or several
same-field violations on one note - are listed together."
  (let* ((face (if (eq (vulpea-ui--schema-violation-severity
                        (vulpea-violation-type violation))
                       'error)
                   'vulpea-ui-schema-health-error-face
                 'vulpea-ui-schema-health-warning-face))
         (note-id (vulpea-note-id note))
         (field (vulpea-violation-field violation))
         (type (vulpea-violation-type violation)))
    (apply
     #'vui-hstack
     (delq
      nil
      (list
       (vui-text vulpea-ui-schema-health-bullet :face face)
       (when (fboundp 'vulpea-schema-fix-violation)
         (vui-button "fix"
           :face 'vulpea-ui-schema-health-action-face
           :key (list 'fix note-id field type index)
           :on-click (lambda ()
                       (vulpea-ui--schema-fix-violation-action note violation))
           :help-echo "Prompt for a value and fix this violation"))
       (vui-button (or field "")
         :face 'vulpea-ui-schema-health-field-face
         :no-decoration t
         :key (list 'field note-id field type index)
         :help-echo nil
         :on-click (lambda ()
                     (vulpea-ui--jump-to-position
                      note
                      (vulpea-ui--schema-violation-position violation note))))
       (vui-text (vulpea-ui--schema-violation-reason violation note)
         :face 'vulpea-ui-schema-health-message-face))))))

(defun vulpea-ui--render-schema-note-entry (entry show-title)
  "Render one violated ENTRY (a per-note health plist).
With SHOW-TITLE, prepend the note's title as a jump button, so the
per-file breakdown names which note each block belongs to; without it,
render just the schema line and violation rows (the single-note look).
The title button is keyed on the note so point can be anchored to it."
  (let* ((note (plist-get entry :note))
         (schemas (plist-get entry :schemas))
         (violations (plist-get entry :violations))
         (names (mapconcat #'symbol-name schemas ", "))
         (summary (vui-text
                   (format "%s %s · %d issue%s"
                           vulpea-ui-schema-health-issue-glyph names
                           (length violations)
                           (if (= (length violations) 1) "" "s"))
                   :face 'vulpea-ui-schema-health-error-face))
         (rows (seq-map-indexed
                (lambda (v i) (vulpea-ui--render-schema-violation v note i))
                violations))
         (title (when show-title
                  (vui-button (or (vulpea-note-title note) "(untitled)")
                    :face 'vulpea-ui-schema-health-note-face
                    :no-decoration t
                    :key (list 'schema-note (vulpea-note-id note))
                    :help-echo "Jump to this note"
                    :on-click (lambda () (vulpea-ui-visit-note note))))))
    (apply #'vui-vstack (delq nil (append (list title summary) rows)))))

(vui-defcomponent vulpea-ui-widget-schema-health ()
  "Widget reporting schema health for every note in the current file.
Covers the file-level note and each heading-level note a schema applies
to.  With a single such note it reads as one status line plus its
violations; with several it breaks the file down per note - the
file-level note first, then headings in document order - each under its
own title, so a violation on a heading is never hidden behind the
file-level note.  A trailing count summarises any notes that are clean.
Renders nothing when the file carries no schema-bearing note."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let* ((path (vulpea-note-path note))
             (note-buf (when path (find-buffer-visiting path)))
             (tick (when note-buf (buffer-modified-tick note-buf)))
             (health (vui-use-memo (note tick vulpea-ui--refresh-generation)
                       (vulpea-ui--schema-file-health
                        (vulpea-ui--schema-file-notes path))))
             (entries (plist-get health :entries)))
        (when entries
          (let* ((violated (plist-get health :violated))
                 (issue-count (plist-get health :issue-count))
                 (healthy-count (plist-get health :healthy-count))
                 (multi (> (length entries) 1)))
            (vui-component 'vulpea-ui-widget
              :title "Schema"
              :count (when (> issue-count 0) issue-count)
              :children
              (lambda ()
                (cond
                 ;; Nothing wrong anywhere in the file.
                 ((null violated)
                  (vui-text
                   (if multi
                       (format "%s %d notes · healthy"
                               vulpea-ui-schema-health-ok-glyph (length entries))
                     (format "%s %s · healthy"
                             vulpea-ui-schema-health-ok-glyph
                             (mapconcat #'symbol-name
                                        (plist-get (car entries) :schemas) ", ")))
                   :face 'vulpea-ui-schema-health-ok-face))
                 ;; Single schema-bearing note: the classic per-note view.
                 ((not multi)
                  (vulpea-ui--render-schema-note-entry (car violated) nil))
                 ;; Several notes in the file: break it down per note.
                 (t
                  (apply
                   #'vui-vstack
                   :spacing 1
                   (vui-text
                    (format "%s %d note%s · %d issue%s"
                            vulpea-ui-schema-health-issue-glyph
                            (length violated) (if (= (length violated) 1) "" "s")
                            issue-count (if (= issue-count 1) "" "s"))
                    :face 'vulpea-ui-schema-health-error-face)
                   (append
                    (seq-map (lambda (e)
                               (vulpea-ui--render-schema-note-entry e t))
                             violated)
                    (when (> healthy-count 0)
                      (list (vui-muted
                             (format "%s %d note%s healthy"
                                     vulpea-ui-schema-health-ok-glyph
                                     healthy-count
                                     (if (= healthy-count 1) "" "s")))))))))))))))))


;;; Built-in widget registration

(vulpea-ui-register-widget 'stats
                           :component 'vulpea-ui-widget-stats
                           :order 100)

(vulpea-ui-register-widget 'schema-health
                           :component 'vulpea-ui-widget-schema-health
                           ;; Feature-detect only; the widget itself hides
                           ;; (renders nothing) when the file has no
                           ;; schema-bearing note, and it looks at every note
                           ;; in the file, not just the one anchoring the
                           ;; sidebar - so an applicable-schema check on that
                           ;; one note would wrongly hide it for heading-only
                           ;; schema files.
                           :predicate (lambda (_note)
                                        (fboundp 'vulpea-schema-note-violations))
                           :order 150)

(vulpea-ui-register-widget 'outline
                           :component 'vulpea-ui-widget-outline
                           :order 200)

(vulpea-ui-register-widget 'backlinks
                           :component 'vulpea-ui-widget-backlinks
                           :order 300)

(vulpea-ui-register-widget 'unlinked-mentions
                           :component 'vulpea-ui-widget-unlinked-mentions
                           :order 350)

(vulpea-ui-register-widget 'links
                           :component 'vulpea-ui-widget-links
                           :order 400)

(vulpea-ui-register-widget 'outgoing-mentions
                           :component 'vulpea-ui-widget-outgoing-mentions
                           :order 450)

;;; Schema dashboard

(declare-function vulpea-schema-collection-health "vulpea-schema" (&optional notes))

(defface vulpea-ui-schema-dashboard-schema-face
  '((t :inherit bold))
  "Face for a schema name in the schema dashboard."
  :group 'vulpea-ui)

(defun vulpea-ui-schema-dashboard--rank (health)
  "Return a sort rank for HEALTH: 0 needs attention, 1 covered, 2 unused."
  (cond ((> (vulpea-schema-health-invalid health) 0) 0)
        ((> (vulpea-schema-health-covered health) 0) 1)
        (t 2)))

(defun vulpea-ui-schema-dashboard--sort (healths)
  "Order HEALTHS for display: needs-attention first, unused last, then by name."
  (sort (copy-sequence healths)
        (lambda (a b)
          (let ((ra (vulpea-ui-schema-dashboard--rank a))
                (rb (vulpea-ui-schema-dashboard--rank b)))
            (if (= ra rb)
                (string< (symbol-name (vulpea-schema-health-schema a))
                         (symbol-name (vulpea-schema-health-schema b)))
              (< ra rb))))))

(defun vulpea-ui-schema-dashboard--status-text (health)
  "Return the status string shown to the right of HEALTH's schema name."
  (let ((covered (vulpea-schema-health-covered health))
        (invalid (vulpea-schema-health-invalid health)))
    (cond
     ((= covered 0) "unused")
     ((= invalid 0)
      (format "%d %s · all valid" covered (if (= covered 1) "note" "notes")))
     (t (format "%d %s · %d invalid"
                covered (if (= covered 1) "note" "notes") invalid)))))

(defun vulpea-ui-schema-dashboard--status-face (health)
  "Return the face for HEALTH's status string."
  (let ((covered (vulpea-schema-health-covered health))
        (invalid (vulpea-schema-health-invalid health)))
    (cond
     ((= covered 0) 'shadow)
     ((= invalid 0) 'vulpea-ui-schema-health-ok-face)
     (t 'vulpea-ui-schema-health-error-face))))

(defun vulpea-ui-schema-dashboard--includes-text (health)
  "Return HEALTH's include-relationship line, or nil when it has none."
  (let ((inc (vulpea-schema-health-includes health))
        (by (vulpea-schema-health-included-by health)))
    (cond
     ((and inc by)
      (format "includes %s · included by %s"
              (mapconcat #'symbol-name inc ", ")
              (mapconcat #'symbol-name by ", ")))
     (inc (format "includes %s" (mapconcat #'symbol-name inc ", ")))
     (by (format "included by %s" (mapconcat #'symbol-name by ", ")))
     (t nil))))

(defun vulpea-ui-schema-dashboard--summary-text (healths)
  "Return the collection summary line for HEALTHS."
  (let ((n (length healths))
        (flagged (cl-count-if (lambda (h) (> (vulpea-schema-health-invalid h) 0))
                              healths)))
    (format "%d %s · %s"
            n (if (= n 1) "schema" "schemas")
            (if (= flagged 0) "all healthy"
              (format "%d with issues" flagged)))))

(defun vulpea-ui-schema-dashboard--width ()
  "Return the dashboard window's body width for right-aligned headers.
Fall back to `fill-column' when the dashboard is not shown in a window."
  (let ((win (get-buffer-window (current-buffer) t)))
    (if win (window-body-width win) fill-column)))

(defconst vulpea-ui-schema-dashboard-buffer-name "*vulpea schema*"
  "Name of the schema dashboard buffer.")

(defun vulpea-ui-schema-dashboard--visit-field (note violation)
  "Show NOTE and move point to VIOLATION's field, returning its buffer."
  (when-let* ((path (vulpea-note-path note))
              (buf (find-file-noselect path)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (goto-char (vulpea-ui--schema-violation-position violation note)))
    buf))

(defun vulpea-ui-schema-dashboard--fix-violation (note violation)
  "Show NOTE at VIOLATION's field, fix it, then refresh the dashboard.
Pop the note for context only while the prompt is open, then restore the
previous window layout so you land back on the dashboard.  Before the
refresh, put point on NOTE's own row so vui has a widget to recover from:
clicking the fix button with the mouse does not move point, so it may sit
on a non-widget line (the summary at the top), and vui would faithfully
keep it there instead of following the note.  Anchored on the note, vui's
cursor restoration lands on the nearest surviving row once the fix clears
the note away, the next note or the previous one when this was the
section's last.  Do nothing when the prompt is skipped."
  (let ((windows (current-window-configuration)))
    (when-let* ((buf (vulpea-ui-schema-dashboard--visit-field note violation)))
      (let ((fixed (with-current-buffer buf
                     (vulpea-schema-fix-violation
                      violation
                      (when (> (vulpea-note-level note) 0)
                        (vulpea-note-pos note))))))
        (when fixed
          (with-current-buffer buf (save-buffer))
          (vulpea-db-update-file (vulpea-note-path note)))
        (set-window-configuration windows)
        (when fixed
          (when-let* ((dashboard (get-buffer vulpea-ui-schema-dashboard-buffer-name)))
            (with-current-buffer dashboard
              (vui-goto-key (list (vulpea-violation-schema violation)
                                  (vulpea-note-id note)))))
          (vulpea-ui-schema-dashboard-refresh))))))

(defun vulpea-ui-schema-dashboard--render-violation (schema note violation index)
  "Render one VIOLATION row for NOTE under SCHEMA: bullet, fix, field, reason.
INDEX is the violation's position among NOTE's violations.  SCHEMA, the
violation's field and type, and INDEX go into the fix and field buttons'
keys, so each row keeps a stable cursor identity that is unique across
the whole buffer: SCHEMA separates a note invalid under two schemas, and
INDEX separates two violations that share a field and type (a multi-value
field can fail its `:one-of' more than once).  See `vui--widget-identity'."
  (let* ((face (if (eq (vulpea-ui--schema-violation-severity
                        (vulpea-violation-type violation))
                       'error)
                   'vulpea-ui-schema-health-error-face
                 'vulpea-ui-schema-health-warning-face))
         (note-id (vulpea-note-id note))
         (field (vulpea-violation-field violation))
         (type (vulpea-violation-type violation)))
    (apply
     #'vui-hstack
     (delq
      nil
      (list
       (vui-text vulpea-ui-schema-health-bullet :face face)
       (when (fboundp 'vulpea-schema-fix-violation)
         (vui-button "fix"
           :face 'vulpea-ui-schema-health-action-face
           :key (list 'fix schema note-id field type index)
           :on-click (lambda ()
                       (vulpea-ui-schema-dashboard--fix-violation note violation))
           :help-echo "Show the note and fix this violation"))
       (vui-button (or field "")
         :face 'vulpea-ui-schema-health-field-face
         :no-decoration t
         :key (list 'field schema note-id field type index)
         :help-echo nil
         :on-click (lambda ()
                     (vulpea-ui-schema-dashboard--visit-field note violation)))
       (vui-text (vulpea-ui--schema-violation-reason violation note)
         :face 'vulpea-ui-schema-health-message-face))))))

(vui-defcomponent vulpea-ui-schema-dashboard-note (entry schema)
  "One invalid note in the dashboard, collapsible to its violations.
ENTRY is a (note . violations) pair and SCHEMA is the owning schema,
threaded in so this note's toggle and its violation buttons stay
globally unique when the same note is invalid under more than one
schema.  Built on `vui-collapsible', which owns the collapsed state and
gives the toggle a stable cursor identity; the note starts collapsed and
its count keeps right-aligning to the window edge via `:header-width'."
  :render
  (let* ((note (car entry))
         (violations (cdr entry))
         (count (length violations)))
    (vui-collapsible
     :title (or (vulpea-note-title note) "(untitled)")
     :header-right (vui-text
                    (format "%d %s" count (if (= count 1) "issue" "issues"))
                    :face 'vulpea-ui-schema-health-message-face)
     :header-width #'vulpea-ui-schema-dashboard--width
     :key (list schema (vulpea-note-id note))
     (seq-map-indexed
      (lambda (v i)
        (vulpea-ui-schema-dashboard--render-violation schema note v i))
      violations))))

(vui-defcomponent vulpea-ui-schema-dashboard-section (entry)
  "One schema's section in the dashboard.
ENTRY is a `vulpea-schema-health'.  Built on `vui-collapsible': the
header toggles the schema's invalid notes and a schema with violations
starts expanded.  The toggle carries the schema symbol as its key, so
its cursor identity is stable across the ▶/▼ flip and unique in the
buffer; the status keeps right-aligning to the window edge via
`:header-width'."
  :render
  (let* ((schema (vulpea-schema-health-schema entry))
         (invalid (vulpea-schema-health-invalid entry))
         (includes (vulpea-ui-schema-dashboard--includes-text entry)))
    (vui-collapsible
     :title (symbol-name schema)
     :title-face 'vulpea-ui-schema-dashboard-schema-face
     :header-right (vui-text (vulpea-ui-schema-dashboard--status-text entry)
                             :face (vulpea-ui-schema-dashboard--status-face entry))
     :header-width #'vulpea-ui-schema-dashboard--width
     :indent 4
     :initially-expanded (> invalid 0)
     :key schema
     (when includes
       (vui-text includes :face 'vulpea-ui-schema-health-message-face))
     (seq-map (lambda (h)
                (vui-component 'vulpea-ui-schema-dashboard-note
                               :entry h
                               :schema schema
                               :key (list schema (vulpea-note-id (car h)))))
              (vulpea-schema-health-invalid-notes entry)))))

(vui-defcomponent vulpea-ui-schema-dashboard-root (health)
  "Root of the schema dashboard.
HEALTH is a list of `vulpea-schema-health', already sorted for display."
  :render
  (vui-vstack
   :spacing 1
   (vui-vstack
    :spacing 0
    (vui-text "Schema health" :face 'vulpea-ui-widget-header-face)
    (vui-text (vulpea-ui-schema-dashboard--summary-text health)
              :face 'shadow))
   (vui-vstack
    :spacing 0
    (seq-map (lambda (h)
               (vui-component 'vulpea-ui-schema-dashboard-section
                              :entry h
                              :key (vulpea-schema-health-schema h)))
             health))))

(defvar vulpea-ui-schema-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'vulpea-ui-schema-dashboard-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `vulpea-ui-schema-dashboard-mode'.")

(define-derived-mode vulpea-ui-schema-dashboard-mode vui-mode "vulpea-schema"
  "Major mode for the vulpea schema health dashboard.
\\{vulpea-ui-schema-dashboard-mode-map}"
  :group 'vulpea-ui
  (setq-local truncate-lines t))

(defvar-local vulpea-ui-schema-dashboard--instance nil
  "The vui instance mounted in the schema dashboard buffer.")

(defun vulpea-ui-schema-dashboard--render ()
  "Compute schema health and (re-)render the dashboard buffer.
Always targets `vulpea-ui-schema-dashboard-buffer-name', so it is safe to
call from any buffer - in particular from a fix action while the note's
own buffer is current, where rendering against the current buffer would
mount a second dashboard into it."
  (when-let* ((buf (get-buffer vulpea-ui-schema-dashboard-buffer-name)))
    (with-current-buffer buf
      (let ((health (vulpea-ui-schema-dashboard--sort
                     (vulpea-schema-collection-health))))
        (if (and vulpea-ui-schema-dashboard--instance
                 (vui-instance-buffer vulpea-ui-schema-dashboard--instance)
                 (buffer-live-p (vui-instance-buffer
                                 vulpea-ui-schema-dashboard--instance)))
            (vui-update vulpea-ui-schema-dashboard--instance (list :health health))
          (setq vulpea-ui-schema-dashboard--instance
                (vui-mount (vui-component 'vulpea-ui-schema-dashboard-root
                                          :health health)
                           vulpea-ui-schema-dashboard-buffer-name))
          ;; right-aligned counts depend on the window width, reflow on resize
          (when (fboundp 'vui-rerender-on-resize)
            (vui-rerender-on-resize)))))))

(defun vulpea-ui-schema-dashboard-refresh ()
  "Recompute schema health and re-render the dashboard."
  (interactive)
  (vulpea-ui-schema-dashboard--render))

;;;###autoload
(defun vulpea-ui-schema-dashboard ()
  "Open the vulpea schema health dashboard.
List every registered schema with how many notes it covers and how many
are invalid; expand a schema to see its invalid notes and jump to them.
Press \\<vulpea-ui-schema-dashboard-mode-map>\\[vulpea-ui-schema-dashboard-refresh] to refresh."
  (interactive)
  (unless (fboundp 'vulpea-schema-collection-health)
    (user-error "This vulpea has no schema engine (need a newer vulpea)"))
  (let ((buf (get-buffer-create vulpea-ui-schema-dashboard-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'vulpea-ui-schema-dashboard-mode)
        (vulpea-ui-schema-dashboard-mode)))
    (switch-to-buffer buf)
    (vulpea-ui-schema-dashboard--render)))

;;; Collection view

(defcustom vulpea-ui-collection-views nil
  "Alist of saved collection views.
Each element is (NAME . SPEC) where NAME is a string and SPEC is a
plist with the keys:

  :filter   - filter plist, see `vulpea-ui-collection--note-matches-p'
  :columns  - list of column descriptors, see
              `vulpea-ui-collection--normalize-column'
  :sort     - initial `tabulated-list-sort-key', e.g. (\"Title\" . nil)"
  :type '(alist :key-type string :value-type plist)
  :group 'vulpea-ui)

(defcustom vulpea-ui-collection-default-columns '(title tags modified)
  "Columns used by collection views that do not specify their own."
  :type '(repeat sexp)
  :group 'vulpea-ui)

(defface vulpea-ui-collection-tags-face
  '((t :inherit shadow))
  "Face for the tags column in collection views."
  :group 'vulpea-ui)

(defface vulpea-ui-collection-date-face
  '((t :inherit shadow))
  "Face for date columns in collection views."
  :group 'vulpea-ui)

(defface vulpea-ui-collection-context-face
  '((t :inherit shadow))
  "Face for the context, file and aliases columns in collection views."
  :group 'vulpea-ui)

(defface vulpea-ui-collection-todo-face
  '((t :inherit org-todo))
  "Face for the todo and priority columns in collection views."
  :group 'vulpea-ui)

(defface vulpea-ui-collection-marked-face
  '((t :inherit warning))
  "Face layered over every cell of a marked row in collection views."
  :group 'vulpea-ui)

(defvar-local vulpea-ui-collection--view nil
  "The view spec rendered in this collection buffer.")

(defvar-local vulpea-ui-collection--marked nil
  "Hash table of marked note ids in this collection buffer.
Marks are keyed by note id, so they survive sorting and re-querying.")

(defvar-local vulpea-ui-collection--note-table nil
  "Hash table mapping note id to `vulpea-note' for the current entries.")

(defvar-local vulpea-ui-collection--ctx nil
  "Per-refresh context data (e.g. backlink counts) for cell rendering.")

;;;; Filtering and querying

(defun vulpea-ui-collection--note-matches-p (note filter)
  "Return non-nil when NOTE satisfies FILTER.
FILTER is a plist; every present condition must hold:

  :tags-all   - list of tags NOTE must all have
  :tags-any   - list of tags of which NOTE must have at least one
  :tags-none  - list of tags NOTE must not have
  :level      - heading level NOTE must have (0 for file-level)
  :directory  - absolute directory prefix, or a relative directory
                that must appear as a component of the note's path
  :title      - regexp the note title must match
  :meta       - alist of (KEY . VALUE) conditions; VALUE t means the
                key must merely be present
  :predicate  - function called with NOTE, must return non-nil"
  (let ((tags (vulpea-note-tags note))
        (path (vulpea-note-path note)))
    (and (let ((all (plist-get filter :tags-all)))
           (seq-every-p (lambda (tag) (member tag tags)) all))
         (let ((any (plist-get filter :tags-any)))
           (or (null any)
               (seq-some (lambda (tag) (member tag tags)) any)))
         (let ((none (plist-get filter :tags-none)))
           (not (seq-some (lambda (tag) (member tag tags)) none)))
         (let ((level (plist-get filter :level)))
           (or (null level) (= level (vulpea-note-level note))))
         (let ((dir (plist-get filter :directory)))
           (or (null dir)
               (if (file-name-absolute-p dir)
                   (string-prefix-p (file-name-as-directory
                                     (expand-file-name dir))
                                    path)
                 (string-match-p
                  (concat "/" (regexp-quote (directory-file-name dir)) "/")
                  path))))
         (let ((re (plist-get filter :title)))
           (or (null re) (string-match-p re (vulpea-note-title note))))
         (seq-every-p
          (lambda (condition)
            (let ((values (cdr (assoc (car condition)
                                      (vulpea-note-meta note)))))
              (if (eq (cdr condition) t)
                  values
                (member (cdr condition) values))))
          (plist-get filter :meta))
         (let ((pred (plist-get filter :predicate)))
           (or (null pred) (funcall pred note))))))

(defun vulpea-ui-collection--filter-description (filter)
  "Return a short human-readable summary of FILTER.
Empty string when FILTER has no conditions."
  (let (parts)
    (dolist (tag (plist-get filter :tags-all))
      (push (concat "#" tag) parts))
    (dolist (tag (plist-get filter :tags-any))
      (push (concat "#" tag "?") parts))
    (dolist (tag (plist-get filter :tags-none))
      (push (concat "-#" tag) parts))
    (when-let* ((level (plist-get filter :level)))
      (push (format "level:%d" level) parts))
    (when-let* ((dir (plist-get filter :directory)))
      (push (concat "dir:" dir) parts))
    (when-let* ((re (plist-get filter :title)))
      (push (concat "title:" re) parts))
    (dolist (condition (plist-get filter :meta))
      (push (if (eq (cdr condition) t)
                (format "%s:*" (car condition))
              (format "%s:%s" (car condition) (cdr condition)))
            parts))
    (when (plist-get filter :predicate)
      (push "predicate" parts))
    (string-join (nreverse parts) " ")))

(defun vulpea-ui-collection--query (filter)
  "Return all notes matching FILTER.
The cheapest applicable database entry point narrows the candidate
set, then `vulpea-ui-collection--note-matches-p' applies the full
FILTER in memory."
  (let ((notes (cond
                ((plist-get filter :tags-all)
                 (vulpea-db-query-by-tags-every
                  (plist-get filter :tags-all)))
                ((plist-get filter :tags-any)
                 (vulpea-db-query-by-tags-some
                  (plist-get filter :tags-any)))
                ((and (plist-get filter :directory)
                      (fboundp 'vulpea-db-query-by-directory))
                 (vulpea-db-query-by-directory
                  (plist-get filter :directory)))
                (t (vulpea-db-query)))))
    (seq-filter (lambda (note)
                  (vulpea-ui-collection--note-matches-p note filter))
                notes)))

;;;; Columns

(defconst vulpea-ui-collection--columns
  '((title "Title" 48)
    (context "Context" 36)
    (tags "Tags" 24)
    (meta nil 14)
    (todo "Todo" 8)
    (priority "Pri" 4)
    (scheduled "Scheduled" 10)
    (deadline "Deadline" 10)
    (created "Created" 10)
    (modified "Modified" 10)
    (links "Links" 6)
    (backlinks "Backlinks" 9)
    (aliases "Aliases" 20)
    (file "File" 20))
  "Column defaults: (ID NAME WIDTH) per column type.
A nil NAME means the column is named after its key (meta columns).")

(defun vulpea-ui-collection--normalize-column (col)
  "Normalize column descriptor COL into a plist.
COL is a symbol from `vulpea-ui-collection--columns', or a list
\(SYMBOL [KEY] [KEYWORD VALUE]...) where KEY names the meta field for
meta columns and the keywords :name and :width override the defaults.
The result is a plist with :id, :key, :name and :width."
  (let* ((col (if (listp col) col (list col)))
         (id (car col))
         (key (when (and (cdr col) (not (keywordp (cadr col))))
                (cadr col)))
         (overrides (if key (cddr col) (cdr col)))
         (defaults (alist-get id vulpea-ui-collection--columns))
         (name (or (plist-get overrides :name)
                   (car defaults)
                   key
                   (capitalize (symbol-name id))))
         (width (or (plist-get overrides :width) (cadr defaults) 12)))
    (list :id id :key key :name name :width width
          :explicit-width (and (plist-member overrides :width) t))))

(defun vulpea-ui-collection--format-time (value)
  "Format timestamp VALUE as an ISO date string.
VALUE may be nil (empty string), a string (its date part, org
timestamp brackets stripped) or a time value (formatted as
%Y-%m-%d)."
  (cond ((null value) "")
        ((stringp value)
         (let ((s (string-trim value "[<[]+" "[]>]+")))
           (substring s 0 (min 10 (length s)))))
        (t (format-time-string "%Y-%m-%d" value))))

(defconst vulpea-ui-collection--column-faces
  '((tags . vulpea-ui-collection-tags-face)
    (context . vulpea-ui-collection-context-face)
    (file . vulpea-ui-collection-context-face)
    (aliases . vulpea-ui-collection-context-face)
    (todo . vulpea-ui-collection-todo-face)
    (priority . vulpea-ui-collection-todo-face)
    (scheduled . vulpea-ui-collection-date-face)
    (deadline . vulpea-ui-collection-date-face)
    (created . vulpea-ui-collection-date-face)
    (modified . vulpea-ui-collection-date-face))
  "Face per column id for collection cells.")

(defun vulpea-ui-collection--column-value (note col &optional ctx)
  "Return the display string for NOTE in normalized column COL.
CTX is a plist with per-refresh data; :backlinks carries the hash
table of backlink counts keyed by note id.  Secondary columns carry
the faces from `vulpea-ui-collection--column-faces'."
  (let ((value (vulpea-ui-collection--column-raw-value note col ctx))
        (face (alist-get (plist-get col :id)
                         vulpea-ui-collection--column-faces)))
    (if (and face (not (string-empty-p value)))
        (propertize value 'face face)
      value)))

(defun vulpea-ui-collection--column-raw-value (note col ctx)
  "Return the unfaced display string for NOTE in normalized column COL.
CTX is the per-refresh data plist."
  (pcase (plist-get col :id)
    ('title (or (vulpea-note-title note) ""))
    ('tags (string-join (vulpea-note-tags note) " "))
    ('meta (string-join (cdr (assoc (plist-get col :key)
                                    (vulpea-note-meta note)))
                        ", "))
    ('context (if (> (vulpea-note-level note) 0)
                  (string-join
                   (delq nil (cons (vulpea-note-file-title note)
                                   (vulpea-note-outline-path note)))
                   " > ")
                ""))
    ('todo (or (vulpea-note-todo note) ""))
    ('priority (let ((priority (vulpea-note-priority note)))
                 (cond ((null priority) "")
                       ((characterp priority) (char-to-string priority))
                       (t (format "%s" priority)))))
    ('scheduled (vulpea-ui-collection--format-time
                 (vulpea-note-scheduled note)))
    ('deadline (vulpea-ui-collection--format-time
                (vulpea-note-deadline note)))
    ('aliases (string-join (vulpea-note-aliases note) ", "))
    ('created (vulpea-ui-collection--format-time
               (vulpea-note-created-at note)))
    ('modified (vulpea-ui-collection--format-time
                (vulpea-note-modified-at note)))
    ('links (number-to-string (length (vulpea-note-links note))))
    ('backlinks (if-let* ((counts (plist-get ctx :backlinks)))
                    (number-to-string
                     (gethash (vulpea-note-id note) counts 0))
                  ""))
    ('file (file-name-nondirectory (vulpea-note-path note)))
    (_ "")))

(defun vulpea-ui-collection--numeric-sorter (index)
  "Return a sort predicate comparing entry column INDEX numerically."
  (lambda (a b)
    (< (string-to-number (aref (cadr a) index))
       (string-to-number (aref (cadr b) index)))))

(defun vulpea-ui-collection--column-width (col index entries)
  "Return the width for normalized COL, the column at ENTRIES' INDEX.
Without ENTRIES, or when COL carries an explicit width, the declared
width is used as-is; otherwise the column is fitted to its longest
cell, with the declared width acting as a cap and the header always
fitting."
  (let ((width (plist-get col :width)))
    (if (or (plist-get col :explicit-width) (null entries))
        width
      (let ((longest 0))
        (dolist (entry entries)
          (setq longest (max longest
                             (string-width (aref (cadr entry) index)))))
        (min width
             (max (+ (string-width (plist-get col :name)) 2)
                  (1+ longest)))))))

(defun vulpea-ui-collection--format (columns &optional entries)
  "Build `tabulated-list-format' for COLUMNS.
A one-character mark column is prepended; count columns sort
numerically, everything else as strings.  With ENTRIES, columns
without an explicit width are fitted to their content."
  (apply #'vector
         (list "" 1 nil)
         (seq-map-indexed
          (lambda (col idx)
            (let ((col (vulpea-ui-collection--normalize-column col)))
              (list (plist-get col :name)
                    (vulpea-ui-collection--column-width col (1+ idx) entries)
                    (if (memq (plist-get col :id) '(links backlinks))
                        (vulpea-ui-collection--numeric-sorter (1+ idx))
                      t))))
          columns)))

(defun vulpea-ui-collection--entry (note cols marked ctx)
  "Build the tabulated-list entry for NOTE.
COLS are normalized columns, MARKED the hash table of marked note
ids, CTX the per-refresh data plist.  Every cell of a marked row is
layered with `vulpea-ui-collection-marked-face'."
  (let* ((id (vulpea-note-id note))
         (marked-p (gethash id marked))
         (vec (apply #'vector
                     (if marked-p "*" " ")
                     (mapcar (lambda (col)
                               (vulpea-ui-collection--column-value
                                note col ctx))
                             cols))))
    (when marked-p
      (dotimes (i (length vec))
        (let ((cell (copy-sequence (aref vec i))))
          (add-face-text-property 0 (length cell)
                                  'vulpea-ui-collection-marked-face
                                  t cell)
          (aset vec i cell))))
    (list id vec)))

(defun vulpea-ui-collection--entries (notes columns marked ctx)
  "Build tabulated-list entries for NOTES with COLUMNS.
MARKED is the hash table of marked note ids; CTX carries per-refresh
data for `vulpea-ui-collection--column-value'.  Each entry is keyed by
the note id."
  (let ((cols (mapcar #'vulpea-ui-collection--normalize-column columns)))
    (mapcar (lambda (note)
              (vulpea-ui-collection--entry note cols marked ctx))
            notes)))

;;;; Mode

(defvar vulpea-ui-collection-mark-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'vulpea-ui-collection-mark-regexp)
    (define-key map (kbd "t") #'vulpea-ui-collection-mark-by-tag)
    map)
  "Keymap for bulk marking in `vulpea-ui-collection-mode'.")

(defvar vulpea-ui-collection-column-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'vulpea-ui-collection-add-column)
    (define-key map (kbd "k") #'vulpea-ui-collection-remove-column)
    map)
  "Keymap for column management in `vulpea-ui-collection-mode'.")

(defvar vulpea-ui-collection-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'vulpea-ui-collection-filter-by-tag)
    (define-key map (kbd "T") #'vulpea-ui-collection-filter-exclude-tag)
    (define-key map (kbd "s") #'vulpea-ui-collection-filter-by-title)
    (define-key map (kbd "d") #'vulpea-ui-collection-filter-by-directory)
    (define-key map (kbd "m") #'vulpea-ui-collection-filter-by-meta)
    (define-key map (kbd "l") #'vulpea-ui-collection-filter-by-level)
    (define-key map (kbd "k") #'vulpea-ui-collection-filter-remove-condition)
    (define-key map (kbd "/") #'vulpea-ui-collection-filter-clear)
    map)
  "Keymap for filter refinement in `vulpea-ui-collection-mode'.")

(defvar vulpea-ui-collection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'vulpea-ui-collection-visit)
    (define-key map (kbd "o") #'vulpea-ui-collection-visit-other-window)
    (define-key map (kbd "C-o") #'vulpea-ui-collection-preview)
    (define-key map (kbd "m") #'vulpea-ui-collection-mark)
    (define-key map (kbd "u") #'vulpea-ui-collection-unmark)
    (define-key map (kbd "U") #'vulpea-ui-collection-unmark-all)
    (define-key map (kbd "t") #'vulpea-ui-collection-toggle-marks)
    (define-key map (kbd "+") #'vulpea-ui-collection-add-tag)
    (define-key map (kbd "-") #'vulpea-ui-collection-remove-tag)
    (define-key map (kbd "e") #'vulpea-ui-collection-set-meta)
    (define-key map (kbd "E") #'vulpea-ui-collection-remove-meta)
    (define-key map (kbd "D") #'vulpea-ui-collection-delete)
    (define-key map (kbd "x") #'vulpea-ui-collection-apply)
    (define-key map (kbd "y") #'vulpea-ui-collection-copy-links)
    (define-key map (kbd "Y") #'vulpea-ui-collection-export)
    (define-key map (kbd "w") #'vulpea-ui-collection-save-view)
    (define-key map (kbd "g") #'vulpea-ui-collection-refresh)
    (define-key map (kbd "/") vulpea-ui-collection-filter-map)
    (define-key map (kbd "c") vulpea-ui-collection-column-map)
    (define-key map (kbd "%") vulpea-ui-collection-mark-map)
    (define-key map (kbd "?") #'vulpea-ui-collection-menu)
    map)
  "Keymap for `vulpea-ui-collection-mode'.")

(define-derived-mode vulpea-ui-collection-mode tabulated-list-mode
  "vulpea-collection"
  "Major mode for vulpea collection views.
An Airtable-like table over a filtered set of notes: sortable columns,
dired-style marks and bulk actions on the selection.
\\{vulpea-ui-collection-mode-map}"
  :group 'vulpea-ui
  (setq-local vulpea-ui-collection--marked (make-hash-table :test 'equal))
  (setq-local vulpea-ui-collection--note-table
              (make-hash-table :test 'equal))
  (setq-local mode-line-process
              '((:eval (vulpea-ui-collection--mode-line-info))))
  (hl-line-mode 1)
  (setq-local revert-buffer-function
              (lambda (&rest _) (vulpea-ui-collection-refresh)))
  (add-hook 'window-buffer-change-functions
            #'vulpea-ui-collection--on-displayed nil t)
  (when (boundp 'vulpea-db-worker-done-functions)
    (add-hook 'vulpea-db-worker-done-functions
              #'vulpea-ui-collection--on-worker-done)))

(defun vulpea-ui-collection--mode-line-info ()
  "Return the mode line suffix: note count, marked count, filter."
  (let ((notes (length tabulated-list-entries))
        (marked (hash-table-count vulpea-ui-collection--marked))
        (filter (vulpea-ui-collection--filter-description
                 (plist-get vulpea-ui-collection--view :filter))))
    (concat (format ":%d" notes)
            (when (> marked 0) (format " (%d marked)" marked))
            (unless (string-empty-p filter)
              (concat " [" (truncate-string-to-width filter 40 nil nil "...")
                      "]")))))

(defun vulpea-ui-collection--context (columns)
  "Compute per-refresh context data needed by COLUMNS.
Backlink counts are fetched with a single grouped query only when a
backlinks column is present."
  (when (and (seq-some (lambda (col)
                         (eq (plist-get
                              (vulpea-ui-collection--normalize-column col)
                              :id)
                             'backlinks))
                       columns)
             (fboundp 'vulpea-db-query-backlink-counts))
    (list :backlinks (vulpea-db-query-backlink-counts "id"))))

(defun vulpea-ui-collection--view-columns ()
  "Return the column descriptors of the current view."
  (or (plist-get vulpea-ui-collection--view :columns)
      vulpea-ui-collection-default-columns))

(defvar-local vulpea-ui-collection--computed-widths nil
  "Alist of column name to last computed (auto-fitted) width.
Lets a refresh tell a hand-resized column apart from an auto-sized
one, so user adjustments survive.")

(defun vulpea-ui-collection--update-format (columns)
  "Install the fitted format for COLUMNS, keeping user width tweaks.
A column whose current width differs from the last computed one was
resized by hand (e.g. `tabulated-list-widen-current-column'); that
width is kept for as long as the column stays."
  (let* ((computed (vulpea-ui-collection--format
                    columns tabulated-list-entries))
         (fresh (mapcar (lambda (col) (cons (car col) (nth 1 col)))
                        (append computed nil))))
    (dolist (col (append computed nil))
      (when-let* ((current (seq-find (lambda (old)
                                       (equal (car old) (car col)))
                                     (append tabulated-list-format nil)))
                  (last-width (cdr (assoc (car col)
                                          vulpea-ui-collection--computed-widths))))
        (when (and (numberp (nth 1 current))
                   (/= (nth 1 current) last-width))
          (setf (nth 1 col) (nth 1 current)))))
    (setq vulpea-ui-collection--computed-widths fresh)
    (setq tabulated-list-format computed)
    (tabulated-list-init-header)))

(defun vulpea-ui-collection-refresh ()
  "Re-query the view and re-render, keeping the marked set and point."
  (interactive)
  (let* ((columns (vulpea-ui-collection--view-columns))
         (notes (vulpea-ui-collection--query
                 (plist-get vulpea-ui-collection--view :filter)))
         (ctx (vulpea-ui-collection--context columns)))
    (setq vulpea-ui-collection--ctx ctx)
    (clrhash vulpea-ui-collection--note-table)
    (dolist (note notes)
      (puthash (vulpea-note-id note) note
               vulpea-ui-collection--note-table))
    (setq tabulated-list-entries
          (vulpea-ui-collection--entries
           notes columns vulpea-ui-collection--marked ctx))
    (vulpea-ui-collection--update-format columns)
    (tabulated-list-print t)
    (when (null tabulated-list-entries)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (propertize
                   (substitute-command-keys
                    (concat "No notes match this filter.  "
                            "\\[vulpea-ui-collection-filter-clear] clears it, "
                            "\\[vulpea-ui-collection-menu] lists every command."))
                   'face 'shadow)))))))

(defvar vulpea-ui-collection--worker-refresh-pending nil
  "Non-nil when background extraction changed data during a busy burst.")

(defvar-local vulpea-ui-collection--stale nil
  "Non-nil when the database changed while this buffer was not displayed.
The buffer re-queries when it is shown again instead of on every
change nobody sees.")

(defun vulpea-ui-collection--on-worker-done (_path status _count)
  "Refresh visible collection buffers when background extraction lands.
Mirrors `vulpea-ui--on-worker-done': STATUS `applied' and `missing'
mark the database dirty; the refresh is deferred while the worker is
busy (bulk syncs) and flushed once when the burst drains.  Buffers
not shown in any window are only marked stale - they re-query when
displayed, so a bulk sync does not run one query per hidden view."
  (when (memq status '(applied missing))
    (setq vulpea-ui-collection--worker-refresh-pending t))
  (when (and vulpea-ui-collection--worker-refresh-pending
             (or (not (fboundp 'vulpea-db-worker-busy-p))
                 (not (vulpea-db-worker-busy-p))))
    (setq vulpea-ui-collection--worker-refresh-pending nil)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (derived-mode-p 'vulpea-ui-collection-mode)
          (if (get-buffer-window buf 'visible)
              (vulpea-ui-collection-refresh)
            (setq vulpea-ui-collection--stale t)))))))

(defun vulpea-ui-collection--on-displayed (window)
  "Re-query a stale collection buffer once it is shown in WINDOW.
Runs on the buffer-local `window-buffer-change-functions'; WINDOW is
nil when called outside that hook."
  (with-current-buffer (if (windowp window)
                           (window-buffer window)
                         (current-buffer))
    (when (and (derived-mode-p 'vulpea-ui-collection-mode)
               vulpea-ui-collection--stale)
      (setq vulpea-ui-collection--stale nil)
      (vulpea-ui-collection-refresh))))

;;;; Marks

(defun vulpea-ui-collection--rebuilt-row (id fallback)
  "Return a fresh entry vector for note ID, FALLBACK when it is unknown.
The row is rebuilt from the note so the marked face covers (or leaves)
every cell; FALLBACK only gets its mark cell fixed."
  (if-let* ((note (gethash id vulpea-ui-collection--note-table)))
      (cadr (vulpea-ui-collection--entry
             note
             (mapcar #'vulpea-ui-collection--normalize-column
                     (vulpea-ui-collection--view-columns))
             vulpea-ui-collection--marked
             vulpea-ui-collection--ctx))
    (let ((vec (copy-sequence fallback)))
      (aset vec 0 (if (gethash id vulpea-ui-collection--marked) "*" " "))
      vec)))

(defun vulpea-ui-collection--sync-marks ()
  "Sync all rows with the marked set and re-print the table.
Used by whole-table operations; a single mark goes through
`vulpea-ui-collection--set-mark-at-point' instead, which touches only
its own row."
  (dolist (entry tabulated-list-entries)
    (setcar (cdr entry)
            (vulpea-ui-collection--rebuilt-row (car entry) (cadr entry))))
  (tabulated-list-print t))

(defun vulpea-ui-collection--set-mark-at-point (on)
  "Set the mark of the row at point to ON, re-printing only that row.
Re-printing the whole table on every mark is unusable on large
collections, so the entry vector is swapped (marks still survive
sorting) and the buffer line is rewritten in place."
  (when-let* ((id (tabulated-list-get-id))
              (entry (assoc id tabulated-list-entries)))
    (if on
        (puthash id t vulpea-ui-collection--marked)
      (remhash id vulpea-ui-collection--marked))
    (let ((new (vulpea-ui-collection--rebuilt-row id (cadr entry)))
          (inhibit-read-only t)
          (pos (line-beginning-position)))
      (setcar (cdr entry) new)
      (delete-region pos (min (point-max) (1+ (line-end-position))))
      (goto-char pos)
      (tabulated-list-print-entry id new)
      (goto-char pos))))

(defun vulpea-ui-collection--mark-region (on)
  "Set the mark of every row the region touches to ON."
  (let ((beg (region-beginning))
        (end (region-end)))
    (deactivate-mark)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (when-let* ((id (tabulated-list-get-id)))
          (if on
              (puthash id t vulpea-ui-collection--marked)
            (remhash id vulpea-ui-collection--marked)))
        (forward-line 1))))
  (vulpea-ui-collection--sync-marks))

(defun vulpea-ui-collection-mark ()
  "Mark the note at point and move to the next line.
With an active region, mark every row the region touches instead."
  (interactive)
  (if (use-region-p)
      (vulpea-ui-collection--mark-region t)
    (vulpea-ui-collection--set-mark-at-point t)
    (forward-line 1)))

(defun vulpea-ui-collection-unmark ()
  "Unmark the note at point and move to the next line.
With an active region, unmark every row the region touches instead."
  (interactive)
  (if (use-region-p)
      (vulpea-ui-collection--mark-region nil)
    (vulpea-ui-collection--set-mark-at-point nil)
    (forward-line 1)))

(defun vulpea-ui-collection-mark-regexp (regexp)
  "Mark every note in the view whose title matches REGEXP."
  (interactive (list (read-regexp "Mark titles matching: ")))
  (let ((count 0))
    (dolist (entry tabulated-list-entries)
      (when-let* ((note (gethash (car entry)
                                 vulpea-ui-collection--note-table)))
        (when (string-match-p regexp (or (vulpea-note-title note) ""))
          (puthash (car entry) t vulpea-ui-collection--marked)
          (setq count (1+ count)))))
    (vulpea-ui-collection--sync-marks)
    (message "Marked %d note(s)" count)))

(defun vulpea-ui-collection-mark-by-tag (tag)
  "Mark every note in the view carrying TAG."
  (interactive
   (list (vulpea-ui-collection--read-tag "Mark notes tagged: ")))
  (let ((count 0))
    (dolist (entry tabulated-list-entries)
      (when-let* ((note (gethash (car entry)
                                 vulpea-ui-collection--note-table)))
        (when (member tag (vulpea-note-tags note))
          (puthash (car entry) t vulpea-ui-collection--marked)
          (setq count (1+ count)))))
    (vulpea-ui-collection--sync-marks)
    (message "Marked %d note(s)" count)))

(defun vulpea-ui-collection-unmark-all ()
  "Unmark all notes."
  (interactive)
  (clrhash vulpea-ui-collection--marked)
  (vulpea-ui-collection--sync-marks))

(defun vulpea-ui-collection-toggle-marks ()
  "Invert the mark of every note currently in the view."
  (interactive)
  (dolist (entry tabulated-list-entries)
    (let ((id (car entry)))
      (if (gethash id vulpea-ui-collection--marked)
          (remhash id vulpea-ui-collection--marked)
        (puthash id t vulpea-ui-collection--marked))))
  (vulpea-ui-collection--sync-marks))

(defun vulpea-ui-collection--notes-for-action ()
  "Return the notes to act on.
The marked notes when any are marked, otherwise the note at point."
  (let (notes)
    (maphash (lambda (id _)
               (when-let* ((note (gethash id
                                          vulpea-ui-collection--note-table)))
                 (push note notes)))
             vulpea-ui-collection--marked)
    (or notes
        (when-let* ((id (tabulated-list-get-id))
                    (note (gethash id vulpea-ui-collection--note-table)))
          (list note)))))

;;;; Filter refinement

(defun vulpea-ui-collection--filter-put (filter key value)
  "Return a copy of FILTER with KEY set to VALUE."
  (plist-put (copy-sequence filter) key value))

(defun vulpea-ui-collection--filter-add-to (filter key value)
  "Return a copy of FILTER with VALUE appended to the list at KEY."
  (vulpea-ui-collection--filter-put
   filter key (append (plist-get filter key) (list value))))

(defun vulpea-ui-collection--set-filter (filter)
  "Install FILTER as the current view's filter and re-query."
  (setq vulpea-ui-collection--view
        (plist-put (copy-sequence vulpea-ui-collection--view)
                   :filter filter))
  (vulpea-ui-collection-refresh))

(defun vulpea-ui-collection--current-filter ()
  "Return the current view's filter."
  (plist-get vulpea-ui-collection--view :filter))

(defun vulpea-ui-collection-filter-by-tag ()
  "Narrow the view to notes carrying one more tag."
  (interactive)
  (vulpea-ui-collection--set-filter
   (vulpea-ui-collection--filter-add-to
    (vulpea-ui-collection--current-filter)
    :tags-all (vulpea-ui-collection--read-tag "Require tag: "))))

(defun vulpea-ui-collection-filter-exclude-tag ()
  "Narrow the view to notes not carrying a tag."
  (interactive)
  (vulpea-ui-collection--set-filter
   (vulpea-ui-collection--filter-add-to
    (vulpea-ui-collection--current-filter)
    :tags-none (vulpea-ui-collection--read-tag "Exclude tag: "))))

(defun vulpea-ui-collection-filter-by-title ()
  "Narrow the view to notes whose title matches a regexp.
Empty input drops the title condition."
  (interactive)
  (let ((re (read-string "Title regexp: "
                         (plist-get (vulpea-ui-collection--current-filter)
                                    :title))))
    (vulpea-ui-collection--set-filter
     (vulpea-ui-collection--filter-put
      (vulpea-ui-collection--current-filter)
      :title (unless (string-empty-p re) re)))))

(defun vulpea-ui-collection-filter-by-directory ()
  "Narrow the view to notes under a directory.
An absolute directory matches as a path prefix, a relative one as a
path component.  Empty input drops the condition."
  (interactive)
  (let ((dir (read-string "Directory (absolute or relative): "
                          (plist-get (vulpea-ui-collection--current-filter)
                                     :directory))))
    (vulpea-ui-collection--set-filter
     (vulpea-ui-collection--filter-put
      (vulpea-ui-collection--current-filter)
      :directory (unless (string-empty-p dir) dir)))))

(defun vulpea-ui-collection-filter-by-meta ()
  "Narrow the view on a metadata field.
Empty value means the field must merely be present."
  (interactive)
  (let* ((key (vulpea-ui-collection--read-meta-key "Meta key: "))
         (value (read-string (format "Value for %s (empty for presence): "
                                     key)))
         (filter (vulpea-ui-collection--current-filter)))
    (vulpea-ui-collection--set-filter
     (vulpea-ui-collection--filter-put
      filter :meta
      (append (plist-get filter :meta)
              (list (cons key (if (string-empty-p value) t value))))))))

(defun vulpea-ui-collection-filter-by-level ()
  "Narrow the view to notes of a given level.
Level 0 keeps file-level notes only; \"any\" drops the condition."
  (interactive)
  (let ((level (completing-read "Level (any, 0 for file-level, 1, ...): "
                                '("any" "0" "1" "2" "3"))))
    (vulpea-ui-collection--set-filter
     (vulpea-ui-collection--filter-put
      (vulpea-ui-collection--current-filter)
      :level (unless (equal level "any")
               (string-to-number level))))))

(defun vulpea-ui-collection--filter-conditions (filter)
  "Return an alist of condition label to (KEY . VALUE) for FILTER.
Labels match `vulpea-ui-collection--filter-description'; the cdr
identifies the condition for `vulpea-ui-collection--filter-remove'."
  (let (conditions)
    (dolist (tag (plist-get filter :tags-all))
      (push (cons (concat "#" tag) (cons :tags-all tag)) conditions))
    (dolist (tag (plist-get filter :tags-any))
      (push (cons (concat "#" tag "?") (cons :tags-any tag)) conditions))
    (dolist (tag (plist-get filter :tags-none))
      (push (cons (concat "-#" tag) (cons :tags-none tag)) conditions))
    (when-let* ((level (plist-get filter :level)))
      (push (cons (format "level:%d" level) (cons :level nil)) conditions))
    (when-let* ((dir (plist-get filter :directory)))
      (push (cons (concat "dir:" dir) (cons :directory nil)) conditions))
    (when-let* ((re (plist-get filter :title)))
      (push (cons (concat "title:" re) (cons :title nil)) conditions))
    (dolist (condition (plist-get filter :meta))
      (push (cons (if (eq (cdr condition) t)
                      (format "%s:*" (car condition))
                    (format "%s:%s" (car condition) (cdr condition)))
                  (cons :meta condition))
            conditions))
    (when (plist-get filter :predicate)
      (push (cons "predicate" (cons :predicate nil)) conditions))
    (nreverse conditions)))

(defun vulpea-ui-collection--filter-remove (filter key &optional value)
  "Return a copy of FILTER without one condition.
KEY names the condition; for the list-valued keys (tags and meta)
VALUE picks the element to drop, scalar keys are cleared entirely."
  (pcase key
    ((or :tags-all :tags-any :tags-none :meta)
     (vulpea-ui-collection--filter-put
      filter key (remove value (plist-get filter key))))
    (_ (vulpea-ui-collection--filter-put filter key nil))))

(defun vulpea-ui-collection-filter-remove-condition ()
  "Remove a single condition from the current filter."
  (interactive)
  (let ((conditions (vulpea-ui-collection--filter-conditions
                     (vulpea-ui-collection--current-filter))))
    (unless conditions (user-error "The filter has no conditions"))
    (let* ((label (completing-read "Remove condition: "
                                   (mapcar #'car conditions)
                                   nil t))
           (condition (cdr (assoc label conditions))))
      (vulpea-ui-collection--set-filter
       (vulpea-ui-collection--filter-remove
        (vulpea-ui-collection--current-filter)
        (car condition)
        (cdr condition))))))

(defun vulpea-ui-collection-filter-clear ()
  "Drop every filter condition and show the whole collection."
  (interactive)
  (vulpea-ui-collection--set-filter nil))

;;;; Column management

(defun vulpea-ui-collection--known-meta-keys ()
  "Return the metadata keys present on the notes currently in the view."
  (let (keys)
    (maphash (lambda (_ note)
               (dolist (item (vulpea-note-meta note))
                 (push (car item) keys)))
             vulpea-ui-collection--note-table)
    (seq-uniq keys)))

(defun vulpea-ui-collection--available-columns ()
  "Return an alist of column display name to column descriptor.
Built-in columns plus a meta:KEY column for every metadata key found
on the notes currently in the view."
  (append
   (mapcar (lambda (id) (cons (symbol-name id) id))
           '(title context tags todo priority scheduled deadline
             created modified links backlinks aliases file))
   (mapcar (lambda (key) (cons (format "meta:%s" key) (list 'meta key)))
           (vulpea-ui-collection--known-meta-keys))))

(defun vulpea-ui-collection--set-columns (columns)
  "Install COLUMNS on the current view and re-render.
The sort key is dropped when it points at a column that is gone."
  (setq vulpea-ui-collection--view
        (plist-put (copy-sequence vulpea-ui-collection--view)
                   :columns columns))
  (setq tabulated-list-format (vulpea-ui-collection--format columns))
  (when (and tabulated-list-sort-key
             (not (seq-some
                   (lambda (col)
                     (equal (car tabulated-list-sort-key)
                            (plist-get
                             (vulpea-ui-collection--normalize-column col)
                             :name)))
                   columns)))
    (setq tabulated-list-sort-key nil))
  (tabulated-list-init-header)
  (vulpea-ui-collection-refresh))

(defun vulpea-ui-collection-add-column (column)
  "Append COLUMN to the current view.
Interactively, complete over the built-in columns and the meta keys of
the notes in the view; free input is treated as a meta key."
  (interactive
   (list (let* ((available (vulpea-ui-collection--available-columns))
                (name (completing-read "Add column: " available)))
           (or (cdr (assoc name available))
               (list 'meta (string-remove-prefix "meta:" name))))))
  (vulpea-ui-collection--set-columns
   (append (vulpea-ui-collection--view-columns) (list column))))

(defun vulpea-ui-collection-remove-column (name)
  "Remove the column named NAME from the current view."
  (interactive
   (list (completing-read
          "Remove column: "
          (mapcar (lambda (col)
                    (plist-get (vulpea-ui-collection--normalize-column col)
                               :name))
                  (vulpea-ui-collection--view-columns))
          nil t)))
  (vulpea-ui-collection--set-columns
   (seq-remove
    (lambda (col)
      (equal (plist-get (vulpea-ui-collection--normalize-column col) :name)
             name))
    (vulpea-ui-collection--view-columns))))

;;;; Actions

(defun vulpea-ui-collection--require (fn)
  "Signal a user error unless FN is available in this vulpea."
  (unless (fboundp fn)
    (user-error "This vulpea has no `%s' (need a newer vulpea)" fn)))

(defun vulpea-ui-collection--read-tag (prompt)
  "Read a tag with PROMPT, completing over known tags when possible."
  (let ((tags (when (fboundp 'vulpea-db-query-tags)
                (ignore-errors (vulpea-db-query-tags)))))
    (completing-read prompt tags)))

(defun vulpea-ui-collection--read-meta-key (prompt)
  "Read a meta key with PROMPT, completing over the selection's keys."
  (let ((keys (seq-uniq
               (mapcan (lambda (note)
                         (mapcar #'car (vulpea-note-meta note)))
                       (vulpea-ui-collection--notes-for-action)))))
    (completing-read prompt keys)))

(defun vulpea-ui-collection--read-function (prompt)
  "Read the name of an existing function with PROMPT."
  (intern (completing-read prompt obarray #'fboundp t)))

(defun vulpea-ui-collection--confirm (action notes)
  "Ask before applying ACTION to NOTES; return non-nil to proceed.
ACTION is the leading part of the question, e.g. \"Add tag x to\".
The prompt lists the affected notes so a bulk edit is never a
surprise; a single-note edit applies without asking."
  (or (null (cdr notes))
      (yes-or-no-p
       (format "%s %d note(s) (%s%s)? "
               action
               (length notes)
               (string-join
                (seq-take (mapcar #'vulpea-note-title notes) 5) ", ")
               (if (> (length notes) 5) ", ..." "")))))

(defun vulpea-ui-collection-add-tag ()
  "Add a tag to the marked notes (or the note at point)."
  (interactive)
  (vulpea-ui-collection--require 'vulpea-tags-batch-add)
  (let ((notes (vulpea-ui-collection--notes-for-action)))
    (unless notes (user-error "No notes selected"))
    (let ((tag (vulpea-ui-collection--read-tag "Add tag: ")))
      (when (vulpea-ui-collection--confirm
             (format "Add tag %s to" tag) notes)
        (vulpea-tags-batch-add notes tag)
        (message "Added tag %s to %d note(s)" tag (length notes))
        (vulpea-ui-collection-refresh)))))

(defun vulpea-ui-collection-remove-tag ()
  "Remove a tag from the marked notes (or the note at point)."
  (interactive)
  (vulpea-ui-collection--require 'vulpea-tags-batch-remove)
  (let ((notes (vulpea-ui-collection--notes-for-action)))
    (unless notes (user-error "No notes selected"))
    (let ((tag (completing-read
                "Remove tag: "
                (seq-uniq (mapcan (lambda (note)
                                    (copy-sequence (vulpea-note-tags note)))
                                  notes)))))
      (when (vulpea-ui-collection--confirm
             (format "Remove tag %s from" tag) notes)
        (vulpea-tags-batch-remove notes tag)
        (message "Removed tag %s from %d note(s)" tag (length notes))
        (vulpea-ui-collection-refresh)))))

(defun vulpea-ui-collection-set-meta ()
  "Set a metadata field on the marked notes (or the note at point)."
  (interactive)
  (vulpea-ui-collection--require 'vulpea-meta-batch-set)
  (let ((notes (vulpea-ui-collection--notes-for-action)))
    (unless notes (user-error "No notes selected"))
    (let* ((key (vulpea-ui-collection--read-meta-key "Set meta key: "))
           (value (read-string (format "Value for %s: " key))))
      (when (vulpea-ui-collection--confirm
             (format "Set %s to %s on" key value) notes)
        (vulpea-meta-batch-set notes key value)
        (message "Set %s on %d note(s)" key (length notes))
        (vulpea-ui-collection-refresh)))))

(defun vulpea-ui-collection-remove-meta ()
  "Remove a metadata field from the marked notes (or the note at point)."
  (interactive)
  (vulpea-ui-collection--require 'vulpea-meta-batch-remove)
  (let ((notes (vulpea-ui-collection--notes-for-action)))
    (unless notes (user-error "No notes selected"))
    (let ((key (vulpea-ui-collection--read-meta-key "Remove meta key: ")))
      (when (vulpea-ui-collection--confirm
             (format "Remove %s from" key) notes)
        (vulpea-meta-batch-remove notes key)
        (message "Removed %s from %d note(s)" key (length notes))
        (vulpea-ui-collection-refresh)))))

(defun vulpea-ui-collection-delete ()
  "Delete the files of the marked file-level notes, with confirmation.
Heading-level notes are skipped; deleting a heading in place is not
supported yet.  The database is updated immediately when vulpea's sync
machinery is available, otherwise the file watcher picks the removal
up on its own."
  (interactive)
  (let* ((notes (vulpea-ui-collection--notes-for-action))
         (files (seq-filter (lambda (note)
                              (= 0 (vulpea-note-level note)))
                            notes))
         (skipped (- (length notes) (length files))))
    (unless files (user-error "No file-level notes selected"))
    (when (yes-or-no-p
           (format "Delete %d note file(s) (%s)? "
                   (length files)
                   (string-join
                    (seq-take (mapcar #'vulpea-note-title files) 5)
                    ", ")))
      (dolist (note files)
        (let ((path (vulpea-note-path note)))
          (delete-file path)
          (when (fboundp 'vulpea-db-sync--handle-removed-file)
            (vulpea-db-sync--handle-removed-file path))
          (remhash (vulpea-note-id note) vulpea-ui-collection--marked)))
      (when (> skipped 0)
        (message "Skipped %d heading-level note(s)" skipped))
      (vulpea-ui-collection-refresh))))

(defun vulpea-ui-collection-apply ()
  "Apply a function to the marked notes (or the note at point).
The function is called once with the list of selected notes."
  (interactive)
  (let ((notes (vulpea-ui-collection--notes-for-action)))
    (unless notes (user-error "No notes selected"))
    (let ((fn (vulpea-ui-collection--read-function "Apply function: ")))
      (funcall fn notes)
      (vulpea-ui-collection-refresh))))

(defun vulpea-ui-collection--notes-for-export ()
  "Return the notes to copy or export.
The marked notes when any are marked, otherwise every note in the
view, in display order."
  (let ((marked (delq nil
                      (mapcar (lambda (entry)
                                (when (gethash (car entry)
                                               vulpea-ui-collection--marked)
                                  (gethash (car entry)
                                           vulpea-ui-collection--note-table)))
                              tabulated-list-entries))))
    (or marked
        (delq nil
              (mapcar (lambda (entry)
                        (gethash (car entry)
                                 vulpea-ui-collection--note-table))
                      tabulated-list-entries)))))

(defun vulpea-ui-collection--link-string (note)
  "Return an org link to NOTE."
  (if (fboundp 'vulpea-utils-link-make-string)
      (vulpea-utils-link-make-string note)
    (format "[[id:%s][%s]]"
            (vulpea-note-id note)
            (vulpea-note-title note))))

(defun vulpea-ui-collection--links-list (notes)
  "Return NOTES as an org list of links, one per line."
  (mapconcat (lambda (note)
               (concat "- " (vulpea-ui-collection--link-string note)))
             notes
             "\n"))

(defun vulpea-ui-collection-copy-links ()
  "Copy the selection to the kill ring as an org list of links.
The marked notes, or the whole view when nothing is marked."
  (interactive)
  (let ((notes (vulpea-ui-collection--notes-for-export)))
    (unless notes (user-error "Nothing to copy"))
    (kill-new (vulpea-ui-collection--links-list notes))
    (message "Copied %d link(s)" (length notes))))

(defun vulpea-ui-collection-export ()
  "Export the selection to an org buffer as a list of links.
The marked notes, or the whole view when nothing is marked."
  (interactive)
  (let ((notes (vulpea-ui-collection--notes-for-export))
        (name (or (plist-get vulpea-ui-collection--view :name)
                  "collection")))
    (unless notes (user-error "Nothing to export"))
    (let ((buffer (generate-new-buffer
                   (format "*vulpea-collection-export: %s*" name))))
      (with-current-buffer buffer
        (org-mode)
        (insert "#+title: " name "\n\n"
                (vulpea-ui-collection--links-list notes)
                "\n"))
      (pop-to-buffer buffer)
      (message "Exported %d note(s)" (length notes)))))

(defun vulpea-ui-collection--note-at-point ()
  "Return the note of the row at point, if any."
  (when-let* ((id (tabulated-list-get-id)))
    (gethash id vulpea-ui-collection--note-table)))

(defun vulpea-ui-collection--goto-note (note)
  "Move point to NOTE's position in the current buffer and reveal it."
  (widen)
  (goto-char (vulpea-note-pos note))
  (when (and (derived-mode-p 'org-mode)
             (fboundp 'org-fold-show-context))
    (org-fold-show-context)))

(defun vulpea-ui-collection-visit ()
  "Visit the note at point."
  (interactive)
  (when-let* ((note (vulpea-ui-collection--note-at-point)))
    (find-file (vulpea-note-path note))
    (vulpea-ui-collection--goto-note note)))

(defun vulpea-ui-collection-visit-other-window ()
  "Visit the note at point in another window."
  (interactive)
  (when-let* ((note (vulpea-ui-collection--note-at-point)))
    (find-file-other-window (vulpea-note-path note))
    (vulpea-ui-collection--goto-note note)))

(defun vulpea-ui-collection-preview ()
  "Display the note at point in another window without leaving the table."
  (interactive)
  (when-let* ((note (vulpea-ui-collection--note-at-point)))
    (let* ((buffer (find-file-noselect (vulpea-note-path note)))
           (window (display-buffer buffer
                                   '(nil (inhibit-same-window . t)))))
      (when (window-live-p window)
        (with-selected-window window
          (vulpea-ui-collection--goto-note note)
          (recenter))))))

;;;; Views

(defun vulpea-ui-collection--resolve-view (input)
  "Resolve INPUT into a view spec.
INPUT is the name of a saved view from `vulpea-ui-collection-views',
a comma-separated list of tags for an ad-hoc view over notes carrying
all of them, or empty for a view over the whole collection."
  (let ((input (string-trim input)))
    (cond
     ((string-empty-p input)
      (list :name "all"))
     ((cdr (assoc input vulpea-ui-collection-views))
      (append (list :name input)
              (cdr (assoc input vulpea-ui-collection-views))))
     (t
      (list :name input
            :filter (list :tags-all (split-string input "[, ]+" t)))))))

(defun vulpea-ui-collection-save-view (name)
  "Save the current view configuration under NAME.
The filter, columns and current sort order are stored in
`vulpea-ui-collection-views' and persisted via Customize."
  (interactive
   (list (read-string "Save view as: "
                      (plist-get vulpea-ui-collection--view :name))))
  (unless (derived-mode-p 'vulpea-ui-collection-mode)
    (user-error "Not in a collection buffer"))
  (let ((spec (list :filter (plist-get vulpea-ui-collection--view :filter)
                    :columns (vulpea-ui-collection--view-columns)
                    :sort tabulated-list-sort-key)))
    (setf (alist-get name vulpea-ui-collection-views nil nil #'equal) spec)
    (condition-case nil
        (customize-save-variable 'vulpea-ui-collection-views
                                 vulpea-ui-collection-views)
      (error (message "View %s saved for this session only" name)))))

(defun vulpea-ui-collection--menu-description ()
  "Return the headline of the collection menu."
  (let ((filter (vulpea-ui-collection--filter-description
                 (plist-get vulpea-ui-collection--view :filter))))
    (format "%s - %d notes - filter: %s"
            (or (plist-get vulpea-ui-collection--view :name) "collection")
            (length tabulated-list-entries)
            (if (string-empty-p filter) "none (all notes)" filter))))

(transient-define-prefix vulpea-ui-collection-menu ()
  "Every collection view command in one place."
  [:description vulpea-ui-collection--menu-description
   ["Filter"
    ("t" "require tag" vulpea-ui-collection-filter-by-tag :transient t)
    ("T" "exclude tag" vulpea-ui-collection-filter-exclude-tag
     :transient t)
    ("s" "title regexp" vulpea-ui-collection-filter-by-title :transient t)
    ("d" "directory" vulpea-ui-collection-filter-by-directory
     :transient t)
    ("M" "meta field" vulpea-ui-collection-filter-by-meta :transient t)
    ("l" "level" vulpea-ui-collection-filter-by-level :transient t)
    ("K" "remove condition" vulpea-ui-collection-filter-remove-condition
     :transient t)
    ("C" "clear" vulpea-ui-collection-filter-clear :transient t)]
   ["Columns"
    ("a" "add" vulpea-ui-collection-add-column :transient t)
    ("k" "remove" vulpea-ui-collection-remove-column :transient t)]
   ["Marks"
    ("m" "mark" vulpea-ui-collection-mark :transient t)
    ("u" "unmark" vulpea-ui-collection-unmark :transient t)
    ("U" "unmark all" vulpea-ui-collection-unmark-all :transient t)
    ("i" "invert" vulpea-ui-collection-toggle-marks :transient t)
    ("r" "mark by title regexp" vulpea-ui-collection-mark-regexp
     :transient t)
    ("#" "mark by tag" vulpea-ui-collection-mark-by-tag :transient t)]]
  [["Act on selection"
    ("+" "add tag" vulpea-ui-collection-add-tag)
    ("-" "remove tag" vulpea-ui-collection-remove-tag)
    ("e" "set meta" vulpea-ui-collection-set-meta)
    ("E" "remove meta" vulpea-ui-collection-remove-meta)
    ("D" "delete" vulpea-ui-collection-delete)
    ("x" "apply function" vulpea-ui-collection-apply)
    ("y" "copy as links" vulpea-ui-collection-copy-links)
    ("Y" "export to org buffer" vulpea-ui-collection-export)]
   ["View"
    ("RET" "visit note" vulpea-ui-collection-visit)
    ("o" "visit other window" vulpea-ui-collection-visit-other-window)
    ("C-o" "preview" vulpea-ui-collection-preview :transient t)
    ("g" "refresh" vulpea-ui-collection-refresh)
    ("w" "save view" vulpea-ui-collection-save-view)
    ("v" "switch view" vulpea-ui-collection)]])

;;;###autoload
(defun vulpea-ui-collection-open (view)
  "Open a collection buffer for the VIEW spec.
VIEW is a plist with :name, :filter, :columns and :sort, see
`vulpea-ui-collection-views' for the format."
  (let* ((name (or (plist-get view :name) "collection"))
         (buf (get-buffer-create (format "*vulpea-collection: %s*" name))))
    (with-current-buffer buf
      (unless (derived-mode-p 'vulpea-ui-collection-mode)
        (vulpea-ui-collection-mode))
      (setq vulpea-ui-collection--view view)
      (setq tabulated-list-format
            (vulpea-ui-collection--format
             (vulpea-ui-collection--view-columns)))
      (setq tabulated-list-sort-key (plist-get view :sort))
      (tabulated-list-init-header)
      (vulpea-ui-collection-refresh))
    (switch-to-buffer buf)))

;;;###autoload
(defun vulpea-ui-collection (view)
  "Open a collection view over vulpea notes.
Interactively, VIEW is read with completion over the saved views in
`vulpea-ui-collection-views'; free input is treated as a
comma-separated list of tags.  From Lisp, VIEW may also be a view
spec plist, which is passed to `vulpea-ui-collection-open'."
  (interactive
   (list (completing-read
          "Collection (saved view, tags, or empty for all notes): "
          (mapcar #'car vulpea-ui-collection-views))))
  (vulpea-ui-collection-open
   (if (stringp view)
       (vulpea-ui-collection--resolve-view view)
     view)))

(provide 'vulpea-ui)
;;; vulpea-ui.el ends here
