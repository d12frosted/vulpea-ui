;;; vulpea-ui.el --- Sidebar infrastructure and widget framework for vulpea notes -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Boris Buliga
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/vulpea-ui
;; Version: 1.1.0
;; Package-Requires: ((emacs "29.1") (vulpea "2.4.0") (vui "1.0"))
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
  (let ((widget (widget-at (point))))
    (when widget
      (widget-apply widget :action))))

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
      (let* ((last-ref (vui-use-ref nil))
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
              ('shown (vulpea-ui--render-unlinked-mentions-body data))
              ('error
               (vui-muted (format "Unavailable: %s"
                                  (plist-get result :error))))
              (_ (vui-muted "Searching…")))))))))

(defun vulpea-ui--render-unlinked-mentions-body (data)
  "Render incoming mention DATA as grouped context lines."
  (let ((groups (vulpea-ui--group-mentions data)))
    (if groups
        (vui-vstack
         :spacing 1
         (seq-map #'vulpea-ui--render-mention-group groups))
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

(defun vulpea-ui--render-mention-group (group)
  "Render a mention GROUP: the mentioning note link and its context lines."
  (let ((note (plist-get group :note))
        (path (plist-get group :path))
        (mentions (plist-get group :mentions)))
    (vui-vstack
     :spacing 0
     (if note
         (vui-component 'vulpea-ui-note-link :note note)
       (vui-muted (file-name-nondirectory path)))
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

(defun vulpea-ui--schema-health (note)
  "Return schema health for NOTE, or nil when no schema is applicable.
The result is a plist with :schemas (the applicable schema names) and
:violations (a list of `vulpea-violation' across them).  Returns nil
when NOTE matches no registered schema, so the widget hides entirely."
  (when note
    (when-let* ((schemas (vulpea-schema-applicable note)))
      (list :schemas schemas
            :violations (vulpea-schema-note-violations note)))))

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
  "Return the end of NOTE's scope in the current buffer.
Assumes the current buffer is NOTE's file."
  (save-excursion
    (goto-char (vulpea-note-pos note))
    (if (> (vulpea-note-level note) 0)
        (progn (org-end-of-subtree t t) (point))
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
A value violation goes to the offending field's line; a missing field
\(which has no line yet) goes to NOTE's metadata block, or to where
metadata would be inserted."
  (let* ((path (vulpea-note-path note))
         (buf (and path (find-buffer-visiting path)))
         (field (vulpea-violation-field violation)))
    (or
     (when (and buf field
                (not (eq (vulpea-violation-type violation) 'missing-required)))
       (with-current-buffer buf
         (save-excursion
           (goto-char (vulpea-note-pos note))
           (let ((end (vulpea-ui--schema-note-end note)))
             (when (re-search-forward
                    (format "^[ \t]*-[ \t]+%s[ \t]+::" (regexp-quote field))
                    end t)
               (line-beginning-position))))))
     (vulpea-ui--schema-meta-position note))))

(declare-function vulpea-schema-fix-violation "vulpea" (violation &optional bound))

(defun vulpea-ui--schema-fix-violation-action (note violation)
  "Fix VIOLATION on NOTE, persist the change, and refresh the sidebar.
Prompt for a corrected value, write it to NOTE's file, re-index, and
re-render.  Do nothing when the prompt is skipped or NOTE's buffer is
not visiting a file."
  (when-let* ((path (vulpea-note-path note))
              (buf (find-buffer-visiting path)))
    (when (with-current-buffer buf
            (vulpea-schema-fix-violation
             violation
             (when (> (vulpea-note-level note) 0)
               (vulpea-note-pos note))))
      (with-current-buffer buf (save-buffer))
      (vulpea-db-update-file path)
      (vulpea-ui-sidebar-refresh))))

(defun vulpea-ui--render-schema-violation (violation note)
  "Render one row for VIOLATION on NOTE.
The row is a severity bullet, an optional quick-fix button, the field
name as a button that jumps to the offending field, and a terse reason."
  (let ((face (if (eq (vulpea-ui--schema-violation-severity
                       (vulpea-violation-type violation))
                      'error)
                  'vulpea-ui-schema-health-error-face
                'vulpea-ui-schema-health-warning-face)))
    (apply
     #'vui-hstack
     (delq
      nil
      (list
       (vui-text vulpea-ui-schema-health-bullet :face face)
       (when (fboundp 'vulpea-schema-fix-violation)
         (vui-button "fix"
           :face 'vulpea-ui-schema-health-action-face
           :on-click (lambda ()
                       (vulpea-ui--schema-fix-violation-action note violation))
           :help-echo "Prompt for a value and fix this violation"))
       (vui-button (or (vulpea-violation-field violation) "")
         :face 'vulpea-ui-schema-health-field-face
         :no-decoration t
         :help-echo nil
         :on-click (lambda ()
                     (vulpea-ui--jump-to-position
                      note
                      (vulpea-ui--schema-violation-position violation note))))
       (vui-text (vulpea-ui--schema-violation-reason violation note)
         :face 'vulpea-ui-schema-health-message-face))))))

(vui-defcomponent vulpea-ui-widget-schema-health ()
  "Widget flagging schema violations for the current note.
Renders a healthy status when the note conforms, or the list of
violations when it does not.  The widget hides entirely when no schema
applies (see its :predicate at registration)."
  :render
  (let ((note (use-vulpea-ui-note)))
    (when note
      (let* ((note-buf (when (vulpea-note-path note)
                         (find-buffer-visiting (vulpea-note-path note))))
             (tick (when note-buf (buffer-modified-tick note-buf)))
             (health (vui-use-memo (note tick)
                       (vulpea-ui--schema-health note))))
        (when health
          (let* ((schemas (plist-get health :schemas))
                 (violations (plist-get health :violations))
                 (names (mapconcat #'symbol-name schemas ", ")))
            (vui-component 'vulpea-ui-widget
              :title "Schema"
              :count (when violations (length violations))
              :children
              (lambda ()
                (if violations
                    (apply #'vui-vstack
                           (vui-text (format "%s %s · %d issue%s"
                                             vulpea-ui-schema-health-issue-glyph
                                             names
                                             (length violations)
                                             (if (= (length violations) 1) "" "s"))
                             :face 'vulpea-ui-schema-health-error-face)
                           (seq-map (lambda (v)
                                      (vulpea-ui--render-schema-violation v note))
                                    violations))
                  (vui-text (format "%s %s · healthy"
                                    vulpea-ui-schema-health-ok-glyph names)
                    :face 'vulpea-ui-schema-health-ok-face))))))))))


;;; Built-in widget registration

(vulpea-ui-register-widget 'stats
                           :component 'vulpea-ui-widget-stats
                           :order 100)

(vulpea-ui-register-widget 'schema-health
                           :component 'vulpea-ui-widget-schema-health
                           :predicate (lambda (note)
                                        (and note
                                             (fboundp 'vulpea-schema-note-violations)
                                             (vulpea-schema-applicable note)))
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
Do nothing when the prompt is skipped."
  (when-let* ((buf (vulpea-ui-schema-dashboard--visit-field note violation)))
    (when (with-current-buffer buf
            (vulpea-schema-fix-violation
             violation
             (when (> (vulpea-note-level note) 0) (vulpea-note-pos note))))
      (with-current-buffer buf (save-buffer))
      (vulpea-db-update-file (vulpea-note-path note))
      (vulpea-ui-schema-dashboard-refresh))))

(defun vulpea-ui-schema-dashboard--render-violation (note violation)
  "Render one VIOLATION row for NOTE: bullet, fix, field, and reason."
  (let ((face (if (eq (vulpea-ui--schema-violation-severity
                       (vulpea-violation-type violation))
                      'error)
                  'vulpea-ui-schema-health-error-face
                'vulpea-ui-schema-health-warning-face)))
    (apply
     #'vui-hstack
     (delq
      nil
      (list
       (vui-text vulpea-ui-schema-health-bullet :face face)
       (when (fboundp 'vulpea-schema-fix-violation)
         (vui-button "fix"
           :face 'vulpea-ui-schema-health-action-face
           :on-click (lambda ()
                       (vulpea-ui-schema-dashboard--fix-violation note violation))
           :help-echo "Show the note and fix this violation"))
       (vui-button (or (vulpea-violation-field violation) "")
         :face 'vulpea-ui-schema-health-field-face
         :no-decoration t
         :help-echo nil
         :on-click (lambda ()
                     (vulpea-ui-schema-dashboard--visit-field note violation)))
       (vui-text (vulpea-ui--schema-violation-reason violation note)
         :face 'vulpea-ui-schema-health-message-face))))))

(vui-defcomponent vulpea-ui-schema-dashboard-note (entry indent)
  "One invalid note in the dashboard, collapsible to its violations.
ENTRY is a (note . violations) pair; the note starts collapsed.  INDENT
is the column the row sits at, so the right-aligned count lines up with
the schema headers above."
  :state ((expanded nil))
  :render
  (let* ((note (car entry))
         (violations (cdr entry))
         (count (length violations))
         (indicator (if expanded "▼" "▶")))
    (vui-vstack
     :spacing 0
     (vui-flex
      :width #'vulpea-ui-schema-dashboard--width
      :indent (or indent 0)
      :justify :space-between
      (vui-button (format "%s %s" indicator
                          (or (vulpea-note-title note) "(untitled)"))
        :no-decoration t
        :help-echo "Toggle this note's violations"
        :on-click (lambda () (vui-set-state :expanded (not expanded))))
      (vui-text (format "%d %s" count (if (= count 1) "issue" "issues"))
                :face 'vulpea-ui-schema-health-message-face))
     (when expanded
       (vui-vstack
        :indent (+ (or indent 0) 2)
        :spacing 0
        (seq-map (lambda (v)
                   (vulpea-ui-schema-dashboard--render-violation note v))
                 violations))))))

(vui-defcomponent vulpea-ui-schema-dashboard-section (entry)
  "One schema's section in the dashboard.
ENTRY is a `vulpea-schema-health'.  The header toggles the schema's
invalid notes; a schema with violations starts expanded."
  :state ((expanded :unset))
  :render
  (let* ((invalid (vulpea-schema-health-invalid entry))
         (is-expanded (if (eq expanded :unset) (> invalid 0) expanded))
         (indicator (if is-expanded "▼" "▶"))
         (includes (vulpea-ui-schema-dashboard--includes-text entry)))
    (vui-vstack
     :spacing 0
     (vui-flex
      :width #'vulpea-ui-schema-dashboard--width
      :justify :space-between
      (vui-button (format "%s %s" indicator
                          (symbol-name (vulpea-schema-health-schema entry)))
        :no-decoration t
        :face 'vulpea-ui-schema-dashboard-schema-face
        :help-echo "Toggle this schema"
        :on-click (lambda () (vui-set-state :expanded (not is-expanded))))
      (vui-text (vulpea-ui-schema-dashboard--status-text entry)
                :face (vulpea-ui-schema-dashboard--status-face entry)))
     (when is-expanded
       (let ((note-indent 4))
         (vui-vstack
          :indent note-indent
          :spacing 0
          (when includes
            (vui-text includes :face 'vulpea-ui-schema-health-message-face))
          (seq-map (lambda (h)
                     (vui-component 'vulpea-ui-schema-dashboard-note
                                    :entry h
                                    :indent note-indent
                                    :key (vulpea-note-id (car h))))
                   (vulpea-schema-health-invalid-notes entry))))))))

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
  "Compute schema health and (re-)render the dashboard in the current buffer."
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
                       (buffer-name)))
      ;; right-aligned counts depend on the window width, so reflow on resize
      (when (fboundp 'vui-rerender-on-resize)
        (vui-rerender-on-resize)))))

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
  (let ((buf (get-buffer-create "*vulpea schema*")))
    (with-current-buffer buf
      (unless (derived-mode-p 'vulpea-ui-schema-dashboard-mode)
        (vulpea-ui-schema-dashboard-mode)))
    (pop-to-buffer buf)
    (vulpea-ui-schema-dashboard--render)))

(provide 'vulpea-ui)
;;; vulpea-ui.el ends here
