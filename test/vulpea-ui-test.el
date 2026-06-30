;;; vulpea-ui-test.el --- Tests for vulpea-ui -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Boris Buliga
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Boris Buliga <boris@d12frosted.io>

;;; Commentary:

;; ERT tests for vulpea-ui sidebar and widget functionality.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'vui)
(require 'vulpea-ui)

;;; Test helpers

(defun vulpea-ui-test--can-create-frames-p ()
  "Return non-nil if we can create frames (i.e., not in batch mode)."
  (and (not noninteractive)
       (display-graphic-p)))

(defmacro vulpea-ui-test--with-temp-frame (&rest body)
  "Execute BODY with a temporary frame, cleaning up afterwards.
In batch mode, execute BODY in the current frame instead."
  (declare (indent 0))
  `(if (vulpea-ui-test--can-create-frames-p)
       (let ((frame (make-frame '((visibility . nil)))))
         (unwind-protect
             (with-selected-frame frame
               ,@body)
           (delete-frame frame)))
     ;; In batch mode, just run in current context
     ,@body))

(defun vulpea-ui-test--make-mock-note (&optional id title properties)
  "Create a mock vulpea-note struct with ID, TITLE, and PROPERTIES."
  (let ((id (or id (format "test-%s" (random 10000))))
        (title (or title "Test Note")))
    (make-vulpea-note
     :id id
     :path (expand-file-name (format "/tmp/test-%s.org" id))
     :level 0
     :pos 1
     :title title
     :primary-title title
     :aliases nil
     :tags nil
     :links nil
     :properties properties
     :meta nil)))

(defmacro vulpea-ui-test--with-clean-registry (&rest body)
  "Run BODY with an empty widget registry, restoring state afterwards.
Each stored plist is deep-copied so destructive updates in BODY do not
leak into the saved snapshot."
  (declare (indent 0))
  `(let ((saved (let ((h (make-hash-table :test 'eq)))
                  (maphash (lambda (k v) (puthash k (copy-tree v) h))
                           vulpea-ui--widget-registry)
                  h)))
     (unwind-protect
         (progn
           (clrhash vulpea-ui--widget-registry)
           ,@body)
       (clrhash vulpea-ui--widget-registry)
       (maphash (lambda (k v) (puthash k v vulpea-ui--widget-registry))
                saved))))


;;; Configuration tests

(ert-deftest vulpea-ui-test-default-position ()
  "Test that default sidebar position is 'right."
  (should (eq vulpea-ui-sidebar-position 'right)))

(ert-deftest vulpea-ui-test-default-size ()
  "Test that default sidebar size is 0.33."
  (should (= vulpea-ui-sidebar-size 0.33)))

(ert-deftest vulpea-ui-test-default-collapsed ()
  "Test that widgets are not collapsed by default."
  (should-not vulpea-ui-default-widget-collapsed))

(ert-deftest vulpea-ui-test-default-auto-hide ()
  "Test that auto-hide is enabled by default."
  (should vulpea-ui-sidebar-auto-hide))


;;; Buffer naming tests

(ert-deftest vulpea-ui-test-buffer-name ()
  "Test sidebar buffer name generation."
  (let ((name (vulpea-ui--sidebar-buffer-name)))
    (should (stringp name))
    (should (string-prefix-p "*vulpea-ui-sidebar:" name))
    (should (string-suffix-p "*" name))))


;;; Sidebar visibility tests

(ert-deftest vulpea-ui-test-sidebar-initially-hidden ()
  "Test that sidebar is not visible initially."
  ;; When no sidebar buffer exists, should return nil
  (should-not (vulpea-ui--sidebar-visible-p)))


;;; Display buffer params tests

(ert-deftest vulpea-ui-test-display-params-right ()
  "Test display buffer params for right position."
  (let ((vulpea-ui-sidebar-position 'right)
        (vulpea-ui-sidebar-size 0.25))
    (let ((params (vulpea-ui--display-buffer-params)))
      (should (eq (alist-get 'side params) 'right))
      (should (= (alist-get 'window-width params) 0.25))
      (should (null (alist-get 'window-height params))))))

(ert-deftest vulpea-ui-test-display-params-bottom ()
  "Test display buffer params for bottom position."
  (let ((vulpea-ui-sidebar-position 'bottom)
        (vulpea-ui-sidebar-size 0.2))
    (let ((params (vulpea-ui--display-buffer-params)))
      (should (eq (alist-get 'side params) 'bottom))
      (should (= (alist-get 'window-height params) 0.2))
      (should (null (alist-get 'window-width params))))))


;;; Side slot guarantee tests

(ert-deftest vulpea-ui-test-ensure-side-slot-raises-disabled ()
  "A disabled side (zero slots) is raised to a single slot."
  (should (equal (vulpea-ui--ensure-side-slot '(1 0 0 1) 'right)
                 '(1 0 1 1)))
  (should (equal (vulpea-ui--ensure-side-slot '(0 0 0 0) 'left)
                 '(1 0 0 0)))
  (should (equal (vulpea-ui--ensure-side-slot '(0 0 0 0) 'bottom)
                 '(0 0 0 1))))

(ert-deftest vulpea-ui-test-ensure-side-slot-keeps-positive ()
  "A side that already allows slots is left untouched."
  (should (equal (vulpea-ui--ensure-side-slot '(1 2 3 4) 'right)
                 '(1 2 3 4))))

(ert-deftest vulpea-ui-test-ensure-side-slot-keeps-unlimited ()
  "A nil (unlimited) side is left untouched."
  (should (equal (vulpea-ui--ensure-side-slot '(nil nil nil nil) 'right)
                 '(nil nil nil nil))))

(ert-deftest vulpea-ui-test-ensure-side-slot-does-not-mutate ()
  "The original list is not modified in place."
  (let ((slots (list 1 0 0 1)))
    (vulpea-ui--ensure-side-slot slots 'right)
    (should (equal slots '(1 0 0 1)))))

(ert-deftest vulpea-ui-test-create-sidebar-window-when-side-disabled ()
  "Sidebar window is created even when its side is disabled.
With `window-sides-slots' forbidding side windows on the configured
side, `vulpea-ui--create-sidebar-window' must still produce a real
side window rather than returning nil (see vulpea-journal#21)."
  (let ((window-sides-slots '(1 0 0 1))   ; right side disabled
        (vulpea-ui-sidebar-position 'right))
    (save-window-excursion
      (let* ((buf (get-buffer-create " *vulpea-ui-test-sidebar*"))
             (win (vulpea-ui--create-sidebar-window buf)))
        (unwind-protect
            (progn
              (should (window-live-p win))
              (should (eq (window-parameter win 'window-side) 'right)))
          (when (window-live-p win) (ignore-errors (delete-window win)))
          (when (buffer-live-p buf) (kill-buffer buf)))))))


;;; Sidebar window teardown tests

(ert-deftest vulpea-ui-test-hide-sidebar-window-keeps-non-side-window ()
  "Hiding leaves a non-side window alone instead of deleting it.
When the sidebar buffer is displayed in a regular (here sole) window,
`vulpea-ui--hide-sidebar-window' must be a no-op rather than signalling
\"Attempt to delete ... sole ordinary window\" (see vulpea-journal#21)."
  (save-window-excursion
    (let ((buf (get-buffer-create (vulpea-ui--sidebar-buffer-name))))
      (unwind-protect
          (progn
            (switch-to-buffer buf)
            (let ((win (selected-window)))
              ;; Precondition: sidebar buffer shown in a non-side window.
              (should (eq (vulpea-ui--get-sidebar-window) win))
              (should-not (window-parameter win 'window-side))
              ;; Hiding must neither error nor delete the window.
              (vulpea-ui--hide-sidebar-window)
              (should (window-live-p win))
              (should (eq (window-buffer win) buf))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest vulpea-ui-test-hide-sidebar-window-deletes-side-window ()
  "Hiding deletes an actual side window."
  (save-window-excursion
    (let* ((buf (get-buffer-create (vulpea-ui--sidebar-buffer-name)))
           (win (display-buffer-in-side-window
                 buf '((side . right) (slot . 0)))))
      (unwind-protect
          (progn
            (should (window-live-p win))
            (should (eq (window-parameter win 'window-side) 'right))
            (vulpea-ui--hide-sidebar-window)
            (should-not (window-live-p win)))
        (when (window-live-p win) (ignore-errors (delete-window win)))
        (when (buffer-live-p buf) (kill-buffer buf))))))


;;; Main window detection tests

(ert-deftest vulpea-ui-test-get-main-window-skips-other-side-windows ()
  "`vulpea-ui--get-main-window' never returns a non-sidebar side window.
A *Help*-style side window (here on the left, as produced by a
`display-buffer-alist' entry) must not be treated as the main window even
when it is selected.  Otherwise focusing it makes
`vulpea-ui--on-buffer-change' believe the user left the vulpea note and
triggers an auto-hide/show thrash that, under `window-combination-resize',
shrinks the note window on every switch (see
https://github.com/d12frosted/vulpea-ui/issues/36)."
  (save-window-excursion
    (let* ((main-buf (get-buffer-create " *vulpea-ui-test-main*"))
           (side-buf (get-buffer-create " *vulpea-ui-test-side*"))
           main-win side-win)
      (unwind-protect
          (progn
            ;; A plain main window holding the note buffer.
            (switch-to-buffer main-buf)
            (setq main-win (selected-window))
            (should-not (window-parameter main-win 'window-side))
            ;; A left side window, like *Help* via `display-buffer-alist'.
            (setq side-win (display-buffer-in-side-window
                            side-buf '((side . left) (slot . 0))))
            (should (eq (window-parameter side-win 'window-side) 'left))
            ;; Even with the side window selected, the main window wins.
            (select-window side-win)
            (should (eq (vulpea-ui--get-main-window) main-win)))
        (when (window-live-p side-win) (ignore-errors (delete-window side-win)))
        (when (buffer-live-p main-buf) (kill-buffer main-buf))
        (when (buffer-live-p side-buf) (kill-buffer side-buf))))))


;;; Sidebar render tests

(ert-deftest vulpea-ui-test-render-sidebar-without-window-is-noop ()
  "Rendering without a live sidebar window leaves the selected window alone.
`vui-mount' calls `switch-to-buffer', so rendering with no side window
must not run and take over an unrelated window (see vulpea-journal#21)."
  (save-window-excursion
    (let ((main-buf (get-buffer-create " *vulpea-ui-test-main*")))
      (unwind-protect
          (progn
            (switch-to-buffer main-buf)
            ;; Precondition: no sidebar window exists for this frame.
            (should-not (vulpea-ui--get-sidebar-window))
            ;; Rendering must neither hijack the window nor create the buffer.
            (vulpea-ui--render-sidebar nil)
            (should (eq (window-buffer (selected-window)) main-buf))
            (should-not (vulpea-ui--get-sidebar-buffer)))
        (when (buffer-live-p main-buf) (kill-buffer main-buf))
        (let ((sb (vulpea-ui--get-sidebar-buffer)))
          (when (and sb (buffer-live-p sb)) (kill-buffer sb)))))))


;;; Number formatting tests

(ert-deftest vulpea-ui-test-format-number-small ()
  "Test number formatting for small numbers."
  (should (equal (vulpea-ui--format-number 0) "0"))
  (should (equal (vulpea-ui--format-number 1) "1"))
  (should (equal (vulpea-ui--format-number 999) "999")))

(ert-deftest vulpea-ui-test-format-number-thousands ()
  "Test number formatting for thousands."
  (should (equal (vulpea-ui--format-number 1000) "1,000"))
  (should (equal (vulpea-ui--format-number 1234) "1,234"))
  (should (equal (vulpea-ui--format-number 12345) "12,345"))
  (should (equal (vulpea-ui--format-number 123456) "123,456")))

(ert-deftest vulpea-ui-test-format-number-millions ()
  "Test number formatting for millions."
  (should (equal (vulpea-ui--format-number 1000000) "1,000,000"))
  (should (equal (vulpea-ui--format-number 1234567) "1,234,567")))


;;; Stats computation tests

(ert-deftest vulpea-ui-test-compute-stats-nil ()
  "Test stats computation with nil note."
  (let ((stats (vulpea-ui--compute-stats nil)))
    (should (= (plist-get stats :chars) 0))
    (should (= (plist-get stats :words) 0))
    (should (= (plist-get stats :links) 0))))

(ert-deftest vulpea-ui-test-compute-stats-with-file ()
  "Test stats computation with actual file."
  (let* ((temp-file (make-temp-file "vulpea-ui-test" nil ".org"))
         (note (make-vulpea-note
                :id "test-stats"
                :path temp-file
                :level 0
                :pos 1
                :title "Test"
                :primary-title "Test"
                :aliases nil
                :tags nil
                :links nil
                :properties nil
                :meta nil)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "Hello world test content"))
          (let ((stats (vulpea-ui--compute-stats note)))
            (should (> (plist-get stats :chars) 0))
            (should (> (plist-get stats :words) 0))
            (should (= (plist-get stats :links) 0))))
      (delete-file temp-file))))


;;; Note preview tests

(ert-deftest vulpea-ui-test-get-preview-nil ()
  "Test preview generation with nil note."
  (should (null (vulpea-ui--get-note-preview nil 10 t t))))

(ert-deftest vulpea-ui-test-get-preview-with-file ()
  "Test preview generation with actual file."
  (let* ((temp-file (make-temp-file "vulpea-ui-test" nil ".org"))
         (note (make-vulpea-note
                :id "test-preview"
                :path temp-file
                :level 0
                :pos 1
                :title "Test"
                :primary-title "Test"
                :aliases nil
                :tags nil
                :links nil
                :properties nil
                :meta nil)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "First line\nSecond line\nThird line"))
          (let ((preview (vulpea-ui--get-note-preview note 10 t t)))
            (should (stringp preview))
            (should (string-match-p "First line" preview))))
      (delete-file temp-file))))


;;; Should update predicate tests

(ert-deftest vulpea-ui-test-should-update-nil ()
  "Test should-update with nil note."
  (let ((vulpea-ui--current-note nil))
    (should-not (vulpea-ui--should-update-p nil))))

(ert-deftest vulpea-ui-test-should-update-new-note ()
  "Test should-update with new note."
  (let ((vulpea-ui--current-note nil)
        (note (vulpea-ui-test--make-mock-note "new-id")))
    (should (vulpea-ui--should-update-p note))))

(ert-deftest vulpea-ui-test-should-update-same-note ()
  "Test should-update with same note."
  (let* ((note (vulpea-ui-test--make-mock-note "same-id"))
         (vulpea-ui--current-note note))
    (should-not (vulpea-ui--should-update-p note))))

(ert-deftest vulpea-ui-test-should-update-different-note ()
  "Test should-update with different note."
  (let* ((note1 (vulpea-ui-test--make-mock-note "id-1"))
         (note2 (vulpea-ui-test--make-mock-note "id-2"))
         (vulpea-ui--current-note note1))
    (should (vulpea-ui--should-update-p note2))))


;;; Outgoing mentions tests

(ert-deftest vulpea-ui-test-group-outgoing-mentions-empty ()
  "Grouping no outgoing mentions yields nil."
  (should-not (vulpea-ui--group-outgoing-mentions nil)))

(ert-deftest vulpea-ui-test-group-outgoing-mentions-single ()
  "A single outgoing mention yields one group with one context line."
  (let* ((note (vulpea-ui-test--make-mock-note "n1" "Note One"))
         (groups (vulpea-ui--group-outgoing-mentions
                  (list (list :note note :line 12 :context "see Note One"
                              :matched "Note One")))))
    (should (= (length groups) 1))
    (let ((g (car groups)))
      (should (eq (plist-get g :note) note))
      (should (equal (plist-get g :mentions)
                     (list (list :line 12 :context "see Note One")))))))

(ert-deftest vulpea-ui-test-group-outgoing-mentions-same-note ()
  "Multiple mentions of the same note are grouped, order preserved."
  (let* ((note (vulpea-ui-test--make-mock-note "n1" "Note One"))
         (groups (vulpea-ui--group-outgoing-mentions
                  (list (list :note note :line 3 :context "first")
                        (list :note note :line 9 :context "second")))))
    (should (= (length groups) 1))
    (should (equal (plist-get (car groups) :mentions)
                   (list (list :line 3 :context "first")
                         (list :line 9 :context "second"))))))

(ert-deftest vulpea-ui-test-group-outgoing-mentions-first-encounter-order ()
  "Groups follow the first-encounter order of candidate notes."
  (let* ((a (vulpea-ui-test--make-mock-note "a" "Alpha"))
         (b (vulpea-ui-test--make-mock-note "b" "Beta"))
         (groups (vulpea-ui--group-outgoing-mentions
                  (list (list :note b :line 1 :context "b1")
                        (list :note a :line 2 :context "a1")
                        (list :note b :line 3 :context "b2")))))
    (should (equal (mapcar (lambda (g) (vulpea-note-id (plist-get g :note)))
                           groups)
                   '("b" "a")))
    (should (equal (plist-get (nth 0 groups) :mentions)
                   (list (list :line 1 :context "b1")
                         (list :line 3 :context "b2"))))))

(ert-deftest vulpea-ui-test-group-outgoing-mentions-skips-noteless ()
  "Mentions without a candidate note are skipped."
  (let* ((note (vulpea-ui-test--make-mock-note "n1" "Note One"))
         (groups (vulpea-ui--group-outgoing-mentions
                  (list (list :note nil :line 1 :context "orphan")
                        (list :note note :line 2 :context "kept")))))
    (should (= (length groups) 1))
    (should (eq (plist-get (car groups) :note) note))))

(ert-deftest vulpea-ui-test-group-outgoing-mentions-dedups-same-line ()
  "Two terms matching one note on the same line collapse to one context line.
Upstream emits one entry per matched term, so a title and an alias both
hitting the same line yield identical :line/:context pairs differing only
in :matched - they must not render twice."
  (let* ((note (vulpea-ui-test--make-mock-note "n1" "Note One"))
         (groups (vulpea-ui--group-outgoing-mentions
                  (list (list :note note :line 6 :context "Note One aka NO"
                              :matched "Note One")
                        (list :note note :line 6 :context "Note One aka NO"
                              :matched "NO")))))
    (should (= (length groups) 1))
    (should (equal (plist-get (car groups) :mentions)
                   (list (list :line 6 :context "Note One aka NO"))))))

(ert-deftest vulpea-ui-test-group-outgoing-mentions-keeps-distinct-lines ()
  "Distinct lines for the same note are kept, in original order."
  (let* ((note (vulpea-ui-test--make-mock-note "n1" "Note One"))
         (groups (vulpea-ui--group-outgoing-mentions
                  (list (list :note note :line 3 :context "first")
                        (list :note note :line 3 :context "first")
                        (list :note note :line 8 :context "second")))))
    (should (= (length groups) 1))
    (should (equal (plist-get (car groups) :mentions)
                   (list (list :line 3 :context "first")
                         (list :line 8 :context "second"))))))


;;; Mention filter tests

(ert-deftest vulpea-ui-test-filter-mentions-identity-keeps-all ()
  "The default identity filter keeps every mention with a note."
  (let* ((a (vulpea-ui-test--make-mock-note "a" "Alpha"))
         (b (vulpea-ui-test--make-mock-note "b" "Beta"))
         (mentions (list (list :note a :line 1 :context "x")
                         (list :note b :line 2 :context "y"))))
    (should (equal (vulpea-ui--filter-mentions mentions #'identity) mentions))))

(ert-deftest vulpea-ui-test-filter-mentions-predicate ()
  "A predicate drops mentions whose note it rejects."
  (let* ((keep (vulpea-ui-test--make-mock-note "k" "Keep"))
         (skip (vulpea-ui-test--make-mock-note "s" "Skip"))
         (mentions (list (list :note keep :line 1 :context "x")
                         (list :note skip :line 2 :context "y")))
         (filtered (vulpea-ui--filter-mentions
                    mentions
                    (lambda (n) (not (equal (vulpea-note-title n) "Skip"))))))
    (should (= (length filtered) 1))
    (should (eq (plist-get (car filtered) :note) keep))))

(ert-deftest vulpea-ui-test-filter-mentions-drops-noteless ()
  "Mentions without a note are dropped even under identity."
  (let* ((a (vulpea-ui-test--make-mock-note "a" "Alpha"))
         (mentions (list (list :note nil :line 1 :context "x")
                         (list :note a :line 2 :context "y"))))
    (should (equal (vulpea-ui--filter-mentions mentions #'identity)
                   (list (list :note a :line 2 :context "y"))))))


;;; Mention linking tests

(ert-deftest vulpea-ui-test-note-link-terms ()
  "Link terms are the title plus aliases, dropping empty strings."
  (let ((note (make-vulpea-note :id "n" :title "Title"
                                :aliases '("A1" "" "A2"))))
    (should (equal (vulpea-ui--note-link-terms note) '("Title" "A1" "A2")))))

(ert-deftest vulpea-ui-test-link-mention-line-single ()
  "A bare occurrence becomes an id: link; returns 1."
  (let ((note (make-vulpea-note :id "n1" :title "Note One")))
    (with-temp-buffer
      (insert "We had Note One today.\n")
      (should (= (vulpea-ui--link-mention-line (current-buffer) 1 note) 1))
      (should (equal (buffer-string)
                     "We had [[id:n1][Note One]] today.\n")))))

(ert-deftest vulpea-ui-test-link-mention-line-skips-linked ()
  "An occurrence already inside a link is left alone; returns 0."
  (let ((note (make-vulpea-note :id "n1" :title "Note One")))
    (with-temp-buffer
      (insert "See [[id:n1][Note One]] here.\n")
      (should (= (vulpea-ui--link-mention-line (current-buffer) 1 note) 0))
      (should (equal (buffer-string)
                     "See [[id:n1][Note One]] here.\n")))))

(ert-deftest vulpea-ui-test-link-mention-line-multiple ()
  "Two bare occurrences on the line are both linked; returns 2."
  (let ((note (make-vulpea-note :id "n1" :title "Cab")))
    (with-temp-buffer
      (insert "Cab and more Cab.\n")
      (should (= (vulpea-ui--link-mention-line (current-buffer) 1 note) 2))
      (should (equal (buffer-string)
                     "[[id:n1][Cab]] and more [[id:n1][Cab]].\n")))))

(ert-deftest vulpea-ui-test-link-mention-line-mixed ()
  "A bare occurrence is linked while an already-linked one is preserved."
  (let ((note (make-vulpea-note :id "n1" :title "Cab")))
    (with-temp-buffer
      (insert "[[id:n1][Cab]] and bare Cab.\n")
      (should (= (vulpea-ui--link-mention-line (current-buffer) 1 note) 1))
      (should (equal (buffer-string)
                     "[[id:n1][Cab]] and bare [[id:n1][Cab]].\n")))))

(ert-deftest vulpea-ui-test-link-mention-line-alias-and-case ()
  "Matching is case-insensitive and works on aliases; casing is preserved."
  (let ((note (make-vulpea-note :id "n1" :title "Cabernet"
                                :aliases '("Cab Sauv"))))
    (with-temp-buffer
      (insert "love cab sauv tonight\n")
      (should (= (vulpea-ui--link-mention-line (current-buffer) 1 note) 1))
      (should (equal (buffer-string)
                     "love [[id:n1][cab sauv]] tonight\n")))))

(ert-deftest vulpea-ui-test-link-mention-line-word-boundary ()
  "A term does not match inside a larger word."
  (let ((note (make-vulpea-note :id "n1" :title "NO")))
    (with-temp-buffer
      (insert "Head NORTH now.\n")
      (should (= (vulpea-ui--link-mention-line (current-buffer) 1 note) 0))
      (should (equal (buffer-string) "Head NORTH now.\n")))))

(ert-deftest vulpea-ui-test-link-mention-line-stale ()
  "When the term is gone (stale mention), nothing is linked; returns 0."
  (let ((note (make-vulpea-note :id "n1" :title "Gone")))
    (with-temp-buffer
      (insert "nothing here\n")
      (should (= (vulpea-ui--link-mention-line (current-buffer) 1 note) 0))
      (should (equal (buffer-string) "nothing here\n")))))


;;; Mention link action tests

(vui-defcomponent vulpea-ui-test--outgoing-mention-wrap (mention note path)
  "Test wrapper rendering a single outgoing MENTION line."
  :render (vulpea-ui--render-outgoing-mention mention note path))

(vui-defcomponent vulpea-ui-test--outgoing-group-wrap (group path)
  "Test wrapper rendering an outgoing GROUP."
  :render (vulpea-ui--render-outgoing-group group path))

(ert-deftest vulpea-ui-test-render-outgoing-mention-link-before-context ()
  "The link button renders before the context text, not after it."
  (let ((note (make-vulpea-note :id "n1" :title "Cab"))
        (mention (list :line 2 :context "had Cab today")))
    (with-temp-buffer
      (vui-mount (vui-component 'vulpea-ui-test--outgoing-mention-wrap
                   :mention mention :note note :path "/tmp/x.org")
                 (buffer-name))
      (let ((s (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "link" s))
        (should (string-match-p "had Cab today" s))
        (should (< (string-match "link" s)
                   (string-match "had Cab today" s)))))))

(ert-deftest vulpea-ui-test-link-action-does-not-refresh ()
  "Linking a single mention does not trigger a sidebar refresh.
A refresh would re-scan and reset point to the top of the sidebar."
  (let* ((dir (make-temp-file "vui-nr-" t))
         (f (expand-file-name "note.org" dir))
         (note (make-vulpea-note :id "tid" :title "Target"))
         (refreshed nil))
    (unwind-protect
        (progn
          (with-temp-file f (insert "Mention Target here.\n"))
          (let ((note-buf (find-file-noselect f)))
            (unwind-protect
                (cl-letf (((symbol-function 'vulpea-ui-sidebar-refresh)
                           (lambda () (setq refreshed t))))
                  (vulpea-ui--link-mention-action f 1 note)
                  (should-not refreshed)
                  (with-current-buffer note-buf
                    (should (string-match-p
                             (regexp-quote "[[id:tid][Target]]")
                             (buffer-string)))))
              (kill-buffer note-buf))))
      (delete-directory dir t))))

(ert-deftest vulpea-ui-test-link-group-action-does-not-refresh ()
  "Linking a whole group does not trigger a sidebar refresh."
  (let* ((dir (make-temp-file "vui-nrg-" t))
         (f (expand-file-name "note.org" dir))
         (note (make-vulpea-note :id "tid" :title "Target"))
         (refreshed nil))
    (unwind-protect
        (progn
          (with-temp-file f (insert "Target a\nTarget b\n"))
          (let ((note-buf (find-file-noselect f)))
            (unwind-protect
                (cl-letf (((symbol-function 'vulpea-ui-sidebar-refresh)
                           (lambda () (setq refreshed t))))
                  (vulpea-ui--link-group-action
                   f note (list (list :line 1 :context "Target a")
                                (list :line 2 :context "Target b")))
                  (should-not refreshed)
                  (with-current-buffer note-buf
                    (let ((s (buffer-string)))
                      ;; The title "Target" links word-bounded, so " a"/" b"
                      ;; remain after each inserted link.
                      (should (string-match-p
                               (regexp-quote "[[id:tid][Target]] a") s))
                      (should (string-match-p
                               (regexp-quote "[[id:tid][Target]] b") s)))))
              (kill-buffer note-buf))))
      (delete-directory dir t))))

(ert-deftest vulpea-ui-test-link-action-keeps-sidebar-point ()
  "Linking from the sidebar leaves point where it was, not at the top."
  (let* ((dir (make-temp-file "vui-pt-" t))
         (f (expand-file-name "note.org" dir))
         (note (make-vulpea-note :id "tid" :title "Target"))
         (group (list :note note
                      :mentions (list (list :line 1 :context "Target one")
                                      (list :line 2 :context "Target two")))))
    (unwind-protect
        (progn
          (with-temp-file f (insert "Target one\nTarget two\n"))
          (let ((note-buf (find-file-noselect f)))
            (unwind-protect
                (with-temp-buffer
                  (vui-mount (vui-component 'vulpea-ui-test--outgoing-group-wrap
                               :group group :path f)
                             (buffer-name))
                  ;; Park point on the second mention's link button: skip the
                  ;; group's "link all" and the first mention's "link".
                  (goto-char (point-min))
                  (should (search-forward "link" nil t))
                  (should (search-forward "link" nil t))
                  (should (search-forward "link" nil t))
                  (let ((parked (point)))
                    (vulpea-ui--link-mention-action f 1 note)
                    (should (= (point) parked))
                    (should (/= (point) (point-min)))))
              (kill-buffer note-buf))))
      (delete-directory dir t))))


;;; Async display-state tests

(ert-deftest vulpea-ui-test-mentions-display-ready-updates-ref ()
  "A ready result shows the fresh data and caches it under its note id."
  (let ((ref (list nil)))
    (should (equal (vulpea-ui--mentions-display-data 'ready "n1" '(a b) ref)
                   '(shown a b)))
    (should (equal (car ref) '("n1" a b)))))

(ert-deftest vulpea-ui-test-mentions-display-pending-keeps-list ()
  "While re-scanning the same note, the previously loaded list stays shown.
This is what keeps point from jumping to the top on refresh."
  (let ((ref (list nil)))
    (vulpea-ui--mentions-display-data 'ready "n1" '(m1 m2) ref)
    (should (equal (vulpea-ui--mentions-display-data 'pending "n1" nil ref)
                   '(shown m1 m2)))))

(ert-deftest vulpea-ui-test-mentions-display-pending-other-note ()
  "Cached data is never reused for a different note."
  (let ((ref (list nil)))
    (vulpea-ui--mentions-display-data 'ready "n1" '(m1) ref)
    (should (equal (vulpea-ui--mentions-display-data 'pending "n2" nil ref)
                   '(loading)))))

(ert-deftest vulpea-ui-test-mentions-display-pending-first-load ()
  "With nothing cached, a pending result is the loading state."
  (let ((ref (list nil)))
    (should (equal (vulpea-ui--mentions-display-data 'pending "n1" nil ref)
                   '(loading)))))

(ert-deftest vulpea-ui-test-mentions-display-error ()
  "An error result is the error state regardless of any cache."
  (let ((ref (list '("n1" m1))))
    (should (equal (vulpea-ui--mentions-display-data 'error "n1" nil ref)
                   '(error)))))


;;; Mode tests

(ert-deftest vulpea-ui-test-mode-keymap ()
  "Test that sidebar mode has expected keybindings."
  (should (eq (lookup-key vulpea-ui-sidebar-mode-map (kbd "q"))
              'vulpea-ui-sidebar-close))
  (should (eq (lookup-key vulpea-ui-sidebar-mode-map (kbd "g"))
              'vulpea-ui-sidebar-refresh)))


;;; Integration tests (require display to be available)

(ert-deftest vulpea-ui-test-sidebar-open-close ()
  "Test opening and closing sidebar."
  :tags '(:integration)
  (skip-unless (vulpea-ui-test--can-create-frames-p))
  (vulpea-ui-test--with-temp-frame
    ;; Open sidebar
    (vulpea-ui-sidebar-open)
    (should (vulpea-ui--sidebar-visible-p))
    (should (vulpea-ui--get-sidebar-buffer))
    (should (vulpea-ui--get-sidebar-window))
    ;; Close sidebar
    (vulpea-ui-sidebar-close)
    (should-not (vulpea-ui--sidebar-visible-p))
    (should-not (vulpea-ui--get-sidebar-buffer))))

(ert-deftest vulpea-ui-test-sidebar-toggle ()
  "Test toggling sidebar."
  :tags '(:integration)
  (skip-unless (vulpea-ui-test--can-create-frames-p))
  (vulpea-ui-test--with-temp-frame
    ;; Initially hidden
    (should-not (vulpea-ui--sidebar-visible-p))
    ;; Toggle on
    (vulpea-ui-sidebar-toggle)
    (should (vulpea-ui--sidebar-visible-p))
    ;; Toggle off
    (vulpea-ui-sidebar-toggle)
    (should-not (vulpea-ui--sidebar-visible-p))))


;;; Org link cleaning tests

(ert-deftest vulpea-ui-test-clean-org-links-nil ()
  "Test cleaning nil text."
  (should (null (vulpea-ui--clean-org-links nil))))

(ert-deftest vulpea-ui-test-clean-org-links-no-links ()
  "Test cleaning text without links."
  (should (equal (vulpea-ui--clean-org-links "plain text") "plain text")))

(ert-deftest vulpea-ui-test-clean-org-links-with-description ()
  "Test cleaning link with description."
  (should (equal (vulpea-ui--clean-org-links "before [[id:123][Description]] after")
                 "before Description after")))

(ert-deftest vulpea-ui-test-clean-org-links-bare-link ()
  "Test cleaning bare link without description."
  (should (equal (vulpea-ui--clean-org-links "before [[id:123]] after")
                 "before after")))

(ert-deftest vulpea-ui-test-clean-org-links-multiple ()
  "Test cleaning multiple links."
  (should (equal (vulpea-ui--clean-org-links "[[id:1][One]] and [[id:2][Two]]")
                 "One and Two")))


;;; Org markup cleaning tests (public API)

(ert-deftest vulpea-ui-test-clean-org-markup-nil ()
  "Test cleaning nil text."
  (should (null (vulpea-ui-clean-org-markup nil))))

(ert-deftest vulpea-ui-test-clean-org-markup-plain-text ()
  "Test cleaning text without markup."
  (should (equal (vulpea-ui-clean-org-markup "plain text") "plain text")))

(ert-deftest vulpea-ui-test-clean-org-markup-link-with-description ()
  "Test cleaning link with description."
  (should (equal (vulpea-ui-clean-org-markup "see [[https://example.com][Example]]")
                 "see Example")))

(ert-deftest vulpea-ui-test-clean-org-markup-bare-url ()
  "Test cleaning bare URL link keeps the URL."
  (should (equal (vulpea-ui-clean-org-markup "visit [[https://example.com]]")
                 "visit https://example.com")))

(ert-deftest vulpea-ui-test-clean-org-markup-bare-id-link ()
  "Test cleaning bare id link removes it."
  (should (equal (vulpea-ui-clean-org-markup "see [[id:abc123]] here")
                 "see here")))

(ert-deftest vulpea-ui-test-clean-org-markup-drawer ()
  "Test cleaning property drawer."
  (should (equal (vulpea-ui-clean-org-markup
                  "before\n:PROPERTIES:\n:ID: abc\n:END:\nafter")
                 "before\nafter")))

(ert-deftest vulpea-ui-test-clean-org-markup-metadata ()
  "Test cleaning metadata lines."
  (should (equal (vulpea-ui-clean-org-markup
                  "#+TITLE: My Note\n#+FILETAGS: :tag1:tag2:\nContent here")
                 "Content here")))

(ert-deftest vulpea-ui-test-clean-org-markup-whitespace ()
  "Test cleaning multiple spaces."
  (should (equal (vulpea-ui-clean-org-markup "hello    world")
                 "hello world")))

(ert-deftest vulpea-ui-test-clean-org-markup-combined ()
  "Test cleaning combined markup."
  (should (equal (vulpea-ui-clean-org-markup
                  "#+TITLE: Test\n:PROPERTIES:\n:ID: x\n:END:\nSee [[id:y][Note]] for  details")
                 "See Note for details")))


;;; Context type detection tests

(ert-deftest vulpea-ui-test-detect-context-meta ()
  "Test detecting meta context."
  (should (eq (vulpea-ui--detect-context-type 1 "- key :: value")
              'meta)))

(ert-deftest vulpea-ui-test-detect-context-header ()
  "Test detecting header context."
  (should (eq (vulpea-ui--detect-context-type 1 "* Heading")
              'header))
  (should (eq (vulpea-ui--detect-context-type 1 "** Subheading")
              'header)))

(ert-deftest vulpea-ui-test-detect-context-table ()
  "Test detecting table context."
  (should (eq (vulpea-ui--detect-context-type 1 "| cell1 | cell2 |")
              'table)))

(ert-deftest vulpea-ui-test-detect-context-list ()
  "Test detecting list context."
  (should (eq (vulpea-ui--detect-context-type 1 "- item")
              'list))
  (should (eq (vulpea-ui--detect-context-type 1 "1. numbered")
              'list)))

(ert-deftest vulpea-ui-test-detect-context-prose ()
  "Test detecting prose context."
  (should (eq (vulpea-ui--detect-context-type 1 "Just regular text")
              'prose)))


;;; Backlinks sorting tests

(ert-deftest vulpea-ui-test-sort-backlinks-nil ()
  "Test sorting with nil configuration."
  (let ((vulpea-ui-backlinks-sort nil)
        (groups (list (list :file-note (vulpea-ui-test--make-mock-note "1" "Zebra"))
                      (list :file-note (vulpea-ui-test--make-mock-note "2" "Apple")))))
    (let ((sorted (vulpea-ui--sort-backlink-groups groups)))
      ;; Should remain in original order
      (should (equal (vulpea-note-title (plist-get (nth 0 sorted) :file-note)) "Zebra"))
      (should (equal (vulpea-note-title (plist-get (nth 1 sorted) :file-note)) "Apple")))))

(ert-deftest vulpea-ui-test-sort-backlinks-title-asc ()
  "Test sorting by title ascending."
  (let ((vulpea-ui-backlinks-sort 'title-asc)
        (groups (list (list :file-note (vulpea-ui-test--make-mock-note "1" "Zebra"))
                      (list :file-note (vulpea-ui-test--make-mock-note "2" "Apple")))))
    (let ((sorted (vulpea-ui--sort-backlink-groups groups)))
      (should (equal (vulpea-note-title (plist-get (nth 0 sorted) :file-note)) "Apple"))
      (should (equal (vulpea-note-title (plist-get (nth 1 sorted) :file-note)) "Zebra")))))

(ert-deftest vulpea-ui-test-sort-backlinks-title-desc ()
  "Test sorting by title descending."
  (let ((vulpea-ui-backlinks-sort 'title-desc)
        (groups (list (list :file-note (vulpea-ui-test--make-mock-note "1" "Apple"))
                      (list :file-note (vulpea-ui-test--make-mock-note "2" "Zebra")))))
    (let ((sorted (vulpea-ui--sort-backlink-groups groups)))
      (should (equal (vulpea-note-title (plist-get (nth 0 sorted) :file-note)) "Zebra"))
      (should (equal (vulpea-note-title (plist-get (nth 1 sorted) :file-note)) "Apple")))))


;;; Fast parse configuration tests

(ert-deftest vulpea-ui-test-fast-parse-default ()
  "Test that fast-parse is disabled by default."
  (should-not vulpea-ui-fast-parse))


;;; Parse headings tests

(ert-deftest vulpea-ui-test-parse-headings-with-attachment-link ()
  "Test that parsing headings works with attachment links.
Regression test for issue #21: inline image previews in temp
buffers crash because `org-attach-id-dir' is relative and the
buffer has no filename."
  (let* ((temp-file (make-temp-file "vulpea-ui-test" nil ".org"))
         (note (make-vulpea-note
                :id "test-attach"
                :path temp-file
                :level 0
                :pos 1
                :title "Test with attachment"
                :primary-title "Test with attachment"
                :aliases nil
                :tags nil
                :links nil
                :properties nil
                :meta nil)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "#+startup: inlineimages\n"
                    "* Heading One\n"
                    "Some text with [[attachment:image.png]]\n"
                    "* Heading Two\n"
                    "More content\n"))
          (let ((headings (vulpea-ui--parse-headings note)))
            (should (= (length headings) 2))
            (should (equal (plist-get (nth 0 headings) :title) "Heading One"))
            (should (equal (plist-get (nth 1 headings) :title) "Heading Two"))))
      (delete-file temp-file))))

(ert-deftest vulpea-ui-test-parse-headings-cleans-org-markup ()
  "Test that heading titles are stripped of org markup.
Regression test for issue #20: outline headings were shown with
raw org markup such as links and emphasis."
  (let* ((temp-file (make-temp-file "vulpea-ui-test" nil ".org"))
         (note (make-vulpea-note
                :id "test-markup"
                :path temp-file
                :level 0
                :pos 1
                :title "Test with markup"
                :primary-title "Test with markup"
                :aliases nil
                :tags nil
                :links nil
                :properties nil
                :meta nil)))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert "* See [[id:abc123][Other Note]]\n"
                    "Some content\n"
                    "* Visit [[https://example.com][Example]] now\n"
                    "More content\n"))
          (let ((headings (vulpea-ui--parse-headings note)))
            (should (= (length headings) 2))
            (should (equal (plist-get (nth 0 headings) :title) "See Other Note"))
            (should (equal (plist-get (nth 1 headings) :title) "Visit Example now"))))
      (delete-file temp-file))))


;;; Widget registry tests

(ert-deftest vulpea-ui-test-register-widget-stores-props ()
  "Registering a widget stores component, predicate and order."
  (vulpea-ui-test--with-clean-registry
    (vulpea-ui-register-widget 'w
                               :component 'w-component
                               :predicate #'identity
                               :order 42)
    (let ((props (gethash 'w vulpea-ui--widget-registry)))
      (should (eq (plist-get props :component) 'w-component))
      (should (eq (plist-get props :predicate) #'identity))
      (should (= (plist-get props :order) 42)))))

(ert-deftest vulpea-ui-test-register-widget-overwrites ()
  "Registering a widget twice overwrites the previous entry."
  (vulpea-ui-test--with-clean-registry
    (vulpea-ui-register-widget 'w :component 'first :order 100)
    (vulpea-ui-register-widget 'w :component 'second :order 200)
    (let ((props (gethash 'w vulpea-ui--widget-registry)))
      (should (eq (plist-get props :component) 'second))
      (should (= (plist-get props :order) 200)))))

(ert-deftest vulpea-ui-test-unregister-widget ()
  "Unregistering removes the widget from the registry."
  (vulpea-ui-test--with-clean-registry
    (vulpea-ui-register-widget 'w :component 'w-component)
    (should (gethash 'w vulpea-ui--widget-registry))
    (vulpea-ui-unregister-widget 'w)
    (should-not (gethash 'w vulpea-ui--widget-registry))))

(ert-deftest vulpea-ui-test-widget-set-updates-prop ()
  "`vulpea-ui-widget-set' installs a property on an existing widget."
  (vulpea-ui-test--with-clean-registry
    (vulpea-ui-register-widget 'w :component 'w-component :order 100)
    (vulpea-ui-widget-set 'w :order 500)
    (should (= (plist-get (gethash 'w vulpea-ui--widget-registry) :order) 500))))

(ert-deftest vulpea-ui-test-widget-set-missing-widget ()
  "`vulpea-ui-widget-set' is a no-op when the widget is unknown."
  (vulpea-ui-test--with-clean-registry
    (vulpea-ui-widget-set 'missing :order 1)
    (should-not (gethash 'missing vulpea-ui--widget-registry))))

(ert-deftest vulpea-ui-test-widgets-for-note-no-predicate ()
  "A widget without a predicate is always shown."
  (vulpea-ui-test--with-clean-registry
    (vulpea-ui-register-widget 'w :component 'w-component)
    (should (memq 'w-component
                  (vulpea-ui--get-widgets-for-note
                   (vulpea-ui-test--make-mock-note))))))

(ert-deftest vulpea-ui-test-widgets-for-note-predicate-filters ()
  "A widget with a predicate is shown only when the predicate passes."
  (vulpea-ui-test--with-clean-registry
    (vulpea-ui-register-widget 'w
                               :component 'w-component
                               :predicate (lambda (_note) nil))
    (should-not (memq 'w-component
                      (vulpea-ui--get-widgets-for-note
                       (vulpea-ui-test--make-mock-note))))
    (vulpea-ui-widget-set 'w :predicate (lambda (_note) t))
    (should (memq 'w-component
                  (vulpea-ui--get-widgets-for-note
                   (vulpea-ui-test--make-mock-note))))))

(ert-deftest vulpea-ui-test-widgets-for-note-ordering ()
  "Widgets are returned in ascending order of `:order'."
  (vulpea-ui-test--with-clean-registry
    (vulpea-ui-register-widget 'a :component 'a-component :order 300)
    (vulpea-ui-register-widget 'b :component 'b-component :order 100)
    (vulpea-ui-register-widget 'c :component 'c-component :order 200)
    (should (equal (vulpea-ui--get-widgets-for-note
                    (vulpea-ui-test--make-mock-note))
                   '(b-component c-component a-component)))))

(ert-deftest vulpea-ui-test-widget-predicate-toggle-recipe ()
  "Per-note toggle recipe: a property on the note overrides a default variable.
Mirrors the example from the README."
  (vulpea-ui-test--with-clean-registry
    (let ((default-on nil))
      (vulpea-ui-register-widget 'w :component 'w-component)
      (vulpea-ui-widget-set
       'w :predicate
       (lambda (note)
         (if-let* ((props (vulpea-note-properties note))
                   (entry (assoc "SHOW_W" props)))
             (not (equal (cdr entry) "nil"))
           default-on)))
      ;; no property, variable nil -> hidden
      (should-not (memq 'w-component
                        (vulpea-ui--get-widgets-for-note
                         (vulpea-ui-test--make-mock-note))))
      ;; no property, variable t -> shown
      (setq default-on t)
      (should (memq 'w-component
                    (vulpea-ui--get-widgets-for-note
                     (vulpea-ui-test--make-mock-note))))
      ;; property "nil" overrides variable t -> hidden
      (should-not (memq 'w-component
                        (vulpea-ui--get-widgets-for-note
                         (vulpea-ui-test--make-mock-note
                          nil nil '(("SHOW_W" . "nil"))))))
      ;; property "t" overrides variable nil -> shown
      (setq default-on nil)
      (should (memq 'w-component
                    (vulpea-ui--get-widgets-for-note
                     (vulpea-ui-test--make-mock-note
                      nil nil '(("SHOW_W" . "t")))))))))


;;; Unlinked mentions grouping tests

(ert-deftest vulpea-ui-test-group-mentions-empty ()
  "Grouping an empty mention list yields nil."
  (should (null (vulpea-ui--group-mentions nil))))

(ert-deftest vulpea-ui-test-group-mentions-by-note ()
  "Mentions are grouped by mentioning note, preserving order.
Groups appear in first-encounter order; mentions keep their order
within a group and carry :line and :context."
  (let* ((a (vulpea-ui-test--make-mock-note "id-a" "Note A"))
         (b (vulpea-ui-test--make-mock-note "id-b" "Note B"))
         (mentions (list
                    (list :note a :path "/a.org" :line 3 :context "first a")
                    (list :note b :path "/b.org" :line 7 :context "only b")
                    (list :note a :path "/a.org" :line 9 :context "second a")))
         (groups (vulpea-ui--group-mentions mentions)))
    ;; Two groups, in first-seen order: A then B
    (should (= (length groups) 2))
    (should (equal (vulpea-note-id (plist-get (nth 0 groups) :note)) "id-a"))
    (should (equal (plist-get (nth 0 groups) :path) "/a.org"))
    (should (equal (vulpea-note-id (plist-get (nth 1 groups) :note)) "id-b"))
    ;; A keeps both mentions in original order
    (let ((a-mentions (plist-get (nth 0 groups) :mentions)))
      (should (= (length a-mentions) 2))
      (should (= (plist-get (nth 0 a-mentions) :line) 3))
      (should (equal (plist-get (nth 0 a-mentions) :context) "first a"))
      (should (= (plist-get (nth 1 a-mentions) :line) 9))
      (should (equal (plist-get (nth 1 a-mentions) :context) "second a")))
    ;; B has its single mention
    (let ((b-mentions (plist-get (nth 1 groups) :mentions)))
      (should (= (length b-mentions) 1))
      (should (= (plist-get (nth 0 b-mentions) :line) 7))
      (should (equal (plist-get (nth 0 b-mentions) :context) "only b")))))

(ert-deftest vulpea-ui-test-group-mentions-single-group ()
  "Multiple mentions from one note collapse into a single group."
  (let* ((a (vulpea-ui-test--make-mock-note "id-a" "Note A"))
         (mentions (list
                    (list :note a :path "/a.org" :line 1 :context "one")
                    (list :note a :path "/a.org" :line 2 :context "two")))
         (groups (vulpea-ui--group-mentions mentions)))
    (should (= (length groups) 1))
    (should (= (length (plist-get (nth 0 groups) :mentions)) 2))))


;;; Unlinked mentions widget render tests

(defmacro vulpea-ui-test--render-mentions (mentions-form &rest body)
  "Render the unlinked-mentions widget and run BODY with OUTPUT bound.

`vulpea-note-unlinked-mentions-async' is stubbed to resolve
synchronously with MENTIONS-FORM, so the whole async pipeline runs
deterministically with no ripgrep and no event loop.  OUTPUT is the
rendered sidebar buffer text.  The widget registry is isolated to the
unlinked-mentions widget for the duration of the render."
  (declare (indent 1))
  `(vulpea-ui-test--with-clean-registry
     (vulpea-ui-register-widget 'unlinked-mentions
                                :component 'vulpea-ui-widget-unlinked-mentions
                                :order 350)
     (let ((note (vulpea-ui-test--make-mock-note "tgt" "Target"))
           (buf-name "*vulpea-ui-mentions-test*"))
       (with-current-buffer (get-buffer-create buf-name)
         (vulpea-ui-sidebar-mode))
       (cl-letf (((symbol-function 'vulpea-note-unlinked-mentions-async)
                  (lambda (_note resolve _reject)
                    (funcall resolve ,mentions-form)
                    nil)))
         (unwind-protect
             (progn
               (vui-mount (vui-component 'vulpea-ui-sidebar-root :note note)
                          buf-name)
               (let ((output (with-current-buffer buf-name
                               (buffer-substring-no-properties
                                (point-min) (point-max)))))
                 ,@body))
           (when (get-buffer buf-name)
             (kill-buffer buf-name)))))))

(ert-deftest vulpea-ui-test-mentions-widget-ready ()
  "Resolved mentions render grouped under each mentioning note with a count."
  (let ((a (vulpea-ui-test--make-mock-note "id-a" "Note A"))
        (b (vulpea-ui-test--make-mock-note "id-b" "Note B")))
    (vulpea-ui-test--render-mentions
        (list (list :note a :path "/a.org" :line 3 :context "mentions Target here")
              (list :note a :path "/a.org" :line 9 :context "Target again")
              (list :note b :path "/b.org" :line 5 :context "a Target reference"))
      ;; Header shows the total number of mentions (not groups)
      (should (string-match-p "Unlinked Mentions (3)" output))
      ;; Both mentioning notes appear, with their context lines
      (should (string-match-p "Note A" output))
      (should (string-match-p "Note B" output))
      (should (string-match-p "mentions Target here" output))
      (should (string-match-p "Target again" output))
      (should (string-match-p "a Target reference" output)))))

(ert-deftest vulpea-ui-test-mentions-widget-empty ()
  "No mentions renders the empty-state message."
  (vulpea-ui-test--render-mentions nil
    (should (string-match-p "No unlinked mentions" output))))


;;; Schema health widget

(ert-deftest vulpea-ui-test-schema-health-no-schema ()
  "No applicable schema yields nil, so the widget hides."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq)))
    (vulpea-schema-define 'wine
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields '((:key "name" :required t)))
    (should-not (vulpea-ui--schema-health
                 (make-vulpea-note :id "b" :title "B" :tags '("beer"))))
    (should-not (vulpea-ui--schema-health nil))))

(ert-deftest vulpea-ui-test-schema-health-conformant ()
  "A conformant note reports its schema and no violations."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq)))
    (vulpea-schema-define 'wine
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields '((:key "name" :required t)))
    (let ((h (vulpea-ui--schema-health
              (make-vulpea-note :id "w" :title "W" :tags '("wine")
                                :meta '(("name" "Chablis"))))))
      (should (equal (plist-get h :schemas) '(wine)))
      (should-not (plist-get h :violations)))))

(ert-deftest vulpea-ui-test-schema-health-violations ()
  "A non-conformant note reports its violations."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq)))
    (vulpea-schema-define 'wine
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields '((:key "name" :required t)
                (:key "colour" :type symbol :one-of (red white))))
    (let* ((h (vulpea-ui--schema-health
               (make-vulpea-note :id "w" :title "W" :tags '("wine")
                                 :meta '(("colour" "blue")))))
           (vs (plist-get h :violations)))
      (should (equal (plist-get h :schemas) '(wine)))
      (should (= (length vs) 2)))))

(ert-deftest vulpea-ui-test-schema-violation-severity ()
  "Structural problems are errors; value problems are warnings."
  (should (eq (vulpea-ui--schema-violation-severity 'missing-required) 'error))
  (should (eq (vulpea-ui--schema-violation-severity 'wrong-type) 'error))
  (should (eq (vulpea-ui--schema-violation-severity 'invalid-reference) 'error))
  (should (eq (vulpea-ui--schema-violation-severity 'disallowed-value) 'warning))
  (should (eq (vulpea-ui--schema-violation-severity 'invalid-value) 'warning))
  (should (eq (vulpea-ui--schema-violation-severity 'invalid-target) 'warning)))

(ert-deftest vulpea-ui-test-schema-violation-reason ()
  "Reason text is terse and field-spec aware."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq)))
    (vulpea-schema-define 'wine
      :predicate (lambda (_n) t)
      :fields '((:key "name" :required t)
                (:key "colour" :type symbol :one-of (red white rose))
                (:key "vintage" :type number)))
    (let* ((note (make-vulpea-note :id "w" :title "W" :tags '("wine")
                                   :meta '(("colour" "blue") ("vintage" "old"))))
           (vs (vulpea-schema-validate note 'wine))
           (by-field (lambda (f) (cl-find f vs :key #'vulpea-violation-field
                                          :test #'equal))))
      (should (equal (vulpea-ui--schema-violation-reason
                      (funcall by-field "name") note)
                     "required"))
      (should (string-match-p
               "red/white/rose"
               (vulpea-ui--schema-violation-reason (funcall by-field "colour") note)))
      (should (string-match-p
               "number"
               (vulpea-ui--schema-violation-reason
                (funcall by-field "vintage") note))))))

(defmacro vulpea-ui-test--with-wine-note (content meta &rest body)
  "Visit a temp wine file of CONTENT; bind NOTE (level-0, META) and BUF.
Registers a `wine' schema requiring `name' and constraining `colour'."
  (declare (indent 2))
  `(let ((vulpea-schema--registry (make-hash-table :test 'eq))
         (file (make-temp-file "vulpea-ui-schema-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (vulpea-schema-define 'wine
             :predicate (lambda (_n) t)
             :fields '((:key "name" :required t)
                       (:key "colour" :type symbol :one-of (red white))))
           (let ((buf (find-file-noselect file))
                 (note (make-vulpea-note :id "w1" :title "Wine" :path file
                                         :level 0 :pos 1 :tags '("wine")
                                         :meta ,meta)))
             (unwind-protect (progn ,@body) (kill-buffer buf))))
       (when (file-exists-p file) (delete-file file)))))

(defun vulpea-ui-test--wine-violation (note field)
  "Return NOTE's wine-schema violation for FIELD."
  (cl-find field (vulpea-schema-validate note 'wine)
           :key #'vulpea-violation-field :test #'equal))

(ert-deftest vulpea-ui-test-schema-violation-position-value ()
  "A value violation points at its field's line."
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: blue\n"
      '(("colour" "blue"))
    (with-current-buffer buf
      (goto-char (vulpea-ui--schema-violation-position
                  (vulpea-ui-test--wine-violation note "colour") note))
      (should (looking-at-p "^- colour ::")))))

(ert-deftest vulpea-ui-test-schema-violation-position-missing-with-meta ()
  "A missing field lands on the metadata block, not the top of the file."
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: blue\n"
      '(("colour" "blue"))
    (with-current-buffer buf
      (let ((pos (vulpea-ui--schema-violation-position
                  (vulpea-ui-test--wine-violation note "name") note)))
        (should (> pos (point-min)))
        (goto-char pos)
        (should (looking-at-p "^- colour ::"))))))

(ert-deftest vulpea-ui-test-schema-violation-position-missing-no-meta ()
  "A missing field with no metadata yet lands after the note's header."
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\nbody text\n"
      nil
    (with-current-buffer buf
      (let ((pos (vulpea-ui--schema-violation-position
                  (vulpea-ui-test--wine-violation note "name") note)))
        (should (> pos (point-min)))
        (goto-char pos)
        (should-not (looking-at-p "^\\(:\\|#\\+\\)"))))))

(ert-deftest vulpea-ui-test-schema-fix-violation-action ()
  "Fixing a violation persists the change, re-indexes, and refreshes."
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: blue\n"
      '(("colour" "blue"))
    (let ((fixed nil) (reindexed nil) (refreshed nil) (saved nil))
      (cl-letf (((symbol-function 'vulpea-schema-fix-violation)
                 (lambda (v &optional _bound) (setq fixed v) "Chateau Test"))
                ((symbol-function 'vulpea-db-update-file)
                 (lambda (p) (setq reindexed p)))
                ((symbol-function 'vulpea-ui-sidebar-refresh)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (setq saved t))))
        (vulpea-ui--schema-fix-violation-action
         note (vulpea-ui-test--wine-violation note "name"))
        (should fixed)
        (should saved)
        (should (equal reindexed (vulpea-note-path note)))
        (should refreshed)))))

(ert-deftest vulpea-ui-test-schema-fix-violation-action-skip ()
  "A cancelled fix persists nothing and does not refresh."
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: blue\n"
      '(("colour" "blue"))
    (let ((reindexed nil) (refreshed nil) (saved nil))
      (cl-letf (((symbol-function 'vulpea-schema-fix-violation)
                 (lambda (_v &optional _bound) nil))
                ((symbol-function 'vulpea-db-update-file)
                 (lambda (_p) (setq reindexed t)))
                ((symbol-function 'vulpea-ui-sidebar-refresh)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (setq saved t))))
        (vulpea-ui--schema-fix-violation-action
         note (vulpea-ui-test--wine-violation note "name"))
        (should-not saved)
        (should-not reindexed)
        (should-not refreshed)))))

(ert-deftest vulpea-ui-test-schema-fix-violation-action-writes-buffer ()
  "The action drives the real fixer to insert the missing field."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: red\n"
      '(("colour" "red"))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "Chateau Test"))
              ((symbol-function 'vulpea-db-update-file) #'ignore)
              ((symbol-function 'vulpea-ui-sidebar-refresh) #'ignore))
      (vulpea-ui--schema-fix-violation-action
       note (vulpea-ui-test--wine-violation note "name"))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (re-search-forward "^- name :: Chateau Test" nil t))))))

;;; Schema dashboard

(ert-deftest vulpea-ui-test-schema-dashboard-sort ()
  "Schemas needing attention sort first, unused last, then by name."
  (let ((wine (vulpea-schema-health--create :schema 'wine :covered 5 :invalid 2))
        (apple (vulpea-schema-health--create :schema 'apple :covered 3 :invalid 1))
        (producer (vulpea-schema-health--create :schema 'producer :covered 5 :invalid 0))
        (region (vulpea-schema-health--create :schema 'region :covered 0 :invalid 0)))
    (should (equal (mapcar #'vulpea-schema-health-schema
                           (vulpea-ui-schema-dashboard--sort
                            (list producer region wine apple)))
                   '(apple wine producer region)))))

(ert-deftest vulpea-ui-test-schema-dashboard-status ()
  "Status text and face reflect covered and invalid counts."
  (let ((inv (vulpea-schema-health--create :schema 'w :covered 5 :invalid 2))
        (ok (vulpea-schema-health--create :schema 'w :covered 5 :invalid 0))
        (one (vulpea-schema-health--create :schema 'w :covered 1 :invalid 0))
        (unused (vulpea-schema-health--create :schema 'w :covered 0 :invalid 0)))
    (should (equal (vulpea-ui-schema-dashboard--status-text inv) "5 notes · 2 invalid"))
    (should (equal (vulpea-ui-schema-dashboard--status-text ok) "5 notes · all valid"))
    (should (equal (vulpea-ui-schema-dashboard--status-text one) "1 note · all valid"))
    (should (equal (vulpea-ui-schema-dashboard--status-text unused) "unused"))
    (should (eq (vulpea-ui-schema-dashboard--status-face inv)
                'vulpea-ui-schema-health-error-face))
    (should (eq (vulpea-ui-schema-dashboard--status-face ok)
                'vulpea-ui-schema-health-ok-face))
    (should (eq (vulpea-ui-schema-dashboard--status-face unused) 'shadow))))

(ert-deftest vulpea-ui-test-schema-dashboard-summary ()
  "The summary counts schemas and how many have issues."
  (let ((a (vulpea-schema-health--create :schema 'a :covered 5 :invalid 2))
        (b (vulpea-schema-health--create :schema 'b :covered 5 :invalid 0))
        (c (vulpea-schema-health--create :schema 'c :covered 0 :invalid 0)))
    (should (equal (vulpea-ui-schema-dashboard--summary-text (list a b c))
                   "3 schemas · 1 with issues"))
    (should (equal (vulpea-ui-schema-dashboard--summary-text (list b c))
                   "2 schemas · all healthy"))
    (should (equal (vulpea-ui-schema-dashboard--summary-text (list a))
                   "1 schema · 1 with issues"))))

(ert-deftest vulpea-ui-test-schema-dashboard-includes-text ()
  "Include relationships render both ways, or nil when absent."
  (let ((inc (vulpea-schema-health--create :schema 'wine :includes '(base-thing)))
        (by (vulpea-schema-health--create :schema 'base :included-by '(wine producer)))
        (both (vulpea-schema-health--create :schema 'mid :includes '(base)
                                            :included-by '(leaf)))
        (none (vulpea-schema-health--create :schema 'lonely)))
    (should (equal (vulpea-ui-schema-dashboard--includes-text inc)
                   "includes base-thing"))
    (should (equal (vulpea-ui-schema-dashboard--includes-text by)
                   "included by wine, producer"))
    (should (equal (vulpea-ui-schema-dashboard--includes-text both)
                   "includes base · included by leaf"))
    (should-not (vulpea-ui-schema-dashboard--includes-text none))))

(ert-deftest vulpea-ui-test-schema-dashboard-width ()
  "Width falls back to `fill-column' when the dashboard has no window."
  (with-temp-buffer
    (setq fill-column 72)
    (should (= (vulpea-ui-schema-dashboard--width) 72))))

(ert-deftest vulpea-ui-test-schema-dashboard-renders ()
  "The command builds a buffer listing schemas, counts, and invalid notes."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq)))
    (vulpea-schema-define 'wine
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields '((:key "name" :required t)))
    (cl-letf (((symbol-function 'vulpea-db-query)
               (lambda (&rest _)
                 (list (make-vulpea-note :id "a" :title "Good Wine"
                                         :tags '("wine") :meta '(("name" "A")))
                       (make-vulpea-note :id "b" :title "Bad Wine"
                                         :tags '("wine") :meta nil)))))
      (save-window-excursion
        (unwind-protect
            (progn
              (vulpea-ui-schema-dashboard)
              (with-current-buffer "*vulpea schema*"
                (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                  (should (string-match-p "Schema health" text))
                  (should (string-match-p "wine" text))
                  (should (string-match-p "1 invalid" text))
                  (should (string-match-p "Bad Wine" text)))))
          (when (get-buffer "*vulpea schema*")
            (kill-buffer "*vulpea schema*")))))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-violation ()
  "Fixing one violation from the dashboard persists, re-indexes, refreshes."
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: blue\n"
      '(("colour" "blue"))
    (let ((fixed nil) (reindexed nil) (refreshed nil) (saved nil)
          (v (make-vulpea-violation :type 'disallowed-value :field "colour")))
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                ((symbol-function 'vulpea-schema-fix-violation)
                 (lambda (vv &optional _bound) (setq fixed vv) "red"))
                ((symbol-function 'vulpea-db-update-file)
                 (lambda (p) (setq reindexed p)))
                ((symbol-function 'vulpea-ui-schema-dashboard-refresh)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (setq saved t))))
        (vulpea-ui-schema-dashboard--fix-violation note v)
        (should (eq fixed v))
        (should saved)
        (should (equal reindexed (vulpea-note-path note)))
        (should refreshed)))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-violation-skip ()
  "A skipped fix persists nothing and does not refresh."
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: blue\n"
      '(("colour" "blue"))
    (let ((reindexed nil) (refreshed nil) (saved nil)
          (v (make-vulpea-violation :type 'disallowed-value :field "colour")))
      (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                ((symbol-function 'vulpea-schema-fix-violation)
                 (lambda (_v &optional _b) nil))
                ((symbol-function 'vulpea-db-update-file)
                 (lambda (_p) (setq reindexed t)))
                ((symbol-function 'vulpea-ui-schema-dashboard-refresh)
                 (lambda () (setq refreshed t)))
                ((symbol-function 'save-buffer)
                 (lambda (&rest _) (setq saved t))))
        (vulpea-ui-schema-dashboard--fix-violation note v)
        (should-not saved)
        (should-not reindexed)
        (should-not refreshed)))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-violation-writes ()
  "The dashboard fix drives the real fixer for one violation."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: red\n"
      '(("colour" "red"))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'read-string)
               (lambda (&rest _) "Chateau Test"))
              ((symbol-function 'vulpea-db-update-file) #'ignore)
              ((symbol-function 'vulpea-ui-schema-dashboard-refresh) #'ignore))
      (vulpea-ui-schema-dashboard--fix-violation
       note (car (vulpea-schema-validate note 'wine)))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (re-search-forward "^- name :: Chateau Test" nil t))))))

(provide 'vulpea-ui-test)
;;; vulpea-ui-test.el ends here
