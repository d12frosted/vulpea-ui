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
    (should-not (vulpea-ui--schema-note-health
                 (make-vulpea-note :id "b" :title "B" :tags '("beer"))))
    (should-not (vulpea-ui--schema-note-health nil))))

(ert-deftest vulpea-ui-test-schema-health-conformant ()
  "A conformant note reports its schema and no violations."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq)))
    (vulpea-schema-define 'wine
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields '((:key "name" :required t)))
    (let ((h (vulpea-ui--schema-note-health
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
    (let* ((h (vulpea-ui--schema-note-health
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

(ert-deftest vulpea-ui-test-schema-dashboard-refresh-targets-buffer ()
  "Refreshing from another buffer updates the dashboard, not the caller."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq)))
    (vulpea-schema-define 'wine
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields '((:key "name" :required t)))
    (cl-letf (((symbol-function 'vulpea-db-query)
               (lambda (&rest _)
                 (list (make-vulpea-note :id "b" :title "Bad Wine"
                                         :tags '("wine") :meta nil)))))
      (let ((other (get-buffer-create "*not-the-dashboard*")))
        (unwind-protect
            (save-window-excursion
              (vulpea-ui-schema-dashboard)
              (with-current-buffer other
                (insert "untouched")
                (vulpea-ui-schema-dashboard-refresh)
                (should (equal (buffer-string) "untouched"))
                (should-not vulpea-ui-schema-dashboard--instance))
              (with-current-buffer vulpea-ui-schema-dashboard-buffer-name
                (should vulpea-ui-schema-dashboard--instance)))
          (kill-buffer other)
          (when (get-buffer vulpea-ui-schema-dashboard-buffer-name)
            (kill-buffer vulpea-ui-schema-dashboard-buffer-name)))))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-keeps-note-buffer ()
  "Fixing from the dashboard leaves the note's own buffer an org buffer."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (vulpea-ui-test--with-wine-note
      ":PROPERTIES:\n:ID: w1\n:END:\n#+title: Wine\n#+filetags: :wine:\n\n- colour :: red\n"
      '(("colour" "red"))
    (cl-letf (((symbol-function 'vulpea-db-query) (lambda (&rest _) (list note)))
              ((symbol-function 'read-string) (lambda (&rest _) "Chateau Test"))
              ((symbol-function 'vulpea-db-update-file) #'ignore))
      (unwind-protect
          (save-window-excursion
            (vulpea-ui-schema-dashboard)
            (vulpea-ui-schema-dashboard--fix-violation
             note (car (vulpea-schema-validate note 'wine)))
            (should (buffer-live-p buf))
            (with-current-buffer buf
              (should (derived-mode-p 'org-mode))
              (should-not vulpea-ui-schema-dashboard--instance)))
        (when (get-buffer vulpea-ui-schema-dashboard-buffer-name)
          (kill-buffer vulpea-ui-schema-dashboard-buffer-name))))))

;;; Schema dashboard cursor identity

(defun vulpea-ui-test--dashboard-health ()
  "Build a two-schema health list that stresses key uniqueness.
`producer' and `wine' both flag note a (Chateau) for a missing `name',
so the same note and the same name/missing-required violation appear in
two sections (keys must separate them by schema).  `wine' also flags
Chateau's colour and gives note c (Rioja) two colour/disallowed-value
violations, which share a field and type within one section (keys must
separate them by their per-note index)."
  (let* ((chateau (make-vulpea-note :id "a" :title "Chateau"
                                    :path "/tmp/a.org" :level 0 :pos 1))
         (rioja (make-vulpea-note :id "c" :title "Rioja"
                                  :path "/tmp/c.org" :level 0 :pos 1))
         (mk (lambda (schema field type)
               (make-vulpea-violation :schema schema :field field :type type
                                      :note-id "x" :note-title "x")))
         (wine (vulpea-schema-health--create
                :schema 'wine :covered 3 :invalid 2
                :invalid-notes
                (list (cons chateau
                            (list (funcall mk 'wine "name" 'missing-required)
                                  (funcall mk 'wine "colour" 'disallowed-value)))
                      (cons rioja
                            (list (funcall mk 'wine "colour" 'disallowed-value)
                                  (funcall mk 'wine "colour" 'disallowed-value))))))
         (producer (vulpea-schema-health--create
                    :schema 'producer :covered 2 :invalid 1
                    :invalid-notes
                    (list (cons chateau
                                (list (funcall mk 'producer "name"
                                               'missing-required)))))))
    (vulpea-ui-schema-dashboard--sort (list wine producer))))

(defun vulpea-ui-test--expand-notes ()
  "Press every collapsed note toggle (▶ Chateau/Rioja) until all expand.
Bounded so a stuck render never spins forever."
  (let ((titles '("Chateau" "Rioja"))
        (guard 0))
    (catch 'done
      (while (< guard 20)
        (setq guard (1+ guard))
        (let ((pressed nil))
          (catch 'again
            (dolist (w (vui--collect-widgets))
              (let ((tag (vui-element-get w :vui-tag)))
                (when (and tag (string-prefix-p "▶ " tag)
                           (member (substring tag 2) titles))
                  (vui-activate (car (vui--widget-bounds w)))
                  (setq pressed t)
                  (throw 'again nil)))))
          (unless pressed (throw 'done nil)))))))

(defmacro vulpea-ui-test--with-dashboard (&rest body)
  "Mount the dashboard root on the two-schema fixture, run BODY, clean up.
BODY runs in the dashboard buffer with all note toggles expanded and
`vui-render-delay' disabled so presses re-render synchronously."
  (declare (indent 0))
  `(let ((vui-render-delay nil)
         (fill-column 60))
     (unwind-protect
         (progn
           (vui-mount (vui-component 'vulpea-ui-schema-dashboard-root
                                     :health (vulpea-ui-test--dashboard-health))
                      "*vulpea schema test*")
           (with-current-buffer "*vulpea schema test*"
             (vulpea-ui-test--expand-notes)
             ,@body))
       (when (get-buffer "*vulpea schema test*")
         (kill-buffer "*vulpea schema test*")))))

(ert-deftest vulpea-ui-test-schema-dashboard-widget-keys ()
  "Every toggle and violation button carries a stable, globally-unique key.
Cursor restoration searches the whole buffer by a widget's `:vui-key', so
the keys have to be unique across sections, not merely within one."
  (vulpea-ui-test--with-dashboard
    (let ((keys (mapcar (lambda (w) (vui-element-get w :vui-key))
                        (vui--collect-widgets))))
      ;; Section toggles keyed by their schema symbol.
      (should (member 'wine keys))
      (should (member 'producer keys))
      ;; Note toggles keyed by (schema note-id); the shared note a gets a
      ;; distinct key under each schema.
      (should (member (list 'wine "a") keys))
      (should (member (list 'producer "a") keys))
      (should (member (list 'wine "c") keys))
      ;; Field buttons keyed by (field schema note-id field type index).
      ;; SCHEMA keeps a's name/missing-required distinct across sections.
      (should (member (list 'field 'wine "a" "name" 'missing-required 0) keys))
      (should (member (list 'field 'producer "a" "name" 'missing-required 0) keys))
      (should (member (list 'field 'wine "a" "colour" 'disallowed-value 1) keys))
      ;; Rioja's two colour/disallowed-value violations share a field and
      ;; type; the per-note INDEX is what keeps their keys apart.
      (should (member (list 'field 'wine "c" "colour" 'disallowed-value 0) keys))
      (should (member (list 'field 'wine "c" "colour" 'disallowed-value 1) keys))
      ;; Fix buttons mirror the field keys under a `fix' tag (when the fixer
      ;; is available, which is when the button renders at all).
      (when (fboundp 'vulpea-schema-fix-violation)
        (should (member (list 'fix 'wine "a" "name" 'missing-required 0) keys))
        (should (member (list 'fix 'producer "a" "name" 'missing-required 0) keys))
        (should (member (list 'fix 'wine "a" "colour" 'disallowed-value 1) keys))
        (should (member (list 'fix 'wine "c" "colour" 'disallowed-value 0) keys))
        (should (member (list 'fix 'wine "c" "colour" 'disallowed-value 1) keys)))
      ;; No interactive widget is left keyless, and no two share a key
      ;; (this is what would fail if the multi-value index were dropped).
      (should-not (memq nil keys))
      (should (= (length keys) (length (seq-uniq keys #'equal)))))))

(ert-deftest vulpea-ui-test-schema-dashboard-cursor-keeps-widget ()
  "Toggling a note keeps point on that toggle, not a same-label sibling.
Note a (Chateau) is invalid under both schemas, so two `▶ Chateau'
toggles exist.  Expanding wine's flips its label to `▼ Chateau'; without
a stable key, cursor restoration relocates point onto producer's
still-collapsed `▶ Chateau'."
  (let ((vui-render-delay nil)
        (fill-column 60))
    (unwind-protect
        (progn
          (vui-mount (vui-component 'vulpea-ui-schema-dashboard-root
                                    :health (vulpea-ui-test--dashboard-health))
                     "*vulpea schema test*")
          (with-current-buffer "*vulpea schema test*"
            (let* ((toggles (seq-filter
                             (lambda (w) (equal (vui-element-get w :vui-tag) "▶ Chateau"))
                             (vui--collect-widgets)))
                   (wine-chateau (car (last toggles))))
              ;; Two collapsed Chateau toggles, one per section.
              (should (= (length toggles) 2))
              ;; Park point on wine's Chateau toggle and expand it.
              (goto-char (car (vui--widget-bounds wine-chateau)))
              (vui-activate)
              ;; Point rides the toggle we pressed: it is now expanded
              ;; (▼ Chateau), not producer's still-collapsed ▶ Chateau.
              (should (equal (vui-element-get (vui-element-at) :vui-tag)
                             "▼ Chateau")))))
      (when (get-buffer "*vulpea schema test*")
        (kill-buffer "*vulpea schema test*")))))

(ert-deftest vulpea-ui-test-schema-dashboard-cursor-survives-refresh ()
  "A refresh that drops rows above point keeps point on its own fix button.
This is the g-refresh / after-fix path: `vui-update' rebuilds the whole
buffer and rows above the target shift.  Every fix button shares the
label `fix', so only its key keeps point from sliding onto a sibling."
  (skip-unless (fboundp 'vulpea-schema-fix-violation)) ; no fix buttons otherwise
  (let ((vui-render-delay nil)
        (fill-column 60))
    (unwind-protect
        (let ((instance
               (vui-mount (vui-component 'vulpea-ui-schema-dashboard-root
                                         :health (vulpea-ui-test--dashboard-health))
                          "*vulpea schema test*")))
          (with-current-buffer "*vulpea schema test*"
            (vulpea-ui-test--expand-notes)
            ;; Fix buttons in buffer order: producer/Chateau/name (0),
            ;; wine/Chateau/name (1, our target), wine/Chateau/colour, then
            ;; Rioja's two.  Locate the target by POSITION, not by its key, so
            ;; a broken key makes this fail rather than silently skip.
            (let* ((fixes (seq-filter
                           (lambda (w) (equal (vui-element-get w :vui-tag) "fix"))
                           (vui--collect-widgets)))
                   (target (nth 1 fixes))
                   (target-key (list 'fix 'wine "a" "name" 'missing-required 0)))
              (should (equal (vui-element-get target :vui-key) target-key))
              ;; Park point on it, then refresh with producer gone so the rows
              ;; above it disappear.
              (goto-char (car (vui--widget-bounds target)))
              (vui-update instance
                          (list :health
                                (seq-filter
                                 (lambda (h)
                                   (eq (vulpea-schema-health-schema h) 'wine))
                                 (vulpea-ui-test--dashboard-health))))
              ;; Point is still on the very same fix button, not another `fix'.
              (should (equal (vui-key-at)
                             target-key)))))
      (when (get-buffer "*vulpea schema test*")
        (kill-buffer "*vulpea schema test*")))))

;;; Schema dashboard point after a fix

(defun vulpea-ui-test--fix-health (where target-issues)
  "Wine health with a Target note among siblings; plus a trailing `zzz'.
WHERE places Target `middle' (Alpha, Target, Gamma) or `last' (Alpha,
Beta, Target).  Target carries TARGET-ISSUES vintage/wrong-type issues;
0 omits it, standing in for a note whose last issue was just fixed away."
  (let* ((note (lambda (id)
                 (make-vulpea-note :id id :title (concat "N-" id)
                                   :path (expand-file-name (format "/tmp/%s.org" id))
                                   :level 0 :pos 1)))
         (viols (lambda (n)
                  (make-list n (make-vulpea-violation
                                :schema 'wine :field "vintage" :type 'wrong-type
                                :note-id "x" :note-title "x"))))
         (entry (lambda (id n) (cons (funcall note id) (funcall viols n))))
         (target (and (> target-issues 0) (funcall entry "target" target-issues)))
         (notes (delq nil
                      (pcase where
                        ('middle (list (funcall entry "alpha" 1) target
                                       (funcall entry "gamma" 1)))
                        ('last (list (funcall entry "alpha" 1)
                                     (funcall entry "beta" 1) target))))))
    (vulpea-ui-schema-dashboard--sort
     (list (vulpea-schema-health--create
            :schema 'wine :covered 9 :invalid (length notes) :invalid-notes notes)
           (vulpea-schema-health--create :schema 'zzz :covered 2 :invalid 0)))))

(defun vulpea-ui-test--simulate-fix (instance where before after)
  "Stand on Target's toggle, refresh to health (WHERE AFTER), return where
point lands.  This mirrors `vulpea-ui-schema-dashboard--fix-violation',
which anchors point on the fixed note's toggle before refreshing and
leaves the landing to vui's cursor restoration."
  (ignore before)
  (goto-char (car (vui--widget-bounds
                   (seq-find (lambda (w) (equal (vui-element-get w :vui-key)
                                                (list 'wine "target")))
                             (vui--collect-widgets)))))
  (vui-update instance (list :health (vulpea-ui-test--fix-health where after)))
  (vui-key-at))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-lands-on-next-note ()
  "Fixing a middle note leaves point on the note that slid into its place.
When the note is fixed away, vui's cursor restoration lands on the
sibling now at its tree path: the next note."
  (let ((vui-render-delay nil) (fill-column 80)
        (bufname vulpea-ui-schema-dashboard-buffer-name))
    (unwind-protect
        (let ((instance (vui-mount
                         (vui-component 'vulpea-ui-schema-dashboard-root
                                        :health (vulpea-ui-test--fix-health 'middle 1))
                         bufname)))
          (with-current-buffer bufname
            (let ((landed (vulpea-ui-test--simulate-fix instance 'middle 1 0)))
              (should-not (= (point) (point-min)))
              (should (equal landed (list 'wine "gamma"))))))
      (when (get-buffer bufname) (kill-buffer bufname)))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-keeps-point-on-note ()
  "Fixing one of a note's several issues keeps point on that note.
The note survives the refresh with one fewer issue, so point on its
toggle is restored to the same toggle."
  (let ((vui-render-delay nil) (fill-column 80)
        (bufname vulpea-ui-schema-dashboard-buffer-name))
    (unwind-protect
        (let ((instance (vui-mount
                         (vui-component 'vulpea-ui-schema-dashboard-root
                                        :health (vulpea-ui-test--fix-health 'middle 2))
                         bufname)))
          (with-current-buffer bufname
            (should (equal (vulpea-ui-test--simulate-fix instance 'middle 2 1)
                           (list 'wine "target")))))
      (when (get-buffer bufname) (kill-buffer bufname)))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-last-note-moves-up ()
  "Fixing the section's last note moves point up to the previous note.
Target is wine's last note; once it is gone vui recovers to the previous
note in the same section (a longer shared path prefix) rather than down
into the trailing `zzz' section."
  (let ((vui-render-delay nil) (fill-column 80)
        (bufname vulpea-ui-schema-dashboard-buffer-name))
    (unwind-protect
        (let ((instance (vui-mount
                         (vui-component 'vulpea-ui-schema-dashboard-root
                                        :health (vulpea-ui-test--fix-health 'last 1))
                         bufname)))
          (with-current-buffer bufname
            (should (equal (vulpea-ui-test--simulate-fix instance 'last 1 0)
                           (list 'wine "beta")))))
      (when (get-buffer bufname) (kill-buffer bufname)))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-violation-repositions-point ()
  "The whole fix action leaves point on the previous note when it clears the
section's last note: the dashboard anchors point on the fixed note and vui
recovers from there.  Here point starts on the note, as after activating
the fix button from the keyboard."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (let ((vui-render-delay nil) (fill-column 80)
        (bufname vulpea-ui-schema-dashboard-buffer-name)
        (gone nil))
    (cl-letf (((symbol-function 'vulpea-schema-collection-health)
               (lambda (&rest _) (vulpea-ui-test--fix-health 'last (if gone 0 1))))
              ((symbol-function 'vulpea-ui-schema-dashboard--visit-field)
               (lambda (&rest _) (get-buffer-create "*fix-note*")))
              ((symbol-function 'vulpea-schema-fix-violation)
               (lambda (&rest _) (setq gone t) "2020"))
              ((symbol-function 'save-buffer) #'ignore)
              ((symbol-function 'vulpea-db-update-file) #'ignore))
      (unwind-protect
          (save-window-excursion
            (vulpea-ui-schema-dashboard)
            (with-current-buffer bufname
              ;; Stand on wine's last note, then fix its only issue away.
              (goto-char (car (vui--widget-bounds
                               (seq-find (lambda (w) (equal (vui-element-get w :vui-key)
                                                            (list 'wine "target")))
                                         (vui--collect-widgets)))))
              (vulpea-ui-schema-dashboard--fix-violation
               (make-vulpea-note :id "target" :title "N-target"
                                 :path "/tmp/target.org" :level 0 :pos 1)
               (make-vulpea-violation :schema 'wine :field "vintage"
                                      :type 'wrong-type :note-id "target"))
              ;; wine's last note is gone; point moves up to the previous one.
              (should-not (= (point) (point-min)))
              (should (equal (vui-key-at)
                             (list 'wine "beta")))))
        (when (get-buffer bufname) (kill-buffer bufname))
        (when (get-buffer "*fix-note*") (kill-buffer "*fix-note*"))))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-violation-off-widget ()
  "Fixing still lands on the previous note when point was not on any widget.
Clicking the fix button with the mouse does not move point, so it can sit
on the summary line at the top when the refresh runs.  vui would keep that
non-widget line, dropping point to the top; the dashboard anchors point on
the fixed note first, so vui recovers to the previous note instead."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (let ((vui-render-delay nil) (fill-column 80)
        (bufname vulpea-ui-schema-dashboard-buffer-name)
        (gone nil))
    (cl-letf (((symbol-function 'vulpea-schema-collection-health)
               (lambda (&rest _) (vulpea-ui-test--fix-health 'last (if gone 0 1))))
              ((symbol-function 'vulpea-ui-schema-dashboard--visit-field)
               (lambda (&rest _) (get-buffer-create "*fix-note*")))
              ((symbol-function 'vulpea-schema-fix-violation)
               (lambda (&rest _) (setq gone t) "2020"))
              ((symbol-function 'save-buffer) #'ignore)
              ((symbol-function 'vulpea-db-update-file) #'ignore))
      (unwind-protect
          (save-window-excursion
            (vulpea-ui-schema-dashboard)
            (with-current-buffer bufname
              ;; A mouse click on the fix button does not move point: it sits
              ;; at point-min, off any widget, when the refresh runs.
              (goto-char (point-min))
              (vulpea-ui-schema-dashboard--fix-violation
               (make-vulpea-note :id "target" :title "N-target"
                                 :path "/tmp/target.org" :level 0 :pos 1)
               (make-vulpea-violation :schema 'wine :field "vintage"
                                      :type 'wrong-type :note-id "target"))
              (should-not (= (point) (point-min)))
              (should (equal (vui-key-at)
                             (list 'wine "beta")))))
        (when (get-buffer bufname) (kill-buffer bufname))
        (when (get-buffer "*fix-note*") (kill-buffer "*fix-note*"))))))

;;; Schema - heading-level notes

;; The file-level schema tests above never exercise a heading-level note
;; (every fixture is `:level 0').  These cover the heading paths - the
;; same class of scoping that vulpea#356/#357 fixed in vulpea itself:
;; scope must be the heading's subtree, not the whole file.

(defmacro vulpea-ui-test--with-heading-note (meta &rest body)
  "Visit a file whose FILE-level note is not wine but which holds a wine
HEADING; bind NOTE to that heading (level-1, META) and BUF.

The fixture is shaped like vulpea#356: a `journal' file with a `* Wine
:wine:' heading, a trailing sibling heading so the subtree end differs
from `point-max', and a file-level meta line so a file-scoped bug is
visible.  Registers a `wine' schema requiring `name', constraining
`colour'."
  (declare (indent 1))
  `(let ((vulpea-schema--registry (make-hash-table :test 'eq))
         (file (make-temp-file "vulpea-ui-schema-h-" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file file
             (insert ":PROPERTIES:\n:ID: file-1\n:END:\n"
                     "#+title: Journal\n#+filetags: :journal:\n\n"
                     "- note :: file level meta\n\n"
                     "* Wine :wine:\n"
                     ":PROPERTIES:\n:ID: w-head\n:END:\n"
                     "- colour :: blue\n\n"
                     "* Other :misc:\ntrailing text\n"))
           (vulpea-schema-define 'wine
             :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
             :fields '((:key "name" :required t)
                       (:key "colour" :type symbol :one-of (red white))))
           (let* ((buf (find-file-noselect file))
                  (pos (with-current-buffer buf
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward "^\\* Wine")
                           (line-beginning-position))))
                  (note (make-vulpea-note :id "w-head" :title "Wine" :path file
                                          :level 1 :pos pos :tags '("wine")
                                          :meta ,meta)))
             (unwind-protect (progn ,@body) (kill-buffer buf))))
       (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-ui-test-schema-note-end-heading ()
  "A heading note's scope ends at its subtree, not `point-max'."
  (vulpea-ui-test--with-heading-note '(("colour" "blue"))
    (with-current-buffer buf
      (let ((end (vulpea-ui--schema-note-end note)))
        (should (> end (vulpea-note-pos note)))
        (should (< end (point-max)))
        (let ((sub (buffer-substring-no-properties (vulpea-note-pos note) end)))
          (should (string-match-p "colour :: blue" sub))
          (should-not (string-match-p "trailing text" sub)))))))

(ert-deftest vulpea-ui-test-schema-meta-position-heading ()
  "Metadata resolves to the heading's own meta, below its heading line."
  (vulpea-ui-test--with-heading-note '(("colour" "blue"))
    (with-current-buffer buf
      (goto-char (vulpea-ui--schema-meta-position note))
      (should (looking-at-p "^- colour ::"))
      (should (> (point) (vulpea-note-pos note))))))

(ert-deftest vulpea-ui-test-schema-violation-position-heading-missing ()
  "A missing field on a heading lands inside its subtree, not the file top."
  (vulpea-ui-test--with-heading-note '(("colour" "blue"))
    (with-current-buffer buf
      (let ((pos (vulpea-ui--schema-violation-position
                  (vulpea-ui-test--wine-violation note "name") note)))
        (should (>= pos (vulpea-note-pos note)))
        (should (< pos (vulpea-ui--schema-note-end note)))
        (goto-char pos)
        (should (looking-at-p "^- colour ::"))))))

(ert-deftest vulpea-ui-test-schema-violation-position-heading-value ()
  "A value violation on a heading points at that heading's field line."
  (vulpea-ui-test--with-heading-note '(("colour" "blue"))
    (with-current-buffer buf
      (goto-char (vulpea-ui--schema-violation-position
                  (vulpea-ui-test--wine-violation note "colour") note))
      (should (looking-at-p "^- colour ::"))
      (should (> (point) (vulpea-note-pos note))))))

(ert-deftest vulpea-ui-test-schema-note-end-stops-before-child ()
  "A heading's section ends at its first child, not at the subtree end.
Metadata under a child heading belongs to the child note, so it must be
out of the parent's scope."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq))
        (file (make-temp-file "vulpea-ui-nested-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: file-1\n:END:\n#+title: J\n\n"
                    "* Wine :wine:\n:PROPERTIES:\n:ID: w\n:END:\n"
                    "** Child\n- foo :: bar\n"))
          (vulpea-schema-define 'wine :predicate (lambda (_n) t)
            :fields '((:key "name" :required t)))
          (let* ((buf (find-file-noselect file))
                 (wpos (with-current-buffer buf
                         (save-excursion (goto-char (point-min))
                                         (re-search-forward "^\\* Wine")
                                         (line-beginning-position))))
                 (cpos (with-current-buffer buf
                         (save-excursion (goto-char (point-min))
                                         (re-search-forward "^\\*\\* Child")
                                         (line-beginning-position))))
                 (note (make-vulpea-note :id "w" :title "Wine" :path file
                                         :level 1 :pos wpos :tags '("wine"))))
            (unwind-protect
                (with-current-buffer buf
                  ;; scope ends at the child heading, excluding its meta
                  (should (<= (vulpea-ui--schema-note-end note) cpos))
                  ;; a missing field lands in the parent's own section, at
                  ;; or before the child boundary - never on the child's
                  ;; "- foo :: bar"
                  (let ((pos (vulpea-ui--schema-violation-position
                              (vulpea-ui-test--wine-violation note "name") note)))
                    (should (>= pos (vulpea-note-pos note)))
                    (should (<= pos cpos))
                    (goto-char pos)
                    (should-not (looking-at-p "^- foo"))))
              (kill-buffer buf))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-ui-test-schema-fix-violation-action-heading-writes ()
  "Fixing a heading note's missing field writes INTO the heading.
The vulpea-ui#356 analog: the field must land below the `* Wine'
heading and above the next heading, never at the top of the file."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (vulpea-ui-test--with-heading-note '(("colour" "blue"))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Chateau Test"))
              ((symbol-function 'vulpea-db-update-file) #'ignore)
              ((symbol-function 'vulpea-ui-sidebar-refresh) #'ignore))
      (vulpea-ui--schema-fix-violation-action
       note (vulpea-ui-test--wine-violation note "name"))
      (with-current-buffer buf
        (let* ((body (buffer-string))
               (name-at (string-match "^- name :: Chateau Test" body))
               (wine-at (string-match "^\\* Wine" body))
               (other-at (string-match "^\\* Other" body)))
          (should name-at)
          (should (> name-at wine-at))
          (should (< name-at other-at)))))))

(ert-deftest vulpea-ui-test-schema-dashboard-fix-heading-writes ()
  "Fixing a heading note's missing field from the dashboard writes INTO it."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (vulpea-ui-test--with-heading-note '(("colour" "blue"))
    (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) "Chateau Test"))
              ((symbol-function 'vulpea-db-update-file) #'ignore)
              ((symbol-function 'vulpea-ui-schema-dashboard-refresh) #'ignore))
      (vulpea-ui-schema-dashboard--fix-violation
       note (vulpea-ui-test--wine-violation note "name"))
      (with-current-buffer buf
        (let* ((body (buffer-string))
               (name-at (string-match "^- name :: Chateau Test" body))
               (wine-at (string-match "^\\* Wine" body))
               (other-at (string-match "^\\* Other" body)))
          (should name-at)
          (should (> name-at wine-at))
          (should (< name-at other-at)))))))

(ert-deftest vulpea-ui-test-schema-sidebar-tracks-file-level ()
  "The sidebar anchors on the FILE-level note, even with point on a heading.
`vulpea-ui--get-note-from-buffer' reads the file-level ID regardless of
point, so the whole-file widgets (outline, backlinks, stats) stay
file-scoped.  The schema widget is no longer limited to that one note -
it breaks the whole file down from this note's path (see the per-file
breakdown tests) - but the sidebar's anchor is still the file note."
  (vulpea-ui-test--with-heading-note '(("colour" "blue"))
    (cl-letf (((symbol-function 'vulpea-db-get-by-id)
               (lambda (id) (make-vulpea-note :id id :title id))))
      (with-current-buffer buf
        (goto-char (vulpea-note-pos note))
        (forward-line 2)
        (let ((shown (vulpea-ui--get-note-from-buffer buf)))
          (should (equal (vulpea-note-id shown) "file-1"))
          (should-not (equal (vulpea-note-id shown) "w-head")))))))

;;; Schema - per-file breakdown (file + heading notes)

;; The schema widget reports on every note in the file, not just the one
;; anchoring the sidebar.  These cover the data layer (pure), the render
;; (single vs multi-note), the heading-only-file anchor, and the keyed
;; cursor identity that keeps point put across a fix.

(defmacro vulpea-ui-test--with-wine-schema (&rest body)
  "Run BODY with a fresh registry holding a `wine' schema.
`wine' applies to notes tagged \"wine\", requires `name', constrains
`colour' to red/white."
  (declare (indent 0))
  `(let ((vulpea-schema--registry (make-hash-table :test 'eq)))
     (vulpea-schema-define 'wine
       :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
       :fields '((:key "name" :required t)
                 (:key "colour" :type symbol :one-of (red white))))
     ,@body))

(defun vulpea-ui-test--wnote (id title pos level &optional tags meta)
  "A note at POS/LEVEL tagged TAGS (default wine) with META, in /tmp/wf.org."
  (make-vulpea-note :id id :title title :path "/tmp/wf.org"
                    :level level :pos pos
                    :tags (or tags '("wine")) :meta meta))

;; --- data layer: vulpea-ui--schema-file-health (pure) -----------------

(ert-deftest vulpea-ui-test-schema-file-health-empty ()
  "No notes yields an empty, zero-count breakdown."
  (let ((h (vulpea-ui--schema-file-health nil)))
    (should-not (plist-get h :entries))
    (should-not (plist-get h :violated))
    (should (= (plist-get h :healthy-count) 0))
    (should (= (plist-get h :issue-count) 0))))

(ert-deftest vulpea-ui-test-schema-file-health-filters-non-schema ()
  "Notes no schema applies to are dropped from the breakdown."
  (vulpea-ui-test--with-wine-schema
    (let* ((wine (vulpea-ui-test--wnote "w" "Wine" 1 0 '("wine") '(("name" "A"))))
           (misc (vulpea-ui-test--wnote "m" "Misc" 40 1 '("misc")))
           (h (vulpea-ui--schema-file-health (list wine misc)))
           (entries (plist-get h :entries)))
      (should (= (length entries) 1))
      (should (equal (vulpea-note-id (plist-get (car entries) :note)) "w")))))

(ert-deftest vulpea-ui-test-schema-file-health-sorts-by-pos ()
  "Entries are ordered by buffer position: file-level note first."
  (vulpea-ui-test--with-wine-schema
    (let* ((h2 (vulpea-ui-test--wnote "h2" "White" 80 1 '("wine") nil))
           (file (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine") nil))
           (h1 (vulpea-ui-test--wnote "h1" "Red" 40 1 '("wine") nil))
           ;; Deliberately out of order on input.
           (h (vulpea-ui--schema-file-health (list h2 file h1)))
           (titles (mapcar (lambda (e) (vulpea-note-title (plist-get e :note)))
                           (plist-get h :entries))))
      (should (equal titles '("Cellar" "Red" "White"))))))

(ert-deftest vulpea-ui-test-schema-file-health-counts ()
  "Violated, healthy-count and issue-count reflect the whole file."
  (vulpea-ui-test--with-wine-schema
    (let* ((file (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine")
                                        '(("name" "C") ("colour" "red"))))
           (h1 (vulpea-ui-test--wnote "h1" "Red" 40 1 '("wine") nil)) ; miss name
           (h2 (vulpea-ui-test--wnote "h2" "Blue" 80 1 '("wine")
                                      '(("colour" "blue"))))          ; miss name + bad colour
           (h (vulpea-ui--schema-file-health (list file h1 h2))))
      (should (= (length (plist-get h :entries)) 3))
      (should (= (length (plist-get h :violated)) 2))
      (should (= (plist-get h :healthy-count) 1))
      (should (= (plist-get h :issue-count) 3))
      ;; violated stays in document order
      (should (equal (mapcar (lambda (e) (vulpea-note-title (plist-get e :note)))
                             (plist-get h :violated))
                     '("Red" "Blue"))))))

(ert-deftest vulpea-ui-test-schema-file-health-multi-schema-note ()
  "A note matching two schemas reports both, with the union of violations."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq)))
    (vulpea-schema-define 'wine
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields '((:key "name" :required t)))
    (vulpea-schema-define 'producer
      :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
      :fields '((:key "region" :required t)))
    (let* ((note (vulpea-ui-test--wnote "w" "Wine" 1 0 '("wine") nil))
           (h (vulpea-ui--schema-file-health (list note)))
           (entry (car (plist-get h :entries)))
           (schemas (plist-get entry :schemas)))
      (should (memq 'wine schemas))
      (should (memq 'producer schemas))
      (should (= (plist-get h :issue-count) 2)))))

;; --- data layer: vulpea-ui--schema-file-notes (DB wrapper) ------------

(ert-deftest vulpea-ui-test-schema-file-notes-query ()
  "The file-notes wrapper passes the path to the DB and returns its rows."
  (let ((seen nil)
        (rows (list (vulpea-ui-test--wnote "a" "A" 1 0))))
    (cl-letf (((symbol-function 'vulpea-db-query-by-file-path)
               (lambda (path &rest _) (setq seen path) rows)))
      (should (eq (vulpea-ui--schema-file-notes "/tmp/wf.org") rows))
      (should (equal seen "/tmp/wf.org"))
      ;; nil path never touches the DB
      (setq seen 'untouched)
      (should-not (vulpea-ui--schema-file-notes nil))
      (should (eq seen 'untouched)))))

;; --- render: mount the widget over a mocked file query ----------------

(vui-defcomponent vulpea-ui-test--schema-harness (note)
  "Mount only the schema-health widget under NOTE's context."
  :render
  (vulpea-ui-note-provider note
    (vui-component 'vulpea-ui-widget-schema-health)))

(defmacro vulpea-ui-test--with-schema-widget (notes anchor &rest body)
  "Mount the schema widget over NOTES (the mocked file-path query) with
ANCHOR as the sidebar note; run BODY in the widget buffer with the mount
INSTANCE bound.  A `wine' schema is registered."
  (declare (indent 2))
  `(vulpea-ui-test--with-wine-schema
     (let ((vui-render-delay nil)
           (bufname "*vulpea-ui schema widget test*"))
       (cl-letf (((symbol-function 'vulpea-db-query-by-file-path)
                  (lambda (&rest _) ,notes)))
         (unwind-protect
             (let ((instance
                    (vui-mount (vui-component 'vulpea-ui-test--schema-harness
                                              :note ,anchor)
                               bufname)))
               (ignore instance)
               (with-current-buffer bufname ,@body))
           (when (get-buffer bufname) (kill-buffer bufname)))))))

(ert-deftest vulpea-ui-test-schema-widget-single-note ()
  "One schema note in the file renders the classic per-note view."
  (let ((note (vulpea-ui-test--wnote "w" "Wine" 1 0 '("wine")
                                     '(("colour" "blue")))))
    (vulpea-ui-test--with-schema-widget (list note) note
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Schema" text))
        (should (string-match-p "wine ·" text))       ; schema-name line
        (should (string-match-p "name" text))          ; the missing field
        (should-not (string-match-p "notes ·" text)))))) ; no multi summary

(ert-deftest vulpea-ui-test-schema-widget-multi-breakdown ()
  "File + violated headings render a per-note breakdown with titles."
  (let ((file (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine")
                                     '(("name" "C") ("colour" "red"))))
        (h1 (vulpea-ui-test--wnote "h1" "Red" 40 1 '("wine") nil))
        (h2 (vulpea-ui-test--wnote "h2" "Blue" 80 1 '("wine")
                                   '(("colour" "blue")))))
    (vulpea-ui-test--with-schema-widget (list file h1 h2)
        (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine")
                               '(("name" "C") ("colour" "red")))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "2 notes · 3 issues" text))
        (should (string-match-p "Red" text))            ; violated heading title
        (should (string-match-p "Blue" text))           ; violated heading title
        (should (string-match-p "1 note healthy" text)) ; the clean file note
        (should-not (string-match-p "Cellar" text))))))  ; healthy note not expanded

(ert-deftest vulpea-ui-test-schema-widget-all-healthy ()
  "Several notes, all conformant, collapse to a healthy summary."
  (let ((file (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine")
                                     '(("name" "C"))))
        (h1 (vulpea-ui-test--wnote "h1" "Red" 40 1 '("wine")
                                   '(("name" "R")))))
    (vulpea-ui-test--with-schema-widget (list file h1)
        (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine") '(("name" "C")))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "2 notes · healthy" text))
        (should-not (string-match-p "issue" text))))))

(ert-deftest vulpea-ui-test-schema-widget-hidden-when-no-schema ()
  "A file with no schema-bearing note renders nothing."
  (let ((note (vulpea-ui-test--wnote "m" "Misc" 1 0 '("misc"))))
    (vulpea-ui-test--with-schema-widget (list note) note
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match-p "Schema" text))))))

(ert-deftest vulpea-ui-test-schema-widget-heading-titles-are-buttons ()
  "Each violated note title is a keyed jump button; violation buttons too."
  (let ((file (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine")
                                     '(("name" "C") ("colour" "red"))))
        (h1 (vulpea-ui-test--wnote "h1" "Red" 40 1 '("wine") nil))
        (h2 (vulpea-ui-test--wnote "h2" "Blue" 80 1 '("wine") nil)))
    (vulpea-ui-test--with-schema-widget (list file h1 h2)
        (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine")
                               '(("name" "C") ("colour" "red")))
      (let ((keys (mapcar (lambda (w) (vui-element-get w :vui-key))
                          (vui--collect-widgets))))
        ;; per-note title buttons
        (should (member (list 'schema-note "h1") keys))
        (should (member (list 'schema-note "h2") keys))
        ;; field + fix buttons keyed by note-id / field / type / index
        (should (member (list 'field "h1" "name" 'missing-required 0) keys))
        (when (fboundp 'vulpea-schema-fix-violation)
          (should (member (list 'fix "h1" "name" 'missing-required 0) keys)))
        ;; every key unique (what a dropped note-id or index would break)
        (let ((real (delq nil keys)))
          (should (= (length real) (length (seq-uniq real #'equal)))))))))

(ert-deftest vulpea-ui-test-schema-widget-cursor-survives-refresh ()
  "Dropping a note above point keeps point on its own fix button.
Only the note-id in the key keeps point from sliding to a same-labelled
`fix' button on another note when the rows above it disappear."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (vulpea-ui-test--with-wine-schema
    (let* ((vui-render-delay nil)
           (bufname "*vulpea-ui schema widget test*")
           (anchor (vulpea-ui-test--wnote "f" "Cellar" 1 0 '("wine")
                                          '(("name" "C") ("colour" "red"))))
           (h1 (vulpea-ui-test--wnote "h1" "Red" 40 1 '("wine") nil))
           (h2 (vulpea-ui-test--wnote "h2" "Blue" 80 1 '("wine") nil))
           (current (list anchor h1 h2)))
      (cl-letf (((symbol-function 'vulpea-db-query-by-file-path)
                 (lambda (&rest _) current)))
        (unwind-protect
            (let ((instance (vui-mount
                             (vui-component 'vulpea-ui-test--schema-harness
                                            :note anchor)
                             bufname)))
              (with-current-buffer bufname
                (let ((target (list 'fix "h2" "name" 'missing-required 0)))
                  ;; park point on h2's fix button
                  (goto-char (car (vui--widget-bounds
                                   (seq-find (lambda (w)
                                               (equal (vui-element-get w :vui-key)
                                                      target))
                                             (vui--collect-widgets)))))
                  ;; h1 vanishes; rows above h2 shrink.  Bump the refresh
                  ;; generation so the widget's memo recomputes.
                  (setq current (list anchor h2))
                  (cl-incf vulpea-ui--refresh-generation)
                  (vui-update instance (list :note anchor))
                  ;; point still on h2's fix button, not h1's old slot
                  (should (equal (vui-key-at) target)))))
          (when (get-buffer bufname) (kill-buffer bufname)))))))

;; --- note resolution: heading-only files ------------------------------

(ert-deftest vulpea-ui-test-get-note-heading-only-file ()
  "A file with no file-level ID anchors on its earliest heading note.
Heading-only files (IDs only on headings) still light up the sidebar."
  (let ((file (make-temp-file "vulpea-ui-honly-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+title: No File ID\n\n"
                    "* First :wine:\n:PROPERTIES:\n:ID: h1\n:END:\n"
                    "* Second :wine:\n:PROPERTIES:\n:ID: h2\n:END:\n"))
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (cl-letf (((symbol-function 'vulpea-db-query-by-file-path)
                           ;; return out of order to prove the sort
                           (lambda (&rest _)
                             (list (make-vulpea-note :id "h2" :title "Second"
                                                     :path file :level 1 :pos 60)
                                   (make-vulpea-note :id "h1" :title "First"
                                                     :path file :level 1 :pos 20)))))
                  (let ((note (vulpea-ui--get-note-from-buffer buf)))
                    (should note)
                    (should (equal (vulpea-note-id note) "h1")))) ; earliest by pos
              (kill-buffer buf))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-ui-test-get-note-file-level-unchanged ()
  "A file WITH a file-level ID still resolves the file note, not a heading.
The heading-only fallback must not disturb the common case."
  (let ((file (make-temp-file "vulpea-ui-fid-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: file-1\n:END:\n#+title: Has ID\n\n"
                    "* Head :wine:\n:PROPERTIES:\n:ID: h1\n:END:\n"))
          (let ((buf (find-file-noselect file))
                (queried nil))
            (unwind-protect
                (cl-letf (((symbol-function 'vulpea-db-get-by-id)
                           (lambda (id) (make-vulpea-note :id id :title id)))
                          ((symbol-function 'vulpea-db-query-by-file-path)
                           (lambda (&rest _) (setq queried t) nil)))
                  (let ((note (vulpea-ui--get-note-from-buffer buf)))
                    (should (equal (vulpea-note-id note) "file-1"))
                    (should-not queried)))  ; fallback never ran
              (kill-buffer buf))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-ui-test-get-note-file-id-db-miss ()
  "A file-level ID the DB has not indexed yet resolves to nil, not a heading.
When a file carries a file-level =:ID:= but the DB lookup misses (a fresh
or unsynced note), the resolver must return nil - never fall through to
the heading query and mis-anchor the whole-file widgets on a heading."
  (let ((file (make-temp-file "vulpea-ui-miss-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: file-1\n:END:\n#+title: Fresh\n\n"
                    "* Head :wine:\n:PROPERTIES:\n:ID: h1\n:END:\n"))
          (let ((buf (find-file-noselect file))
                (fell-through nil))
            (unwind-protect
                (cl-letf (((symbol-function 'vulpea-db-get-by-id)
                           (lambda (_id) nil)) ; DB has not caught up
                          ((symbol-function 'vulpea-db-query-by-file-path)
                           (lambda (&rest _)
                             (setq fell-through t)
                             (list (make-vulpea-note :id "h1" :title "Head"
                                                     :path file :level 1 :pos 40)))))
                  (should-not (vulpea-ui--get-note-from-buffer buf))
                  (should-not fell-through))
              (kill-buffer buf))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-ui-test-schema-violation-position-multi-value ()
  "Two disallowed values of one field resolve to their own distinct lines.
Without value-aware search both rows would jump to the first occurrence."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq))
        (file (make-temp-file "vulpea-ui-mv-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: file-1\n:END:\n#+title: J\n\n"
                    "* Wine :wine:\n:PROPERTIES:\n:ID: w\n:END:\n"
                    "- name :: X\n- colour :: blue\n- colour :: green\n"))
          (vulpea-schema-define 'wine :predicate (lambda (_n) t)
            :fields '((:key "name" :required t)
                      (:key "colour" :type symbol :one-of (red white))))
          (let* ((buf (find-file-noselect file))
                 (wpos (with-current-buffer buf
                         (save-excursion (goto-char (point-min))
                                         (re-search-forward "^\\* Wine")
                                         (line-beginning-position))))
                 (note (make-vulpea-note :id "w" :title "Wine" :path file
                                         :level 1 :pos wpos :tags '("wine")
                                         :meta '(("name" "X")
                                                 ("colour" "blue" "green"))))
                 (viols (seq-filter
                         (lambda (v) (equal (vulpea-violation-field v) "colour"))
                         (vulpea-schema-validate note 'wine)))
                 (val (lambda (v) (format "%s" (vulpea-violation-value v))))
                 (blue (cl-find "blue" viols :key val :test #'equal))
                 (green (cl-find "green" viols :key val :test #'equal)))
            (unwind-protect
                (with-current-buffer buf
                  (should (= (length viols) 2))
                  (should blue) (should green)
                  (let ((bpos (vulpea-ui--schema-violation-position blue note))
                        (gpos (vulpea-ui--schema-violation-position green note)))
                    (should-not (= bpos gpos))
                    (goto-char bpos) (should (looking-at-p "^- colour :: blue"))
                    (goto-char gpos) (should (looking-at-p "^- colour :: green"))))
              (kill-buffer buf))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-ui-test-schema-heading-at-point-max ()
  "A heading that ends the file: scope runs to `point-max', positions hold."
  (let ((vulpea-schema--registry (make-hash-table :test 'eq))
        (file (make-temp-file "vulpea-ui-eof-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert ":PROPERTIES:\n:ID: file-1\n:END:\n#+title: J\n\n"
                    "* Wine :wine:\n:PROPERTIES:\n:ID: w\n:END:\n- colour :: blue\n"))
          (vulpea-schema-define 'wine :predicate (lambda (_n) t)
            :fields '((:key "name" :required t)
                      (:key "colour" :type symbol :one-of (red white))))
          (let* ((buf (find-file-noselect file))
                 (wpos (with-current-buffer buf
                         (save-excursion (goto-char (point-min))
                                         (re-search-forward "^\\* Wine")
                                         (line-beginning-position))))
                 (note (make-vulpea-note :id "w" :title "Wine" :path file
                                         :level 1 :pos wpos :tags '("wine")
                                         :meta '(("colour" "blue")))))
            (unwind-protect
                (with-current-buffer buf
                  (should (= (vulpea-ui--schema-note-end note) (point-max)))
                  (goto-char (vulpea-ui--schema-violation-position
                              (vulpea-ui-test--wine-violation note "colour") note))
                  (should (looking-at-p "^- colour :: blue"))
                  (let ((pos (vulpea-ui--schema-violation-position
                              (vulpea-ui-test--wine-violation note "name") note)))
                    (should (>= pos wpos))
                    (should (<= pos (point-max)))))
              (kill-buffer buf))))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-ui-test-schema-widget-duplicate-titles ()
  "Two headings sharing a title stay distinct: keys are the note id, not title."
  (let ((h1 (vulpea-ui-test--wnote "id-1" "Red Wine" 40 1 '("wine") nil))
        (h2 (vulpea-ui-test--wnote "id-2" "Red Wine" 80 1 '("wine") nil)))
    (vulpea-ui-test--with-schema-widget (list h1 h2)
        (vulpea-ui-test--wnote "id-1" "Red Wine" 40 1 '("wine") nil)
      (let ((keys (mapcar (lambda (w) (vui-element-get w :vui-key))
                          (vui--collect-widgets))))
        (should (member (list 'schema-note "id-1") keys))
        (should (member (list 'schema-note "id-2") keys))
        (let ((real (delq nil keys)))
          (should (= (length real) (length (seq-uniq real #'equal)))))))))

;;; Schema - real database, end to end (the vulpea#356 shape)

;; The tests above mock the file-path query.  These index a real journal
;; file with two wine headings into a real temp DB and drive the whole
;; chain - query, health, render, fix - the way it runs in anger.

(defmacro vulpea-ui-test--with-indexed-wine-file (&rest body)
  "Index a journal file with two wine headings into a real temp DB.
Binds FILE (path), FILE-ID, RED-ID, WHITE-ID.  The file-level note is a
`journal' (matching no schema); `Red Wine' is invalid (missing name, bad
colour); `White Wine' is valid.  A `wine' schema is registered and the
DB is live for BODY, then everything is torn down."
  (declare (indent 0))
  `(let* ((vulpea-schema--registry (make-hash-table :test 'eq))
          (temp-db (make-temp-file "vulpea-ui-idb-" nil ".db"))
          (vulpea-db-location temp-db)
          (vulpea-db--connection nil)
          (file (make-temp-file "vulpea-ui-idb-" nil ".org"))
          (file-id "11111111-1111-1111-1111-111111111111")
          (red-id "22222222-2222-2222-2222-222222222222")
          (white-id "33333333-3333-3333-3333-333333333333"))
     (unwind-protect
         (progn
           (vulpea-schema-define 'wine
             :predicate (lambda (n) (member "wine" (vulpea-note-tags n)))
             :fields '((:key "name" :required t)
                       (:key "colour" :type symbol :one-of (red white))))
           (with-temp-file file
             (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n" file-id)
                     "#+title: Journal\n#+filetags: :journal:\n\n"
                     (format "* Red Wine :wine:\n:PROPERTIES:\n:ID: %s\n:END:\n"
                             red-id)
                     "- colour :: blue\n\n"
                     (format "* White Wine :wine:\n:PROPERTIES:\n:ID: %s\n:END:\n"
                             white-id)
                     "- name :: Chablis\n- colour :: white\n"))
           (vulpea-db)
           (cl-letf (((symbol-function 'lwarn) #'ignore))
             (vulpea-db-update-file file))
           ,@body)
       (when vulpea-db--connection (vulpea-db-close))
       (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
       (when (file-exists-p temp-db) (delete-file temp-db))
       (when (file-exists-p file) (delete-file file)))))

(ert-deftest vulpea-ui-test-schema-real-db-indexes-headings ()
  "The DB indexes the file-level note and both heading-level notes."
  (vulpea-ui-test--with-indexed-wine-file
    (let ((notes (vulpea-db-query-by-file-path file)))
      (should (= (length notes) 3))
      (should (equal (sort (mapcar #'vulpea-note-level notes) #'<) '(0 1 1))))))

(ert-deftest vulpea-ui-test-schema-real-db-widget-breakdown ()
  "Anchored on the journal note, the widget surfaces the heading issues.
The journal file-level note matches no schema, yet the widget - keyed on
its file, not on that one note - still reports the wine headings.  This
is the vulpea#356 shape driven through the real widget and DB."
  (vulpea-ui-test--with-indexed-wine-file
    (let ((anchor (vulpea-db-get-by-id file-id))
          (vui-render-delay nil)
          (bufname "*vulpea-ui schema widget test*"))
      (should (equal (vulpea-note-tags anchor) '("journal")))
      (unwind-protect
          (progn
            (vui-mount (vui-component 'vulpea-ui-test--schema-harness :note anchor)
                       bufname)
            (with-current-buffer bufname
              (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "1 note · 2 issues" text))
                (should (string-match-p "Red Wine" text))
                (should (string-match-p "1 note healthy" text))
                (should-not (string-match-p "White Wine" text)))))
        (when (get-buffer bufname) (kill-buffer bufname))))))

(ert-deftest vulpea-ui-test-schema-real-db-fix-into-heading ()
  "Fixing from the widget writes the field into the heading's subtree.
Real DB, real fixer: `- name ::' must land between the two headings."
  (skip-unless (fboundp 'vulpea-schema-fix-violation))
  (vulpea-ui-test--with-indexed-wine-file
    (let* ((red (vulpea-db-get-by-id red-id))
           (viol (cl-find "name" (vulpea-schema-validate red 'wine)
                          :key #'vulpea-violation-field :test #'equal)))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Chateau Real"))
                ((symbol-function 'vulpea-ui-sidebar-refresh) #'ignore)
                ((symbol-function 'lwarn) #'ignore))
        (find-file-noselect file)
        (vulpea-ui--schema-fix-violation-action red viol))
      (with-current-buffer (find-file-noselect file)
        (revert-buffer t t)
        (let* ((body (buffer-string))
               (name-at (string-match "^- name :: Chateau Real" body))
               (red-at (string-match "^\\* Red Wine" body))
               (white-at (string-match "^\\* White Wine" body)))
          (should name-at)
          (should (> name-at red-at))
          (should (< name-at white-at)))))))

(ert-deftest vulpea-ui-worker-done-refreshes-when-idle ()
  "A background extraction landing triggers a sidebar refresh.
With vulpea's async extraction, the database changes when the worker
applies results - not when the file is saved - so the sidebar must
refresh on the done hook, once the worker is idle."
  (let ((refreshed 0)
        (vulpea-ui--worker-refresh-pending nil))
    (cl-letf (((symbol-function 'vulpea-ui--sidebar-visible-p)
               (lambda (&rest _) t))
              ((symbol-function 'vulpea-ui-sidebar-refresh)
               (lambda () (cl-incf refreshed)))
              ((symbol-function 'vulpea-db-worker-busy-p)
               (lambda () nil)))
      (vulpea-ui--on-worker-done "/tmp/a.org" 'applied 3)
      (should (= 1 refreshed))
      ;; unchanged results do not change data: no refresh
      (vulpea-ui--on-worker-done "/tmp/b.org" 'unchanged nil)
      (should (= 1 refreshed)))))

(ert-deftest vulpea-ui-worker-done-coalesces-bulk-syncs ()
  "During a bulk sync, refreshes coalesce to one when the burst drains.
Refreshing per applied file during a 10k-note rebuild would be a
refresh storm; the pending flag defers to the end of the burst."
  (let ((refreshed 0)
        (busy t)
        (vulpea-ui--worker-refresh-pending nil))
    (cl-letf (((symbol-function 'vulpea-ui--sidebar-visible-p)
               (lambda (&rest _) t))
              ((symbol-function 'vulpea-ui-sidebar-refresh)
               (lambda () (cl-incf refreshed)))
              ((symbol-function 'vulpea-db-worker-busy-p)
               (lambda () busy)))
      ;; Burst in progress: applied results only mark pending
      (vulpea-ui--on-worker-done "/tmp/a.org" 'applied 1)
      (vulpea-ui--on-worker-done "/tmp/b.org" 'applied 1)
      (should (= 0 refreshed))
      (should vulpea-ui--worker-refresh-pending)
      ;; Last completion of the burst: worker idle, even an unchanged
      ;; status flushes the pending refresh
      (setq busy nil)
      (vulpea-ui--on-worker-done "/tmp/c.org" 'unchanged nil)
      (should (= 1 refreshed))
      (should-not vulpea-ui--worker-refresh-pending))))

(ert-deftest vulpea-ui-worker-done-ignores-when-sidebar-hidden ()
  "No sidebar, no refresh - but the pending flag must not leak."
  (let ((refreshed 0)
        (vulpea-ui--worker-refresh-pending nil))
    (cl-letf (((symbol-function 'vulpea-ui--sidebar-visible-p)
               (lambda (&rest _) nil))
              ((symbol-function 'vulpea-ui-sidebar-refresh)
               (lambda () (cl-incf refreshed)))
              ((symbol-function 'vulpea-db-worker-busy-p)
               (lambda () nil)))
      (vulpea-ui--on-worker-done "/tmp/a.org" 'applied 1)
      (should (= 0 refreshed))
      (should-not vulpea-ui--worker-refresh-pending))))

(ert-deftest vulpea-ui-worker-hook-registration-guarded ()
  "Hook setup registers on the worker hook only when vulpea provides it.
Keeps vulpea-ui compatible with vulpea versions without async
extraction."
  (when (boundp 'vulpea-db-worker-done-functions)
    (let ((vulpea-db-worker-done-functions nil)
          (vulpea-ui-auto-refresh t))
      (cl-letf (((symbol-function 'vulpea-ui--start-idle-timer) #'ignore))
        (vulpea-ui--setup-hooks)
        (unwind-protect
            (should (memq #'vulpea-ui--on-worker-done
                          vulpea-db-worker-done-functions))
          (vulpea-ui--teardown-hooks))
        (should-not (memq #'vulpea-ui--on-worker-done
                          vulpea-db-worker-done-functions))))))

;;; Collection view tests

(cl-defun vulpea-ui-test--collection-note
    (&key (id "n1") (title "Note") (path "/notes/n1.org") (level 0)
          (pos 1) tags meta links created-at modified-at
          todo priority scheduled deadline aliases
          file-title outline-path)
  "Create a mock note for collection tests.
ID, TITLE, PATH, LEVEL, POS, TAGS, META, LINKS, CREATED-AT,
MODIFIED-AT, TODO, PRIORITY, SCHEDULED, DEADLINE, ALIASES,
FILE-TITLE and OUTLINE-PATH map to the `vulpea-note' slots."
  (make-vulpea-note
   :id id :title title :primary-title title :path path :level level
   :pos pos :tags tags :meta meta :links links
   :created-at created-at :modified-at modified-at
   :todo todo :priority priority :scheduled scheduled
   :deadline deadline :aliases aliases
   :file-title file-title :outline-path outline-path))

(ert-deftest vulpea-ui-collection-test-matches-empty-filter ()
  "An empty filter matches any note."
  (should (vulpea-ui-collection--note-matches-p
           (vulpea-ui-test--collection-note) nil)))

(ert-deftest vulpea-ui-collection-test-matches-tags ()
  "Tags conditions: all, any, none."
  (let ((note (vulpea-ui-test--collection-note :tags '("wine" "france"))))
    (should (vulpea-ui-collection--note-matches-p
             note '(:tags-all ("wine"))))
    (should (vulpea-ui-collection--note-matches-p
             note '(:tags-all ("wine" "france"))))
    (should-not (vulpea-ui-collection--note-matches-p
                 note '(:tags-all ("wine" "italy"))))
    (should (vulpea-ui-collection--note-matches-p
             note '(:tags-any ("beer" "wine"))))
    (should-not (vulpea-ui-collection--note-matches-p
                 note '(:tags-any ("beer"))))
    (should (vulpea-ui-collection--note-matches-p
             note '(:tags-none ("beer"))))
    (should-not (vulpea-ui-collection--note-matches-p
                 note '(:tags-none ("wine"))))))

(ert-deftest vulpea-ui-collection-test-matches-level ()
  "Level condition distinguishes file and heading notes."
  (let ((file-note (vulpea-ui-test--collection-note :level 0))
        (head-note (vulpea-ui-test--collection-note :level 2)))
    (should (vulpea-ui-collection--note-matches-p file-note '(:level 0)))
    (should-not (vulpea-ui-collection--note-matches-p head-note '(:level 0)))
    (should (vulpea-ui-collection--note-matches-p head-note '(:level 2)))))

(ert-deftest vulpea-ui-collection-test-matches-directory ()
  "Directory condition: absolute prefix and relative component."
  (let ((note (vulpea-ui-test--collection-note :path "/notes/wine/a.org")))
    (should (vulpea-ui-collection--note-matches-p
             note '(:directory "/notes/wine")))
    (should-not (vulpea-ui-collection--note-matches-p
                 note '(:directory "/notes/beer")))
    (should (vulpea-ui-collection--note-matches-p
             note '(:directory "wine")))
    (should-not (vulpea-ui-collection--note-matches-p
                 note '(:directory "beer")))))

(ert-deftest vulpea-ui-collection-test-matches-meta ()
  "Meta conditions: value equality and key presence."
  (let ((note (vulpea-ui-test--collection-note
               :meta '(("country" "France") ("grapes" "Syrah" "Viognier")))))
    (should (vulpea-ui-collection--note-matches-p
             note '(:meta (("country" . "France")))))
    (should (vulpea-ui-collection--note-matches-p
             note '(:meta (("grapes" . "Syrah")))))
    (should-not (vulpea-ui-collection--note-matches-p
                 note '(:meta (("country" . "Italy")))))
    (should (vulpea-ui-collection--note-matches-p
             note '(:meta (("country" . t)))))
    (should-not (vulpea-ui-collection--note-matches-p
                 note '(:meta (("region" . t)))))))

(ert-deftest vulpea-ui-collection-test-matches-title-and-predicate ()
  "Title regexp and custom predicate conditions."
  (let ((note (vulpea-ui-test--collection-note :title "Chateauneuf")))
    (should (vulpea-ui-collection--note-matches-p note '(:title "^Chateau")))
    (should-not (vulpea-ui-collection--note-matches-p note '(:title "^Bord")))
    (should (vulpea-ui-collection--note-matches-p
             note (list :predicate (lambda (n)
                                     (string-prefix-p "Ch" (vulpea-note-title n))))))
    (should-not (vulpea-ui-collection--note-matches-p
                 note (list :predicate #'ignore)))))

(ert-deftest vulpea-ui-collection-test-query-dispatch ()
  "The query uses the cheapest DB entry point, then filters in memory."
  (let ((wine (vulpea-ui-test--collection-note
               :id "w" :tags '("wine" "rated")))
        (beer (vulpea-ui-test--collection-note
               :id "b" :tags '("beer"))))
    (cl-letf (((symbol-function 'vulpea-db-query-by-tags-every)
               (lambda (_tags) (list wine beer))))
      (should (equal (list wine)
                     (vulpea-ui-collection--query
                      '(:tags-all ("wine") :tags-none ("beer"))))))
    (cl-letf (((symbol-function 'vulpea-db-query)
               (lambda (&optional _pred) (list wine beer))))
      (should (equal (list wine beer)
                     (vulpea-ui-collection--query nil))))))

(ert-deftest vulpea-ui-collection-test-normalize-column ()
  "Column descriptors normalize to plists with name and width."
  (let ((col (vulpea-ui-collection--normalize-column 'title)))
    (should (eq (plist-get col :id) 'title))
    (should (equal (plist-get col :name) "Title"))
    (should (numberp (plist-get col :width))))
  (let ((col (vulpea-ui-collection--normalize-column '(meta "country"))))
    (should (eq (plist-get col :id) 'meta))
    (should (equal (plist-get col :key) "country"))
    (should (equal (plist-get col :name) "country")))
  (let ((col (vulpea-ui-collection--normalize-column
              '(title :name "T" :width 60))))
    (should (equal (plist-get col :name) "T"))
    (should (= (plist-get col :width) 60))))

(ert-deftest vulpea-ui-collection-test-column-value ()
  "Column values render note slots as strings."
  (let ((note (vulpea-ui-test--collection-note
               :title "Note" :tags '("a" "b")
               :meta '(("country" "France" "Italy"))
               :links '((:dest "x" :type "id") (:dest "y" :type "id"))
               :created-at (encode-time 0 0 12 15 3 2025)
               :path "/notes/wine/n1.org")))
    (cl-flet ((value (col &optional ctx)
                (vulpea-ui-collection--column-value
                 note (vulpea-ui-collection--normalize-column col) ctx)))
      (should (equal (value 'title) "Note"))
      (should (equal (value 'tags) "a b"))
      (should (equal (value '(meta "country")) "France, Italy"))
      (should (equal (value '(meta "region")) ""))
      (should (equal (value 'links) "2"))
      (should (equal (value 'created) "2025-03-15"))
      (should (equal (value 'modified) ""))
      (should (equal (value 'file) "n1.org"))
      (let ((counts (make-hash-table :test 'equal)))
        (puthash "n1" 3 counts)
        (should (equal (value 'backlinks (list :backlinks counts)) "3"))
        (should (equal (value 'backlinks (list :backlinks (make-hash-table :test 'equal)))
                       "0"))))))

(ert-deftest vulpea-ui-collection-test-context-and-task-columns ()
  "Context, todo, priority, scheduled, deadline and aliases columns."
  (cl-flet ((value (note col)
              (vulpea-ui-collection--column-value
               note (vulpea-ui-collection--normalize-column col) nil)))
    (let ((heading (vulpea-ui-test--collection-note
                    :id "h1" :title "Tasks" :level 2
                    :file-title "Barberry Garden"
                    :outline-path '("Environment")
                    :todo "TODO" :priority ?A
                    :scheduled "<2026-07-10 Fri>"
                    :deadline "[2026-08-01 Sat]"
                    :aliases '("bg tasks")))
          (file-note (vulpea-ui-test--collection-note :id "f1")))
      (should (equal (value heading 'context) "Barberry Garden > Environment"))
      (should (equal (value file-note 'context) ""))
      (should (equal (value heading 'todo) "TODO"))
      (should (equal (value file-note 'todo) ""))
      (should (equal (value heading 'priority) "A"))
      (should (equal (value heading 'scheduled) "2026-07-10"))
      (should (equal (value heading 'deadline) "2026-08-01"))
      (should (equal (value heading 'aliases) "bg tasks")))))

(ert-deftest vulpea-ui-collection-test-column-faces ()
  "Secondary columns carry muted faces; the title stays plain."
  (cl-flet ((value (note col)
              (vulpea-ui-collection--column-value
               note (vulpea-ui-collection--normalize-column col) nil)))
    (let ((note (vulpea-ui-test--collection-note
                 :tags '("wine") :todo "TODO"
                 :modified-at (encode-time 0 0 12 15 3 2025))))
      (should (eq (get-text-property 0 'face (value note 'tags))
                  'vulpea-ui-collection-tags-face))
      (should (eq (get-text-property 0 'face (value note 'modified))
                  'vulpea-ui-collection-date-face))
      (should (eq (get-text-property 0 'face (value note 'todo))
                  'vulpea-ui-collection-todo-face))
      (should-not (get-text-property 0 'face (value note 'title))))))

(ert-deftest vulpea-ui-collection-test-marked-row-face ()
  "Marking adds the marked face to every cell of the row; unmarking removes it."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (vulpea-ui-collection-mark)
    (goto-char (point-min))
    (let ((title-cell (aref (tabulated-list-get-entry) 1)))
      (should (memq 'vulpea-ui-collection-marked-face
                    (ensure-list (get-text-property 0 'face title-cell)))))
    (vulpea-ui-collection-unmark)
    (goto-char (point-min))
    (let ((title-cell (aref (tabulated-list-get-entry) 1)))
      (should-not (memq 'vulpea-ui-collection-marked-face
                        (ensure-list
                         (get-text-property 0 'face title-cell)))))))

(ert-deftest vulpea-ui-collection-test-format ()
  "The tabulated-list format has a mark column plus one per descriptor."
  (let ((format (vulpea-ui-collection--format '(title tags))))
    (should (= (length format) 3))
    (should (equal (aref format 0) '("" 1 nil)))
    (should (equal (car (aref format 1)) "Title"))
    (should (equal (car (aref format 2)) "Tags"))))

(ert-deftest vulpea-ui-collection-test-auto-fit-widths ()
  "Widths fit the content, capped at the default; explicit widths win."
  (let ((marked (make-hash-table :test 'equal)))
    (let* ((entries (vulpea-ui-collection--entries
                     (list (vulpea-ui-test--collection-note :title "Short"))
                     '(title) marked nil))
           (format (vulpea-ui-collection--format '(title) entries)))
      (should (< (nth 1 (aref format 1)) 48))
      (should (>= (nth 1 (aref format 1)) (length "Title"))))
    (let* ((entries (vulpea-ui-collection--entries
                     (list (vulpea-ui-test--collection-note
                            :title (make-string 100 ?x)))
                     '(title) marked nil))
           (format (vulpea-ui-collection--format '(title) entries)))
      (should (= (nth 1 (aref format 1)) 48))
      (should (= (nth 1 (aref (vulpea-ui-collection--format
                               '((title :width 60)) entries)
                              1))
                 60)))))

(ert-deftest vulpea-ui-collection-test-user-width-survives-refresh ()
  "A hand-resized column keeps its width across format updates."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (vulpea-ui-collection--update-format '(title))
    (let ((auto (nth 1 (aref tabulated-list-format 1))))
      (setf (nth 1 (aref tabulated-list-format 1)) (+ auto 7))
      (vulpea-ui-collection--update-format '(title))
      (should (= (nth 1 (aref tabulated-list-format 1)) (+ auto 7))))))

(ert-deftest vulpea-ui-collection-test-empty-state ()
  "An empty result shows a hint instead of a blank buffer."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (cl-letf (((symbol-function 'vulpea-ui-collection--query)
               (lambda (_filter) nil)))
      (vulpea-ui-collection-refresh)
      (should (string-match-p "No notes match" (buffer-string))))))

(ert-deftest vulpea-ui-collection-test-entries ()
  "Entries are keyed by note id and reflect the marked set."
  (let* ((n1 (vulpea-ui-test--collection-note :id "n1" :title "One"))
         (n2 (vulpea-ui-test--collection-note :id "n2" :title "Two"))
         (marked (make-hash-table :test 'equal)))
    (puthash "n2" t marked)
    (let ((entries (vulpea-ui-collection--entries
                    (list n1 n2) '(title) marked nil)))
      (should (equal (mapcar #'car entries) '("n1" "n2")))
      (should (equal (aref (cadr (car entries)) 0) " "))
      (should (equal (aref (cadr (cadr entries)) 0) "*"))
      (should (equal (aref (cadr (car entries)) 1) "One")))))

(ert-deftest vulpea-ui-collection-test-resolve-view ()
  "View resolution prefers saved views, falls back to a tags query."
  (let ((vulpea-ui-collection-views
         '(("wines" . (:filter (:tags-all ("wine"))
                       :columns (title tags))))))
    (let ((view (vulpea-ui-collection--resolve-view "wines")))
      (should (equal (plist-get view :name) "wines"))
      (should (equal (plist-get view :filter) '(:tags-all ("wine")))))
    (let ((view (vulpea-ui-collection--resolve-view "beer, ale")))
      (should (equal (plist-get (plist-get view :filter) :tags-all)
                     '("beer" "ale"))))))

(ert-deftest vulpea-ui-collection-test-resolve-view-empty ()
  "Empty or blank input resolves to a view over all notes."
  (dolist (input '("" "   "))
    (let ((view (vulpea-ui-collection--resolve-view input)))
      (should (equal (plist-get view :name) "all"))
      (should-not (plist-get view :filter)))))

(ert-deftest vulpea-ui-collection-test-filter-description ()
  "The filter summary is short and covers every condition kind."
  (should (equal (vulpea-ui-collection--filter-description nil) ""))
  (should (equal (vulpea-ui-collection--filter-description
                  '(:tags-all ("wine")
                    :tags-any ("red" "white")
                    :tags-none ("beer")
                    :level 0
                    :directory "cellar"
                    :title "^Ch"
                    :meta (("country" . "France") ("rating" . t))))
                 (concat "#wine #red? #white? -#beer level:0 dir:cellar"
                         " title:^Ch country:France rating:*"))))

(defmacro vulpea-ui-test--with-collection-buffer (notes &rest body)
  "Run BODY in a collection buffer populated with NOTES, no DB access."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((notes ,notes))
       (vulpea-ui-collection-mode)
       (setq-local vulpea-ui-collection--view '(:name "test" :columns (title)))
       (dolist (n notes)
         (puthash (vulpea-note-id n) n vulpea-ui-collection--note-table))
       (setq tabulated-list-format (vulpea-ui-collection--format '(title)))
       (setq tabulated-list-entries
             (vulpea-ui-collection--entries
              notes '(title) vulpea-ui-collection--marked nil))
       (tabulated-list-init-header)
       (tabulated-list-print)
       (goto-char (point-min))
       ,@body)))

(ert-deftest vulpea-ui-collection-test-mark-unmark ()
  "Marking updates the marked set and the mark cell; unmark-all clears."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    (vulpea-ui-collection-mark)
    (should (gethash "n1" vulpea-ui-collection--marked))
    ;; mark moves to the next line
    (should (equal (tabulated-list-get-id) "n2"))
    (goto-char (point-min))
    (should (equal (aref (tabulated-list-get-entry) 0) "*"))
    (vulpea-ui-collection-unmark)
    (should-not (gethash "n1" vulpea-ui-collection--marked))
    (goto-char (point-min))
    (vulpea-ui-collection-mark)
    (vulpea-ui-collection-unmark-all)
    (should (= 0 (hash-table-count vulpea-ui-collection--marked)))))

(ert-deftest vulpea-ui-collection-test-mark-updates-single-row ()
  "Marking rewrites only the row at point, leaving other lines alone."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    (let ((second-line (save-excursion
                         (goto-char (point-min))
                         (forward-line 1)
                         (buffer-substring (point) (line-end-position)))))
      (vulpea-ui-collection-mark)
      (goto-char (point-min))
      (should (string-prefix-p "*" (buffer-substring
                                    (point) (line-end-position))))
      (forward-line 1)
      (should (equal (buffer-substring (point) (line-end-position))
                     second-line)))))

(ert-deftest vulpea-ui-collection-test-mark-no-full-reprint ()
  "A single mark must not re-print the whole table."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    (cl-letf (((symbol-function 'tabulated-list-print)
               (lambda (&rest _) (error "Full re-print on mark"))))
      (vulpea-ui-collection-mark)
      (goto-char (point-min))
      (vulpea-ui-collection-unmark))
    (goto-char (point-min))
    (should (equal (aref (tabulated-list-get-entry) 0) " "))))

(ert-deftest vulpea-ui-collection-test-toggle-marks ()
  "Inverting marks flips every row."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    (vulpea-ui-collection-mark)
    (vulpea-ui-collection-toggle-marks)
    (should-not (gethash "n1" vulpea-ui-collection--marked))
    (should (gethash "n2" vulpea-ui-collection--marked))))

(ert-deftest vulpea-ui-collection-test-mark-region ()
  "With an active region, m marks every row the region touches."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two")
            (vulpea-ui-test--collection-note :id "n3" :title "Three"))
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 2)
      (vulpea-ui-collection-mark))
    (should (gethash "n1" vulpea-ui-collection--marked))
    (should (gethash "n2" vulpea-ui-collection--marked))
    (should-not (gethash "n3" vulpea-ui-collection--marked))))

(ert-deftest vulpea-ui-collection-test-mark-regexp-and-tag ()
  "Regexp marking matches titles; tag marking matches tags."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note
             :id "n1" :title "Wine note" :tags '("wine"))
            (vulpea-ui-test--collection-note
             :id "n2" :title "Beer note" :tags '("beer")))
    (vulpea-ui-collection-mark-regexp "^Wine")
    (should (gethash "n1" vulpea-ui-collection--marked))
    (should-not (gethash "n2" vulpea-ui-collection--marked))
    (vulpea-ui-collection-unmark-all)
    (vulpea-ui-collection-mark-by-tag "beer")
    (should (gethash "n2" vulpea-ui-collection--marked))
    (should-not (gethash "n1" vulpea-ui-collection--marked))))

(ert-deftest vulpea-ui-collection-test-notes-for-action ()
  "Actions target marked notes, falling back to the note at point."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    (should (equal (mapcar #'vulpea-note-id
                           (vulpea-ui-collection--notes-for-action))
                   '("n1")))
    (vulpea-ui-collection-mark)
    (vulpea-ui-collection-mark)
    (should (equal (sort (mapcar #'vulpea-note-id
                                 (vulpea-ui-collection--notes-for-action))
                         #'string<)
                   '("n1" "n2")))))

(ert-deftest vulpea-ui-collection-test-add-tag-action ()
  "Adding a tag batches over the selection and refreshes."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    (vulpea-ui-collection-mark)
    (vulpea-ui-collection-mark)
    (let (batched)
      (cl-letf (((symbol-function 'vulpea-tags-batch-add)
                 (lambda (notes tag) (setq batched (cons tag notes))))
                ((symbol-function 'vulpea-ui-collection-refresh) #'ignore)
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'vulpea-ui-collection--read-tag)
                 (lambda (_prompt) "rated")))
        (vulpea-ui-collection-add-tag)
        (should (equal (car batched) "rated"))
        (should (= (length (cdr batched)) 2))))))

(ert-deftest vulpea-ui-collection-test-bulk-edit-aborts ()
  "A declined confirmation leaves the notes untouched.
Edits on more than one note ask first, listing the affected notes;
single-note edits apply without asking."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    (vulpea-ui-collection-mark)
    (vulpea-ui-collection-mark)
    (let (batched prompt)
      (cl-letf (((symbol-function 'vulpea-tags-batch-add)
                 (lambda (notes tag) (setq batched (cons tag notes))))
                ((symbol-function 'vulpea-ui-collection-refresh) #'ignore)
                ((symbol-function 'yes-or-no-p)
                 (lambda (question) (setq prompt question) nil))
                ((symbol-function 'vulpea-ui-collection--read-tag)
                 (lambda (_prompt) "rated")))
        (vulpea-ui-collection-add-tag)
        (should-not batched)
        (should (string-match-p "2 note" prompt))
        (should (string-match-p "One" prompt))))))

(ert-deftest vulpea-ui-collection-test-delete-action ()
  "Delete removes files of file-level notes after confirmation."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note
             :id "n1" :title "One" :path "/notes/one.org" :level 0)
            (vulpea-ui-test--collection-note
             :id "n2" :title "Two" :path "/notes/two.org" :level 2))
    (vulpea-ui-collection-mark)
    (vulpea-ui-collection-mark)
    (let (deleted)
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (path &optional _trash) (push path deleted)))
                ((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'vulpea-ui-collection-refresh) #'ignore)
                ((symbol-function 'vulpea-db-sync--handle-removed-file) #'ignore))
        (vulpea-ui-collection-delete)
        ;; only the file-level note is deleted; heading notes are skipped
        (should (equal deleted '("/notes/one.org")))))))

(ert-deftest vulpea-ui-collection-test-delete-aborts ()
  "Delete does nothing when the user does not confirm."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note
             :id "n1" :title "One" :path "/notes/one.org"))
    (let (deleted)
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (path &optional _trash) (push path deleted)))
                ((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (vulpea-ui-collection-delete)
        (should-not deleted)))))

(ert-deftest vulpea-ui-collection-test-set-meta-action ()
  "Setting meta batches key and value over the selection."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (let (batched)
      (cl-letf (((symbol-function 'vulpea-meta-batch-set)
                 (lambda (notes key value) (setq batched (list notes key value))))
                ((symbol-function 'vulpea-ui-collection-refresh) #'ignore)
                ((symbol-function 'vulpea-ui-collection--read-meta-key)
                 (lambda (_prompt) "country"))
                ((symbol-function 'read-string)
                 (lambda (&rest _) "France")))
        (vulpea-ui-collection-set-meta)
        (should (equal (nth 1 batched) "country"))
        (should (equal (nth 2 batched) "France"))
        (should (= (length (nth 0 batched)) 1))))))

(ert-deftest vulpea-ui-collection-test-copy-links ()
  "Copying links puts an org list of the selection on the kill ring."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    ;; nothing marked: the whole view
    (vulpea-ui-collection-copy-links)
    (let ((copied (current-kill 0)))
      (should (string-match-p "\\[\\[id:n1\\]\\[One\\]\\]" copied))
      (should (string-match-p "\\[\\[id:n2\\]\\[Two\\]\\]" copied)))
    ;; marked subset wins
    (goto-char (point-min))
    (vulpea-ui-collection-mark)
    (vulpea-ui-collection-copy-links)
    (let ((copied (current-kill 0)))
      (should (string-match-p "id:n1" copied))
      (should-not (string-match-p "id:n2" copied)))))

(ert-deftest vulpea-ui-collection-test-export ()
  "Export produces an org buffer listing the selection as links."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (setq vulpea-ui-collection--view '(:name "test view" :columns (title)))
    (unwind-protect
        (progn
          (vulpea-ui-collection-export)
          (should (derived-mode-p 'org-mode))
          (should (string-match-p "\\[\\[id:n1\\]\\[One\\]\\]"
                                  (buffer-string))))
      (when (derived-mode-p 'org-mode)
        (kill-buffer)))))

(ert-deftest vulpea-ui-collection-test-apply-action ()
  "The custom action calls the chosen function with the selection."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (let (captured)
      (cl-letf (((symbol-function 'vulpea-ui-collection-refresh) #'ignore)
                ((symbol-function 'vulpea-ui-collection--read-function)
                 (lambda (_prompt) (lambda (notes) (setq captured notes)))))
        (vulpea-ui-collection-apply)
        (should (= (length captured) 1))))))

(ert-deftest vulpea-ui-collection-test-preview ()
  "Preview displays the note's buffer without selecting its window."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note
             :id "n1" :title "One" :path "/notes/one.org" :pos 5))
    (let (displayed (table-buffer (current-buffer)))
      (cl-letf (((symbol-function 'find-file-noselect)
                 (lambda (path &rest _)
                   (setq displayed path)
                   (generate-new-buffer " *stub-note*")))
                ((symbol-function 'display-buffer)
                 (lambda (buffer &rest _) (get-buffer-window buffer t))))
        (unwind-protect
            (progn
              (vulpea-ui-collection-preview)
              (should (equal displayed "/notes/one.org"))
              (should (eq (current-buffer) table-buffer)))
          (when-let* ((buf (get-buffer " *stub-note*")))
            (kill-buffer buf)))))))

(ert-deftest vulpea-ui-collection-test-marks-survive-sort ()
  "Marks live in entry vectors, so re-printing keeps them."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "B")
            (vulpea-ui-test--collection-note :id "n2" :title "A"))
    (vulpea-ui-collection-mark)
    (setq tabulated-list-sort-key '("Title" . nil))
    (tabulated-list-print)
    (goto-char (point-min))
    ;; sorted by title: "A" (n2) first, unmarked; "B" (n1) second, marked
    (should (equal (tabulated-list-get-id) "n2"))
    (should (equal (aref (tabulated-list-get-entry) 0) " "))
    (forward-line 1)
    (should (equal (tabulated-list-get-id) "n1"))
    (should (equal (aref (tabulated-list-get-entry) 0) "*"))))

(ert-deftest vulpea-ui-collection-test-filter-helpers ()
  "Filter helpers return updated copies without mutating the original."
  (let ((filter '(:tags-all ("wine"))))
    (let ((updated (vulpea-ui-collection--filter-put filter :level 0)))
      (should (= (plist-get updated :level) 0))
      (should-not (plist-get filter :level)))
    (let ((updated (vulpea-ui-collection--filter-add-to
                    filter :tags-all "rated")))
      (should (equal (plist-get updated :tags-all) '("wine" "rated")))
      (should (equal (plist-get filter :tags-all) '("wine"))))
    (should (equal (plist-get (vulpea-ui-collection--filter-add-to
                               nil :tags-none "beer")
                              :tags-none)
                   '("beer")))))

(ert-deftest vulpea-ui-collection-test-filter-commands ()
  "Filter commands update the view's filter and re-query."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (let ((refreshed 0))
      (cl-letf (((symbol-function 'vulpea-ui-collection-refresh)
                 (lambda () (cl-incf refreshed)))
                ((symbol-function 'vulpea-ui-collection--read-tag)
                 (lambda (_prompt) "wine")))
        (vulpea-ui-collection-filter-by-tag)
        (vulpea-ui-collection-filter-exclude-tag)
        (should (equal (plist-get
                        (plist-get vulpea-ui-collection--view :filter)
                        :tags-all)
                       '("wine")))
        (should (equal (plist-get
                        (plist-get vulpea-ui-collection--view :filter)
                        :tags-none)
                       '("wine")))
        (should (= refreshed 2))
        (vulpea-ui-collection-filter-clear)
        (should-not (plist-get vulpea-ui-collection--view :filter))
        (should (= refreshed 3))))))

(ert-deftest vulpea-ui-collection-test-filter-remove ()
  "Single conditions are removed from list, meta and scalar keys."
  (let ((filter '(:tags-all ("wine" "rated")
                  :level 0
                  :meta (("country" . "France") ("rating" . t)))))
    (should (equal (plist-get (vulpea-ui-collection--filter-remove
                               filter :tags-all "wine")
                              :tags-all)
                   '("rated")))
    (should-not (plist-get (vulpea-ui-collection--filter-remove
                            filter :level)
                           :level))
    (should (equal (plist-get (vulpea-ui-collection--filter-remove
                               filter :meta '("country" . "France"))
                              :meta)
                   '(("rating" . t))))
    ;; the original is untouched
    (should (equal (plist-get filter :tags-all) '("wine" "rated")))))

(ert-deftest vulpea-ui-collection-test-filter-remove-command ()
  "The command completes over active conditions and removes the pick."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (setq vulpea-ui-collection--view
          '(:name "test" :columns (title)
            :filter (:tags-all ("wine" "rated") :level 0)))
    (cl-letf (((symbol-function 'vulpea-ui-collection-refresh) #'ignore)
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member "#wine" collection))
                 (should (member "level:0" collection))
                 "#wine")))
      (vulpea-ui-collection-filter-remove-condition)
      (should (equal (plist-get
                      (plist-get vulpea-ui-collection--view :filter)
                      :tags-all)
                     '("rated"))))))

(ert-deftest vulpea-ui-collection-test-filter-by-level-and-title ()
  "Level and title filter commands parse their input."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (cl-letf (((symbol-function 'vulpea-ui-collection-refresh) #'ignore))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "0")))
        (vulpea-ui-collection-filter-by-level)
        (should (= (plist-get
                    (plist-get vulpea-ui-collection--view :filter)
                    :level)
                   0)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "any")))
        (vulpea-ui-collection-filter-by-level)
        (should-not (plist-get
                     (plist-get vulpea-ui-collection--view :filter)
                     :level)))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "^Ch")))
        (vulpea-ui-collection-filter-by-title)
        (should (equal (plist-get
                        (plist-get vulpea-ui-collection--view :filter)
                        :title)
                       "^Ch"))))))

(ert-deftest vulpea-ui-collection-test-available-columns ()
  "Available columns include the built-ins and meta keys in the view."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note
             :id "n1" :title "One" :meta '(("country" "France"))))
    (let ((available (vulpea-ui-collection--available-columns)))
      (should (assoc "tags" available))
      (should (equal (cdr (assoc "meta:country" available))
                     '(meta "country"))))))

(ert-deftest vulpea-ui-collection-test-add-remove-column ()
  "Adding and removing columns updates the view and the format."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note
             :id "n1" :title "One" :meta '(("country" "France"))))
    (cl-letf (((symbol-function 'vulpea-ui-collection-refresh) #'ignore))
      (vulpea-ui-collection-add-column '(meta "country"))
      (should (equal (plist-get vulpea-ui-collection--view :columns)
                     '(title (meta "country"))))
      (should (= (length tabulated-list-format) 3))
      (setq tabulated-list-sort-key '("Title" . nil))
      (vulpea-ui-collection-remove-column "Title")
      (should (equal (plist-get vulpea-ui-collection--view :columns)
                     '((meta "country"))))
      ;; the sort key pointed at the removed column
      (should-not tabulated-list-sort-key))))

(ert-deftest vulpea-ui-collection-test-mode-line-info ()
  "The mode line shows note count, marked count and the filter."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One")
            (vulpea-ui-test--collection-note :id "n2" :title "Two"))
    (setq vulpea-ui-collection--view
          '(:name "test" :columns (title) :filter (:tags-all ("wine"))))
    (let ((info (vulpea-ui-collection--mode-line-info)))
      (should (string-match-p "2" info))
      (should-not (string-match-p "marked" info))
      (should (string-match-p "#wine" info)))
    (vulpea-ui-collection-mark)
    (should (string-match-p "1 marked"
                            (vulpea-ui-collection--mode-line-info)))))

(ert-deftest vulpea-ui-collection-test-menu ()
  "The menu is a transient prefix reachable on ?."
  (should (fboundp 'vulpea-ui-collection-menu))
  (should (eq (lookup-key vulpea-ui-collection-mode-map (kbd "?"))
              #'vulpea-ui-collection-menu)))

(ert-deftest vulpea-ui-collection-test-menu-description ()
  "The menu headline names the view and describes the filter."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (should (string-match-p "none (all notes)"
                            (vulpea-ui-collection--menu-description)))
    (setq vulpea-ui-collection--view
          '(:name "wines" :columns (title) :filter (:tags-all ("wine"))))
    (let ((description (vulpea-ui-collection--menu-description)))
      (should (string-match-p "wines" description))
      (should (string-match-p "#wine" description)))))

(ert-deftest vulpea-ui-collection-test-stale-refresh ()
  "Hidden collection buffers go stale on DB changes and refresh on display."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (let ((refreshed 0)
          (vulpea-ui-collection--worker-refresh-pending nil))
      (cl-letf (((symbol-function 'vulpea-ui-collection-refresh)
                 (lambda () (cl-incf refreshed)))
                ((symbol-function 'vulpea-db-worker-busy-p) #'ignore))
        ;; the temp buffer is not displayed: no refresh, marked stale
        (vulpea-ui-collection--on-worker-done "/tmp/a.org" 'applied 1)
        (should (= refreshed 0))
        (should vulpea-ui-collection--stale)
        ;; displaying it flushes the pending refresh
        (vulpea-ui-collection--on-displayed nil)
        (should (= refreshed 1))
        (should-not vulpea-ui-collection--stale)
        ;; a second display does nothing
        (vulpea-ui-collection--on-displayed nil)
        (should (= refreshed 1))))))

(ert-deftest vulpea-ui-collection-test-bookmark ()
  "Bookmarks capture the view (minus the predicate) and restore it."
  (vulpea-ui-test--with-collection-buffer
      (list (vulpea-ui-test--collection-note :id "n1" :title "One"))
    (setq vulpea-ui-collection--view
          (list :name "wines" :columns '(title)
                :filter (list :tags-all '("wine") :predicate #'ignore)))
    (let ((record (vulpea-ui-collection--bookmark-record)))
      (should (string-match-p "wines" (car record)))
      (let ((view (alist-get 'view (cdr record))))
        (should (equal (plist-get (plist-get view :filter) :tags-all)
                       '("wine")))
        (should-not (plist-get (plist-get view :filter) :predicate)))
      (let (opened)
        (cl-letf (((symbol-function 'vulpea-ui-collection-open)
                   (lambda (view) (setq opened view))))
          (vulpea-ui-collection-bookmark-handler record)
          (should (equal (plist-get opened :name) "wines")))))))

(ert-deftest vulpea-ui-collection-test-format-time ()
  "Time formatting handles nil, time values, and strings."
  (should (equal (vulpea-ui-collection--format-time nil) ""))
  (should (equal (vulpea-ui-collection--format-time
                  (encode-time 0 0 12 15 3 2025))
                 "2025-03-15"))
  (should (equal (vulpea-ui-collection--format-time "2025-01-02T10:00:00")
                 "2025-01-02")))

(provide 'vulpea-ui-test)
;;; vulpea-ui-test.el ends here
