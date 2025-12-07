;;; vulpea-ui-test.el --- Tests for vulpea-ui -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>

;;; Commentary:

;; ERT tests for vulpea-ui sidebar and widget functionality.

;;; Code:

(require 'ert)
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

(defun vulpea-ui-test--make-mock-note (&optional id title)
  "Create a mock vulpea-note struct with ID and TITLE."
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
     :properties nil
     :meta nil)))


;;; Configuration tests

(ert-deftest vulpea-ui-test-default-position ()
  "Test that default sidebar position is 'right."
  (should (eq vulpea-ui-sidebar-position 'right)))

(ert-deftest vulpea-ui-test-default-size ()
  "Test that default sidebar size is 0.33."
  (should (= vulpea-ui-sidebar-size 0.33)))

(ert-deftest vulpea-ui-test-default-widgets ()
  "Test that default widgets list contains expected widgets."
  (should (memq 'vulpea-ui-widget-outline vulpea-ui-sidebar-widgets))
  (should (memq 'vulpea-ui-widget-backlinks vulpea-ui-sidebar-widgets))
  (should (memq 'vulpea-ui-widget-links vulpea-ui-sidebar-widgets))
  (should (memq 'vulpea-ui-widget-stats vulpea-ui-sidebar-widgets)))

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

(ert-deftest vulpea-ui-test-compute-stats-nonexistent ()
  "Test stats computation with non-existent file."
  (let ((note (vulpea-ui-test--make-mock-note)))
    (let ((stats (vulpea-ui--compute-stats note)))
      (should (= (plist-get stats :chars) 0))
      (should (= (plist-get stats :words) 0))
      (should (= (plist-get stats :links) 0)))))


;;; Note preview tests

(ert-deftest vulpea-ui-test-get-preview-nil ()
  "Test preview generation with nil note."
  (should (null (vulpea-ui--get-note-preview nil 10 t t))))

(ert-deftest vulpea-ui-test-get-preview-nonexistent ()
  "Test preview generation with non-existent file."
  (let ((note (vulpea-ui-test--make-mock-note)))
    (should (null (vulpea-ui--get-note-preview note 10 t t)))))


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


;;; Mode tests

(ert-deftest vulpea-ui-test-mode-keymap ()
  "Test that sidebar mode has expected keybindings."
  (should (eq (lookup-key vulpea-ui-sidebar-mode-map (kbd "q"))
              'vulpea-ui-sidebar-close))
  (should (eq (lookup-key vulpea-ui-sidebar-mode-map (kbd "g"))
              'vulpea-ui-sidebar-refresh))
  (should (eq (lookup-key vulpea-ui-sidebar-mode-map (kbd "TAB"))
              'vulpea-ui-widget-toggle-at-point))
  (should (eq (lookup-key vulpea-ui-sidebar-mode-map (kbd "RET"))
              'vulpea-ui-follow-link-at-point)))


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


(provide 'vulpea-ui-test)
;;; vulpea-ui-test.el ends here
