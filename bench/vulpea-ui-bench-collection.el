;;; vulpea-ui-bench-collection.el --- Benchmark the collection view -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Boris Buliga
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/d12frosted/vulpea-ui
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

;; A development-only benchmark for the collection view.  It fabricates
;; notes in memory (no DB, nothing written to disk) and times the three
;; paths that matter at scale:
;;
;; - ENTRIES: building tabulated-list entries (cell rendering, faces,
;;   help-echo) at a few collection sizes.
;; - RENDER: a full refresh-equivalent - entries, the width-fitting
;;   format pass, and `tabulated-list-print' - at the same sizes.
;; - MARK: 100 single marks on the largest table.  This is the guard
;;   for the O(1) mark path: marking used to go through the table's
;;   full re-print machinery, which walks and diffs every buffer line,
;;   and was unusable around 4k notes.  If this number regresses from
;;   microseconds to anything visible, that path broke.
;;
;; Run it via eldev, for example:
;;
;;   eldev exec "(progn (require 'vulpea-ui-bench-collection) \
;;                      (vulpea-ui-bench-collection-run))"

;;; Code:

(require 'vulpea-ui)
(require 'benchmark)

(defvar vulpea-ui-bench-collection-sizes '(1000 5000 10000)
  "Collection sizes to measure.")

(defun vulpea-ui-bench-collection--notes (n)
  "Fabricate N in-memory notes with tags, meta and mixed levels."
  (mapcar
   (lambda (i)
     (make-vulpea-note
      :id (format "bench-%d" i)
      :path (format "/bench/notes/note-%d.org" (/ i 10))
      :level (if (= 0 (% i 3)) 0 2)
      :pos 1
      :title (format "Benchmark note %d with a reasonably long title" i)
      :file-title (format "File %d" (/ i 10))
      :outline-path (unless (= 0 (% i 3)) (list "Section"))
      :tags (list (format "tag-%d" (% i 7)) "bench")
      :todo (when (= 0 (% i 4)) "TODO")
      :meta (list (list "rating" (number-to-string (% i 10)))
                  (list "country" (format "Country %d" (% i 5))))))
   (number-sequence 1 n)))

(defun vulpea-ui-bench-collection--buffer (notes columns)
  "Return a fresh collection buffer rendered with NOTES and COLUMNS."
  (let ((buffer (generate-new-buffer " *bench-collection*")))
    (with-current-buffer buffer
      (vulpea-ui-collection-mode)
      (setq vulpea-ui-collection--view (list :name "bench"
                                             :columns columns))
      (dolist (note notes)
        (puthash (vulpea-note-id note) note
                 vulpea-ui-collection--note-table))
      (setq tabulated-list-entries
            (vulpea-ui-collection--entries
             notes columns vulpea-ui-collection--marked nil))
      (setq tabulated-list-format
            (vulpea-ui-collection--format columns tabulated-list-entries))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min)))
    buffer))

(defun vulpea-ui-bench-collection-run ()
  "Run the collection view benchmarks and report the timings."
  (interactive)
  (let ((columns '(title context tags todo (meta "rating") backlinks)))
    (dolist (size vulpea-ui-bench-collection-sizes)
      (let ((notes (vulpea-ui-bench-collection--notes size))
            (marked (make-hash-table :test 'equal)))
        (message "entries %5d notes: %s" size
                 (benchmark-run 1
                   (vulpea-ui-collection--entries notes columns marked nil)))
        (let ((buffer (generate-new-buffer " *bench-collection*")))
          (unwind-protect
              (with-current-buffer buffer
                (vulpea-ui-collection-mode)
                (setq vulpea-ui-collection--view
                      (list :name "bench" :columns columns))
                (message "render  %5d notes: %s" size
                         (benchmark-run 1
                           (progn
                             (setq tabulated-list-entries
                                   (vulpea-ui-collection--entries
                                    notes columns
                                    vulpea-ui-collection--marked nil))
                             (vulpea-ui-collection--update-format columns)
                             (tabulated-list-print)))))
            (kill-buffer buffer)))))
    (let* ((size (car (last vulpea-ui-bench-collection-sizes)))
           (notes (vulpea-ui-bench-collection--notes size))
           (buffer (vulpea-ui-bench-collection--buffer notes columns)))
      (unwind-protect
          (with-current-buffer buffer
            (message "mark x100 at %d notes: %s" size
                     (benchmark-run 100
                       (vulpea-ui-collection-mark))))
        (kill-buffer buffer)))))

(provide 'vulpea-ui-bench-collection)
;;; vulpea-ui-bench-collection.el ends here
