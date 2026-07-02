;;; vulpea-ui-bench-render.el --- Benchmark the schema dashboard render -*- lexical-binding: t; -*-

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

;; A development-only benchmark for the schema dashboard render.  It
;; fabricates invalid wine notes in memory, builds the collection health
;; struct straight from them (no DB, no ripgrep, no external harness),
;; and times how long `vui-mount' of `vulpea-ui-schema-dashboard-root'
;; takes at a few collection sizes.  Nothing is written to disk.
;;
;; Two render modes are measured:
;;
;; - DEFAULT (collapsed) is the primary, robust measurement.  Invalid
;;   notes start collapsed, so the dashboard draws one toggle button per
;;   invalid note plus the per-schema headers.  This exercises the render
;;   path a user actually hits when opening the dashboard on a large,
;;   messy collection and does not depend on the internals of the note
;;   component.
;;
;; - EXPANDED forces every note open so each violation row renders too.
;;   This is a heavier stress test, but see the big warning on
;;   `vulpea-ui-bench-render--with-expanded' below: it works by
;;   redefining `vulpea-ui-schema-dashboard-note' in vui's component
;;   registry, so the redefinition is a hand-copied clone of the real
;;   component and MUST be kept in sync if that component ever changes.
;;
;; The point of the benchmark is to compare vui versions.  It renders the
;; current vui-collapsible dashboard, so it needs a vui that supports rich
;; collapsible headers (vui #108 and later); a pre-#108 vui cannot render
;; the current dashboard at all, so it is out of scope here.  By default
;; the bench uses the vui that eldev resolves for the project.  To pin a
;; specific vui checkout (e.g. to compare vui #108 against #109), set the
;; VULPEA_UI_BENCH_VUI_DIR environment variable to that checkout's
;; directory; only that directory is put on the load-path for vui, so the
;; version under test is unambiguous.
;;
;; History: this bench exists because an older vui rendered the dashboard
;; buttons in O(n^2) and could crash a large dashboard; vui #108 made the
;; render linear and #109 cut the per-row cost by rendering real button.el
;; text buttons instead of widgets.  That original investigation and its
;; before/after numbers live on vui #107
;; (https://github.com/d12frosted/vui.el/issues/107); this bench is the
;; guard that stays behind for the current dashboard render.
;;
;; Run it via eldev, for example:
;;
;;   eldev exec "(progn (require 'vulpea-ui-bench-render) \
;;                       (vulpea-ui-bench-render-run))"
;;
;; or, pinning a vui checkout:
;;
;;   VULPEA_UI_BENCH_VUI_DIR=~/Developer/vui.el \
;;     eldev exec "(progn (require 'vulpea-ui-bench-render) \
;;                        (vulpea-ui-bench-render-run))"
;;
;; See bench/README.org for the batch-vs-redisplay caveat and reference
;; numbers on the current dashboard.

;;; Code:

;; vui-version knob: when VULPEA_UI_BENCH_VUI_DIR is set, load vui from
;; that directory ONLY, so the version under test is unambiguous.  This
;; must happen before vui / vulpea-ui are required below.
(let ((vui-dir (getenv "VULPEA_UI_BENCH_VUI_DIR")))
  (when (and vui-dir (not (string-empty-p vui-dir)))
    (setq load-path (cons (expand-file-name vui-dir) load-path))))

(require 'benchmark)
(require 'cl-lib)
(require 'seq)
(require 'vulpea)
(require 'vui)
(require 'vui-components)
(require 'vulpea-ui)

;; Declared so this file byte-compiles even though it is excluded from
;; the main build and depends on the dashboard + vui only at runtime.
(defvar vui--component-registry)
(declare-function vui-mount "vui" (component-vnode &optional buffer-name))
(declare-function vui-component "vui" (type &rest props-and-children))
(declare-function vui-text "vui" (content &rest props))
(declare-function vui-collapsible "vui-components" (&rest args))
(declare-function vulpea-ui-schema-dashboard--sort "vulpea-ui" (healths))
(declare-function vulpea-ui-schema-dashboard--width "vulpea-ui" ())
(declare-function vulpea-ui-schema-dashboard--render-violation "vulpea-ui" (schema note violation index))
(declare-function vulpea-schema-collection-health "vulpea-schema" (&optional notes))
(declare-function vulpea-schema-list "vulpea-schema" ())
(declare-function vulpea-schema-unregister "vulpea-schema" (name))
(declare-function vulpea-schema-define "vulpea-schema" (name &rest props))
(declare-function vulpea-schema-health-invalid "vulpea-schema" (health))
(declare-function vulpea-schema-health-invalid-notes "vulpea-schema" (health))

(defun vulpea-ui-bench-render--define-schema ()
  "Register a single wine schema for the benchmark.
Clears any previously registered schemas first so the fabricated notes
are the only thing the health scan sees."
  (dolist (n (vulpea-schema-list))
    (vulpea-schema-unregister n))
  (vulpea-schema-define 'wine
    :predicate (lambda (note) (member "wine" (vulpea-note-tags note)))
    :fields '((:key "name"      :type string :required t)
              (:key "producer"  :type string :required t)
              (:key "region"    :type string :required t)
              (:key "vintage"   :type number)
              (:key "colour"    :type symbol :required t :one-of (red white rose orange))
              (:key "sweetness" :type string)
              (:key "rating"    :type string))))

(defun vulpea-ui-bench-render--notes (count nviol)
  "Fabricate COUNT invalid wine notes in memory, each with NVIOL violations.
Every note is missing required fields (producer) and carries too many
values for the single-valued =colour= field, so the schema scan flags
it.  Nothing touches the DB or the filesystem."
  (let (notes)
    (dotimes (i count)
      (push (make-vulpea-note
             :id (format "%08x-0000-4000-8000-%012x" i i)
             :title (format "Cuvee %d" i)
             :tags '("wine")
             :meta (list (cons "name"   (list (format "Cuvee %d" i)))
                         (cons "region" (list "Bordeaux"))
                         (cons "colour" (make-list (max 1 (1- nviol)) "chartreuse"))))
            notes))
    (nreverse notes)))

(defun vulpea-ui-bench-render--health (notes)
  "Build the sorted dashboard health for NOTES.
Mirrors what the live dashboard does: `vulpea-schema-collection-health'
on an explicit note list, then `vulpea-ui-schema-dashboard--sort'."
  (vulpea-ui-schema-dashboard--sort (vulpea-schema-collection-health notes)))

(defun vulpea-ui-bench-render--render-once (health)
  "Mount the dashboard root for HEALTH once and tear it down."
  (vui-mount (vui-component 'vulpea-ui-schema-dashboard-root :health health)
             "*vulpea-ui-bench-render*")
  (when (get-buffer "*vulpea-ui-bench-render*")
    (kill-buffer "*vulpea-ui-bench-render*")))

(defun vulpea-ui-bench-render--time (health)
  "Return the fastest of two timed renders of HEALTH, in seconds.
A warmup render runs first (untimed) so byte-code and caches are warm."
  (garbage-collect)
  (vulpea-ui-bench-render--render-once health) ; warmup
  (let (times)
    (dotimes (_ 2)
      (garbage-collect)
      (push (car (benchmark-run 1 (vulpea-ui-bench-render--render-once health)))
            times))
    (car (sort times #'<))))

;; WARNING: force-expand is coupled to the current dashboard note
;; component.  This macro redefines `vulpea-ui-schema-dashboard-note' in
;; `vui--component-registry' with a clone of the real component whose
;; `vui-collapsible' is flipped to :initially-expanded t, so every note
;; renders its violation rows.  The body below is copied verbatim from
;; `vulpea-ui-schema-dashboard-note' in vulpea-ui.el, changing only that
;; one prop.  If that component changes (its signature, layout, keys, or
;; the `vulpea-ui-schema-dashboard--render-violation' call it makes),
;; this clone MUST be updated to match, or the expanded numbers stop
;; measuring the real render.  (This already bit once: the scratchpad
;; driver this bench came from cloned an older vui-vstack/vui-button note
;; whose `--render-violation' took two args, and it errored against the
;; current vui-collapsible component whose renderer takes four.)  The
;; default (collapsed) benchmark has no such coupling and is the primary
;; measurement.
(defmacro vulpea-ui-bench-render--with-expanded (&rest body)
  "Run BODY with every dashboard note forced expanded, then restore."
  `(let ((orig (gethash 'vulpea-ui-schema-dashboard-note vui--component-registry)))
     (unwind-protect
         (progn
           (eval '(vui-defcomponent vulpea-ui-schema-dashboard-note (entry schema)
                    :render
                    (let* ((note (car entry))
                           (violations (cdr entry))
                           (count (length violations)))
                      (vui-collapsible
                       :title (or (vulpea-note-title note) "(untitled)")
                       :header-right (vui-text
                                      (format "%d %s" count
                                              (if (= count 1) "issue" "issues"))
                                      :face 'vulpea-ui-schema-health-message-face)
                       :header-width #'vulpea-ui-schema-dashboard--width
                       :initially-expanded t
                       :key (list schema (vulpea-note-id note))
                       (seq-map-indexed
                        (lambda (v i)
                          (vulpea-ui-schema-dashboard--render-violation schema note v i))
                        violations))))
                 t)
           ,@body)
       (when orig
         (puthash 'vulpea-ui-schema-dashboard-note orig vui--component-registry)))))

(defconst vulpea-ui-bench-render-scales '(2000 20000 50000)
  "Invalid-note counts the benchmark renders at.")

;;;###autoload
(defun vulpea-ui-bench-render-run ()
  "Benchmark the dashboard render at a few scales and print a summary.
Fabricates invalid wine notes in memory, builds the health, and times a
default (collapsed) and a forced-expanded render at each scale in
`vulpea-ui-bench-render-scales'.  Writes nothing to disk."
  (vulpea-ui-bench-render--define-schema)
  (princ (format "== vui loaded from:       %s\n" (locate-library "vui")))
  (princ (format "== vulpea-ui loaded from: %s\n" (locate-library "vulpea-ui")))
  (princ "\ncount\tviolation_rows\tdefault_s\tdefault_us_per_row\texpanded_s\n")
  (dolist (count vulpea-ui-bench-render-scales)
    (let* ((notes (vulpea-ui-bench-render--notes count 2))
           (health (vulpea-ui-bench-render--health notes))
           (rows (let ((tot 0))
                   (dolist (h health)
                     (setq tot (+ tot (vulpea-schema-health-invalid h)))
                     (dolist (nv (vulpea-schema-health-invalid-notes h))
                       (setq tot (+ tot (length (cdr nv))))))
                   tot))
           (t-default (vulpea-ui-bench-render--time health))
           (t-expanded (vulpea-ui-bench-render--with-expanded
                        (vulpea-ui-bench-render--time health))))
      (princ (format "%d\t%d\t%.4f\t%.1f\t%.4f\n"
                     count rows t-default
                     (/ (* t-default 1e6) (max 1 count))
                     t-expanded))
      (setq notes nil health nil)
      (garbage-collect)))
  (princ "\ndone\n"))

(provide 'vulpea-ui-bench-render)
;;; vulpea-ui-bench-render.el ends here
