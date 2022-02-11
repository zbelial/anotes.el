;;; ivy-anotes.el --- Take notes in Emacs -*- lexical-binding: t; -*-

;; Copyright © 2022 Gene Zhao <zjyzhaojiyang@gmail.com>

;; Author: zbelial <zjyzhaojiyang@gmail.com>
;; URL: https://github.com/zbelial/anotes.el
;; Version: 0.1.0
;; Created: 2022-02-07
;; Keywords: annotate notes
;; Package-Requires: ((emacs "26"))

;; This file is not part of GNU Emacs.

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
;;
;; Take notes and add annotations in Emacs when viewing files, without changing the files.
;; 
;; For files that may be changed a lot, it's not appropriate to use this package.
;; For files that may be changed a lot, it's not appropriate to use this package.
;; For files that may be changed a lot, it's not appropriate to use this package.
;;


(require 'anotes-struct)
(require 'anotes)
(require 'ivy)
(require 'ht)


(defun ivy-anotes--format-note (note)
  (let (str
        meta
        id context annotation start end uri tags
        )
    (setq id (anotes-live-note-id note))
    (setq context (anotes-live-note-context note))
    (setq annotation (anotes-live-note-annotation note))
    (setq start (anotes-live-note-start note))
    (setq end (anotes-live-note-end note))
    (setq tags (anotes-live-note-tags note))
    (setq uri (anotes-buffer-info-uri anotes--buffer-info))

    (setq str (format "%20s - %60s" tags annotation))
    (setq meta (list :id id :context context :annotation annotation :start start :end end :tags tags :uri uri))

    (cons str meta)
    )
  )

;; sort in back-to-front order according to start.
(defun ivy-anotes--sorter (note1 note2)
  )

(defun ivy-anotes--buffer-candidates ()
  (let ((notes (ht-values anotes--buffer-notes))
        cand candidates)
    (setq notes (cl-sort notes #'ivy-anotes--sorter))
    (dolist (note notes)
      (cl-pushnew (ivy-anotes--format-note note) candidates)
      )
    candidates
    )
  )

(defun ivy-anotes--label-candidates ()
  )

(defun ivy-anotes--buffer-jump (cand)
  (let ((meta (cdr cand))
        pos)
    (setq pos (plist-get meta :start))
    (goto-char pos)
    )
  )

(defun ivy-anotes--buffer-preview ()
  (let ((current (ivy-state-current ivy-last))
	item meta
	marker
	)
    (with-ivy-window
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq meta (cdr item))
        (setq marker (plist-get meta :start))
        (goto-char marker)
        (recenter)
        (let ((pulse-delay 0.05))
	  (pulse-momentary-highlight-one-line (point))
	  )
        )))  
  )

(defun ivy-anotes--buffer-note-edit (cand)
  (let ((meta (cdr cand))
        id
        )
    (setq id (plist-get meta :id))
    (anotes--edit-note id t)
    )
  )

(defun ivy-anotes--buffer-note-delete (cand)
  (let ((meta (cdr cand))
        id
        )
    (setq id (plist-get meta :id))
    (anotes--delete-note id t)
    )
  )


(defvar ivy-anotes--opoint nil)
(defun ivy-anotes ()
  (interactive)
  (setq ivy-anotes--opoint (point))
  (let (candidates res)
    (setq candidates (ivy-anotes--buffer-candidates))
    (unwind-protect
        (setq res (ivy-read "Notes: " candidates
                            :action '(1
                                      ("j" ivy-anotes--buffer-jump "Jump to note position.")
                                      ("p" ivy-anotes--buffer-preview "Preview note context.")
                                      ("m" ivy-anotes--buffer-note-edit "Edit note.")
                                      ("d" ivy-anotes--buffer-note-delete "Delete note.")
                                      )
                            :update-fn #'ivy-anotes--buffer-preview
                            ))
      (unless res
        (goto-char ivy-anotes--opoint)
        (setq ivy-anotes--opoint nil))
      )
    )
  )
