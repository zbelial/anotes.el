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

(defcustom ivy-anotes-auto-enable-anotes t
  "Whether enable `anote-local-mode' of a buffer or not when jump to a note of that buffer."
  :group 'anotes
  :type 'boolean)


(defun ivy-anotes--format-note (note uri &optional with-file)
  (let (str 
        meta
        id context annotation start-pos end-pos tags pos-type file-type
        ac
        )
    (setq id (anotes-note-id note))
    (setq context (anotes-note-context note))
    (setq annotation (anotes-note-annotation note))
    (setq start-pos (anotes-note-start-pos note))
    (setq end-pos (anotes-note-end-pos note))
    (setq pos-type (anotes-note-pos-type note))
    (setq file-type (anotes-note-file-type note))
    (setq tags (anotes-note-tags note))

    (setq ac annotation)
    (when (string-empty-p ac)
      (setq ac context))
    (if with-file
        (setq str (format "%-30s        %-90s        %s" (s-truncate 30 tags) (s-truncate 90 ac) (f-short uri)))
      (setq str (format "%-30s        %-90s" (s-truncate 30 tags) (s-truncate 90 ac)))
      )
    (setq meta (list :id id :context context :annotation annotation :start-pos start-pos :end-pos end-pos :tags tags :uri uri :pos-type pos-type :file-type file-type))

    (cons str meta)
    )
  )

;; sort in back-to-front order according to start.
(defun ivy-anotes--buffer-note-sorter (note1 note2)
  (let ((start1 (anotes-live-note-start-pos note1))
        (start2 (anotes-live-note-start-pos note2)))
    (> start1 start2)
    )
  )

(defun ivy-anotes--buffer-candidates ()
  (let ((notes (ht-values anotes--buffer-notes))
        (uri (anotes-buffer-info-uri anotes--buffer-info))
        cand candidates uri)
    (setq notes (cl-sort notes #'ivy-anotes--buffer-note-sorter))
    (dolist (note notes)
      (cl-pushnew (ivy-anotes--format-note (anotes-from-live-note note) uri) candidates)
      )
    candidates
    )
  )

(defun ivy-anotes--buffer-jump (cand)
  (let ((meta (cdr cand))
        pos)
    (setq pos (plist-get meta :start-pos))
    (goto-char pos)
    (recenter)
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
        (setq marker (plist-get meta :start-pos))
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
  (when (not anotes-local-mode)
    (user-error "Enable anotes-local-mode first.")
    )
  (interactive)
  (setq ivy-anotes--opoint (point))
  (let (candidates res
                   (preselect 0))
    (setq candidates (ivy-anotes--buffer-candidates))
    (dolist (cand candidates)
      (when (< (plist-get (cdr cand) :start-pos) ivy-anotes--opoint)
        (setq preselect (1+ preselect))))
    (unwind-protect
        (setq res (ivy-read "Notes: " candidates
                            :preselect preselect
                            :action '(1
                                      ("j" ivy-anotes--buffer-jump "Jump to note position.")
                                      ("p" ivy-anotes--buffer-preview "Preview note context.")
                                      ("m" ivy-anotes--buffer-note-edit "Edit note.")
                                      ("d" ivy-anotes--buffer-note-delete "Delete note.")
                                      )
                            :update-fn #'ivy-anotes--buffer-preview
                            :caller #'ivy-anotes
                            ))
      (unless res
        (goto-char ivy-anotes--opoint)
        (setq ivy-anotes--opoint nil))
      )
    )
  )

(defun ivy-anotes--label-candidates ()
  (let (label
        (buffer-info anotes--buffer-info)
        pos-type
        label-notes file-notes
        candidates)
    (if buffer-info
        (setq label (anotes-buffer-info-label buffer-info))
      (setq buffer-info (anotes--buffer-info))
      (setq pos-type (anotes-buffer-info-type buffer-info))
      (when (eq pos-type 'unsupported)
        (user-error "Not supported."))
      (setq label (anotes-buffer-info-label buffer-info))
      )
    (message "ivy-anotes-label label %s" label)
    (setq label-notes (ht-get anotes--label-notes label))
    (when (not label-notes)
      (anotes--load-label-notes label)
      (setq label-notes (ht-get anotes--label-notes label)))
    (when label-notes
      (dolist (uri (ht-keys label-notes))
        (setq file-notes (ht-get label-notes uri))
        (dolist (note (ht-values file-notes))
          (cl-pushnew (ivy-anotes--format-note note uri t) candidates)
          )
        )
      )
    candidates
    )
  )

(defun ivy-anotes--existing-eww-buffer (uri)
  (let ((buffers (buffer-list))
        target)
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when (and
               (eq major-mode 'eww-mode)
               (or (equal uri (anotes--buffer-file-name))
                   (equal uri (eww-current-url))
                   ))
          (setq target buffer))))
    target))

(defun ivy-anotes--label-jump (cand)
  (let ((meta (cdr cand))
        file-type pos-type
        uri
        pos)
    (setq uri (plist-get meta :uri))
    (setq pos (plist-get meta :start-pos))
    (setq file-type (plist-get meta :file-type))
    (setq pos-type (plist-get meta :pos-type))
    (cond
     ((eq file-type 'text)
      (find-file uri)
      (goto-char pos)
      (recenter)
      )
     ((eq file-type 'local-webpage)
      (let ((buffer (ivy-anotes--existing-eww-buffer uri))
            )
        (if buffer
            (progn
              (switch-to-buffer buffer)
              (goto-char pos)
              (recenter))
          (eww (s-concat "file://" file) 4)
          (goto-char pos)
          (recenter)))      
      )
     ((eq file-type 'remote-webpage)
      (let ((buffer (ivy-anotes--existing-eww-buffer uri))
            )
        (if buffer
            (progn
              (switch-to-buffer buffer)
              (goto-char pos)
              (recenter))
          (eww-browse-url uri t)
          (goto-char pos)
          (recenter)))
      )
     (t
      (user-error "Internal error - unknown type.")
      )
     )
    (when (and
           ivy-anotes-auto-enable-anotes
           (not anotes-local-mode))
      (anotes-local-mode t))
    )
  )

(defun ivy-anotes-label ()
  (interactive)
  (let (candidates res)
    (setq candidates (ivy-anotes--label-candidates))
    (ivy-read "Notes: " candidates
              :action '(1
                        ("j" ivy-anotes--label-jump "Jump to note position.")
                        )
              :caller #'ivy-anotes-label
              )    
    )
  )

(provide 'ivy-anotes)
