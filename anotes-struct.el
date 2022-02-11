;;; anotes-struct.el --- Take notes in Emacs -*- lexical-binding: t; -*-

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

(require 'cl-lib)
(require 'cl-generic)

(defconst ANOTES-CHAR-POS 0 "Identifying a position in text files.")
(defconst ANOTES-PERCENT-POS 1 "Identifying a position in, for example, PDF files.")

(cl-defgeneric anotes-to-live-note (note)
  )

(cl-defgeneric anotes-from-live-note (live-note)
  )

(cl-defstruct anotes-note
  "Represent a note."
  (id)
  (tags) ;; tag seperated with comma
  (context) ;; text selected 
  (annotation)
  (label)
  (extra-note-file) ;; for a large note, it can be writen to an org file.
  (pos-type)
  (start-pos)
  (end-pos)
  (file-type) ;; symbol: text pdf local-webpage remote-webpage unsupported
  )

(cl-defstruct anotes-live-note
  "Represent a live note that uses markers(if possible) as position."
  (id)
  (tags) ;; tag seperated with comma
  (context) ;; text selected 
  (annotation)
  (label)
  (extra-note-file) ;; for a large note, it can be writen to an org file.
  (pos-type)
  (start-pos) ;; marker or percent
  (end-pos) ;; marker or percent
  (file-type) ;; symbol: text pdf local-webpage remote-webpage unsupported
  )

(cl-defmethod anotes-from-live-note ((live-note anotes-live-note))
  (let ((pos-type (anotes-live-note-pos-type live-note))
        (tags (anotes-live-note-tags live-note))
        (context (anotes-live-note-context live-note))
        (annotation (anotes-live-note-annotation live-note))
        (start (anotes-live-note-start-pos live-note))
        (end (anotes-live-note-end-pos live-note))
        (id (anotes-live-note-id live-note))
        (label (anotes-live-note-label live-note))
        (extra-note-file (anotes-live-note-extra-note-file live-note))
        (file-type (anotes-live-note-file-type live-note))
        start-pos end-pos range
        note)
    (if (equal pos-type ANOTES-CHAR-POS)
        (progn
          (setq start-pos (marker-position start))
          (setq end-pos (marker-position end))
          )
      (setq start-pos start)
      (setq end-pos end))
    (setq note (make-anotes-note :id id :tags tags :context context :annotation annotation :pos-type pos-type :start-pos start-pos :end-pos end-pos :label label :extra-note-file extra-note-file :file-type file-type))

    note
    )
  )

(cl-defmethod anotes-to-live-note ((note anotes-note))
  (let ((id (anotes-note-id note))
        (tags (anotes-note-tags note))
        (context (anotes-note-context note))
        (annotation (anotes-note-annotation note))
        (label (anotes-note-label note))
        (extra-note-file (anotes-note-extra-note-file note))
        (file-type (anotes-note-file-type note))
        pos-type start-pos end-pos
        live-note)
    (setq pos-type (anotes-note-pos-type note))
    (if (equal pos-type ANOTES-CHAR-POS)
        (progn
          (setq start-pos (copy-marker (anotes-note-start-pos note)))
          (setq end-pos (copy-marker (anotes-note-end-pos note)))
          )
      (setq start-pos (anotes-note-start-pos note))
      (setq end-pos (anotes-note-end-pos note)))
    (setq live-note (make-anotes-live-note :id id :tags tags :context context :annotation annotation :label label :extra-note-file extra-note-file :pos-type pos-type :start-pos start-pos :end-pos end-pos :file-type file-type))

    live-note
    )
  )

(cl-defstruct anotes-anote-info
  (label)
  (filedir) ;; base directory name of file, for remote webpage, this is an empty string
  (anote-dir))

(cl-defstruct anotes-buffer-info
  (type) ;; symbol: text pdf local-webpage remote-webpage unsupported
  (uri) ;; filename or webpage url
  (label)
  (filedir) ;; base directory name of file, for remote webpage, this is an empty string
  (anote-dir)
  (anote-file)
  )

(provide 'anotes-struct)
