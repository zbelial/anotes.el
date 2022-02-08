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


(cl-defstruct anotes-position
  (position)
  )

(cl-defstruct anotes-range
  (type)
  (start-pos) ;; anotes-position
  (end-pos) ;; anotes-position
  )

(cl-defstruct anotes-note
  "Represent a note."
  (id)
  (tags) ;; tag seperated with comma
  (context) ;; text selected 
  (annotation)
  (note-file) ;; for a large note, it can be write to an org file.
  (range) ;; anotes-range
  )

(cl-defstruct anotes-live-note
  "Represent a live note that uses markers(if possible) as position."
  (id)
  (tags) ;; tag seperated with comma
  (context) ;; text selected 
  (annotation)
  (note-file) ;; for a large note, it can be write to an org file.
  (type)
  (start) ;; marker or percent
  (end) ;; marker or percent
  )

(cl-defmethod anotes-from-live-note ((live-note anotes-live-note))
  (let ((type (anotes-live-note-type live-note))
        (tags (anotes-live-note-tags live-note))
        (context (anotes-live-note-context live-note))
        (annotation (anotes-live-note-annotation live-note))
        (start (anotes-live-note-start live-note))
        (end (anotes-live-note-end live-note))
        (id (anotes-live-note-id live-note))
        (note-file (anotes-live-note-note-file live-note))
        start-pos end-pos range
        note)
    (if (equal type ANOTES-CHAR-POS)
        (progn
          (setq start-pos (marker-position start))
          (setq end-pos (marker-position end))
          )
      (setq start-pos start)
      (setq end-pos end))
    (setq range (make-anotes-range :type type :start-pos start-pos :end-pos end-pos))  
    (setq note (make-anotes-note :id id :tags tags :context context :annotation annotation :range range :note-file note-file))

    note
    )
  )

(cl-defmethod anotes-to-live-note ((note anotes-note))
  (let ((id (anotes-note-id note))
        (tags (anotes-note-tags note))
        (context (anotes-note-context note))
        (annotation (anotes-note-annotation note))
        (note-file (anotes-note-note-file note))
        (range (anotes-note-range note))
        type start end range
        live-note)
    (setq type (anotes-range-type range))
    (if (equal type ANOTES-CHAR-POS)
        (progn
          (setq start (copy-marker (anotes-range-start-pos range)))
          (setq end (copy-marker (anotes-range-end-pos range)))
          )
      (setq start (anotes-range-start-pos range))
      (setq end (anotes-range-end-pos range)))
    (setq live-note (make-anotes-live-note :id id :tags tags :context context :annotation annotation :note-file note-file :type type :start start :end end))

    live-note
    )  
  )
