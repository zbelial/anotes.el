;;; anotes.el --- Take notes in Emacs -*- lexical-binding: t; -*-

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

(require 'ht)
(require 's)
(require 'f)
(require 'seq)
(require 'cl-seq)
(require 'cl-lib)

(declare-function doc-view-current-page "doc-view")
(declare-function pdf-view-bookmark-make-record "ext:pdf-view")
(declare-function pdf-view-current-page "ext:pdf-view")
(declare-function pdf-view-mode "ext:pdf-view")

(defgroup anotes nil
  "Notes manager in Emacs."
  :prefix "anotes-"
  :group 'tools)

;;; Custom
(defcustom anotes-default-directory user-emacs-directory
  "The default directory used to store notes."
  :type  'string
  :group 'anotes)

(defcustom anotes-directory-alist nil
  "Alist of directory.
The notes added to files in the first directory will be saved to the second directory."
  :group 'anotes
  :type '(repeat (cons string (cons (directory :tag "Directory storing files.")
                                    (directory :tag "Anotes data file directory.")))))

(defcustom anotes-open-pdf-with-eaf nil
  "When non-nil, use `eaf-open' to open pdf files."
  :group 'anotes
  :type 'boolean)

(defcustom anotes-recent-topic-count 5
  "The number of topics recently added or visited that should be saved."
  :group 'anotes
  :type 'integer)

(defcustom anotes-show-visible-area-anotes-count nil
  "When non-nil, show how many anotes in visible area of current buffer."
  :group 'anotes
  :type 'boolean)


(defconst anotes-default-label "__DEFAULT_LABEL__")
(defconst anotes-default-anotes-file-name "anotes-data.el")

;;; Macros
(defmacro anotes--with-message-suppression (&rest body)
  "Suppress any incoming messages within `body' while keeping the
currently displayed message, if any."
  (let ((msg (make-symbol "msg-temp")))
    `(let ((,msg (current-message))
           (message-log-max nil))
       (unwind-protect
           (progn ,@body)
         (if ,msg
             (message ,msg)
           (message nil))))))


(defsubst anotes--label-item-contain-filename (filename)
  "Return the `anotes-directory-alist' item which matches filename most."
  (let ((max 0)
        result)
    (dolist (item anotes-directory-alist)
      (when (string-prefix-p (cadr item) filename)
        (when (> (length (cadr item)) max)
          (setq result item)
          (setq max (length (cadr item))))))
    result))

(defsubst anotes--label-item-match-label (label)
  "Return `anotes-directory-alist' item matching label."
  (cl-find-if
   (lambda (x) (string= (car x) label))
   anotes-directory-alist))

(defsubst anotes--note-file-directory (filename)
  "Return xray file directory of filename"
  (let ((label-item (anotes--label-item-contain-filename filename)))
    (if label-item
        (f-slash (cddr label-item))
      (f-slash anotes-default-directory))))

(defsubst anotes--file-label (filename)
  "Return base directory of filename"
  (let ((label-item (anotes--label-item-contain-filename filename)))
    (if label-item
        (car label-item)
      anotes-default-label)))

(defsubst anotes--note-file-directory-match-label (label)
  "Return note directory matching label"
  (let ((label-item (anotes--label-item-match-label label)))
    (if label-item
        (f-slash (cddr label-item))
      (f-slash anotes-default-directory))))

(defsubst anotes--directory-contain-file (filename)
  "Return base directory of filename"
  (let ((label-item (anotes--label-item-contain-filename filename)))
    (if label-item
        (f-slash (cadr label-item))
      "/"
      )))

(defsubst anotes--directory-match-label (label)
  "Return base directory of label"
  (let ((label-item (anotes--label-item-match-label label)))
    (if label-item
        (f-slash (cadr label-item))
      "/"
      )))

(defsubst anotes--file-relative-path (filename)
  "Return relative path of filename."
  (let ((label-item (anotes--label-item-contain-filename filename)))
    (if label-item
        (s-chop-prefix (f-full (cadr label-item)) filename)
      (s-chop-prefix "/" filename))))

(defsubst anotes--note-file-relative-path (filename note-file-name)
  "Return relative path of filename to anotes--file-base-directory"
  (let ((label-item (anotes--label-item-contain-filename filename)))
    (if label-item
        (s-chop-prefix (f-full (cddr label-item)) note-file-name)
      (s-chop-prefix (f-full anotes-default-directory) note-file-name)
      )))

(defun anotes--current-line-number ()
  ""
  (line-number-at-pos)
  )

(defun anotes--current-time()
  ""
  (time-convert nil 'integer))

(defun anotes--current-time-readable()
  ""
  (format-time-string "%Y%m%d%H%M%S"))

(defun anotes--id ()
  "Note id."
  (+ (* (anotes--current-time) 1000) (random 999)))


(defsubst anotes--remove-html-anchor (url)
  ""
  (let* ((len (length url))
         (pos-of-anchor (s-index-of "#" url)))
    (if (not pos-of-anchor) ;; no anchor part
        url
      (substring url 0 pos-of-anchor))))


(defun anotes--buffer-file-name()
  ""
  (cond
   ((eq major-mode 'eaf-mode)
    (let ((app eaf--buffer-app-name))
      (when (string-equal app "pdf-viewer")
        eaf--buffer-url)))
   ((eq major-mode 'eww-mode)
    (let ((buffer-url (eww-current-url)))
      (when (string-prefix-p "file://" buffer-url)
        (substring (anotes--remove-html-anchor buffer-url) (length "file://")))))
   (t
    (buffer-file-name))))

(defun anotes--remote-webpage ()
  (with-current-buffer (current-buffer)
    (when (eq major-mode 'eww-mode)
      (let ((buffer-url (eww-current-url)))
        (or (string-prefix-p "http://" buffer-url)
            (string-prefix-p "https://" buffer-url))))))

(defun anotes--remote-webpage-url ()
  (let ((buffer-url (eww-current-url)))
    buffer-url))

(defun anotes--local-webpage ()
  (with-current-buffer (current-buffer)
    (eq major-mode 'eww-mode)))

(defun anotes--absolute-point ()
  (save-excursion
    (save-restriction
      (point))))

(defun anotes--new-note-in-text-buffer ()
  "Create a new piece of note in a text-mode/prog-mode/eww-mode buffer."
  (let ((id (anotes--id))
        (tags "")
        (context "")
        annotation
        start
        end
        live-note
        )
    (if (region-active-p)
        (progn
          (setq start (copy-marker (region-beginning)))
          (setq end (copy-marker (region-end)))
          (setq context (buffer-substring-no-properties (region-beginning) (region-end))))
      (setq start (copy-marker (anotes--absolute-point)))
      (setq end (copy-marker start))
      (setq context ""))
    (setq annotation (read-string "Annotation: "))
    (setq tags (read-string "Tags(seperated with comma): "))
    (unless tags
      (setq tags ""))

    (setq live-note (make-anotes-live-note :id id :tags tags :context context :annotation annotation :type ANOTES-CHAR-POS :start start :end end))

    live-note
    )
  )

(defun anotes--new-note-in-pdf-buffer ()
  "Create a new piece of note in a pdf buffer."
  (user-error "Not Implemented!")
  )

(defun anotes-add-note ()
  "Add a new piece of note, save it to a note file, and display it in current buffer if possible."
  (interactive)
  (let (live-note)
    (cond
     ((derived-mode-p 'text-mode)
      )
     ((derived-mode-p 'prog-mode)
      )
     ((eq major-mode 'eww-mode)
      )
     (t
      ))
    )
  )

