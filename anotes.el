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

(require 'anotes-struct)

(declare-function doc-view-current-page "doc-view")
(declare-function pdf-view-bookmark-make-record "ext:pdf-view")
(declare-function pdf-view-current-page "ext:pdf-view")
(declare-function pdf-view-mode "ext:pdf-view")

(defgroup anotes nil
  "Notes manager in Emacs."
  :prefix "anotes-"
  :group 'tools)

;;; Face
(defface anotes-highlight
  '((t (:underline (:color "coral" :style wave))))
  "Face for annotation highlights."
  :group 'anotes)

(defface anotes-annotation
  '((t (:background "coral" :foreground "black" :inherit default)))
  "Face for annotations."
  :group 'anotes)

;;; Custom
(defcustom anotes-default-local-note-directory user-emacs-directory
  "The default directory used to store notes."
  :type  'string
  :group 'anotes)

(defcustom anotes-default-webpage-note-directory user-emacs-directory
  "The default directory used to store notes."
  :type  'string
  :group 'anotes)

(defcustom anotes-directory-alist nil
  "Alist of directory.
The notes added to files in the first directory will be saved to the second directory."
  :group 'anotes
  :type '(repeat (cons string (cons (directory :tag "Directory storing files.")
                                    (directory :tag "Anotes data file directory.")))))

(defcustom anotes-right-margin-width 30
  "The width of right margin to show annotation."
  :group 'anotes
  :type 'integer)

(defconst anotes-default-local-label "ANOTE_LOCAL")
(defconst anotes-default-remote-webpage-label "ANOTE_REMOTE")

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

(defsubst anotes--local-webpage (url)
  (string-prefix-p "file://" url))

(defsubst anotes--remote-webpage (url)
  (or (string-prefix-p "http://" url)
      (string-prefix-p "https://" url)))

(defsubst anotes--anote-file-name (label anote-dir)
  (concat (f-full anote-dir) label ".anote"))

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

(defsubst anotes--file-anote-info (filename filetype)
  "Return base directory of filename"
  (let (label anote-dir anote-info filedir)
    (if (eq filetype 'remote-webpage)
        (progn
          (setq label anotes-default-remote-webpage-label)
          (setq anote-dir anotes-default-webpage-note-directory)
          (setq filedir "")
          )
      (let ((label-item (anotes--label-item-contain-filename filename)))
        (if label-item
            (progn
              (setq label (car label-item))
              (setq anote-dir (cddr label-item))
              (setq filedir (cadr label-item)))
          (setq label anotes-default-local-label)
          (setq anote-dir anotes-default-local-note-directory)
          (setq filedir "/"))
        ))
    (setq anote-info (make-anotes-anote-info :label label :anote-dir anote-dir :filedir filedir))

    anote-info
    )
  )

(defsubst anotes--note-file-directory-match-label (label)
  "Return note directory matching label"
  (let ((label-item (anotes--label-item-match-label label)))
    (if label-item
        (f-slash (cddr label-item))
      (if (s-equals? label anotes-default-remote-webpage-label)
          (f-slash anotes-default-webpage-note-directory)
        (f-slash anotes-default-local-note-directory)))))

(defsubst anotes--directory-match-label (label)
  "Return base directory of label"
  (let ((label-item (anotes--label-item-match-label label)))
    (if label-item
        (f-slash (cadr label-item))
      (if (s-equals? label anotes-default-remote-webpage-label)
          ""
        "/"
        ))))

(defsubst anotes--file-label (filename)
  "Return label of filename"
  (let ((label-item (anotes--label-item-contain-filename filename)))
    (if label-item
        (car label-item)
      (if (anotes--remote-webpage filename)
          anotes-default-remote-webpage-label
        anotes-default-local-label
        ))))

(defsubst anotes--current-time()
  ""
  (time-convert nil 'integer))

(defsubst anotes--id ()
  "Note id."
  (+ (* (anotes--current-time) 1000) (random 999)))

(defsubst anotes--remove-html-anchor (url)
  ""
  (let* ((len (length url))
         (pos-of-anchor (s-index-of "#" url)))
    (if (not pos-of-anchor) ;; no anchor part
        url
      (substring url 0 pos-of-anchor))))

(defun anotes--buffer-info (&optional buffer)
  (let ((type 'unsupported)
        buffer-info uri label anote-info anote-dir filedir)
    (with-current-buffer (or buffer (current-buffer))
      (cond
       ((or (derived-mode-p 'text-mode 'prog-mode))
        (setq uri (anotes--buffer-file-name))
        (when uri
          (setq type 'text)
          (setq anote-info (anotes--file-anote-info uri type))
          ))
       ((eq major-mode 'eww-mode)
        (let ((buffer-url (eww-current-url)))
          (if (anotes--local-webpage buffer-url)
              (progn
                (setq type 'local-webpage)
                (setq uri (anotes--buffer-file-name)))
            (setq type 'remote-webpage)
            (setq uri buffer-url))
          (setq anote-info (anotes--file-anote-info uri type))
          )
        )
       ;; TODO pdf
       (t
        (setq type 'unsupported)
        ))
      )
    (if (eq type 'unsupported)
        (setq buffer-info (make-anotes-buffer-info :type 'unsupported))
      (setq label (anotes-anote-info-label anote-info))
      (setq anote-dir (anotes-anote-info-anote-dir anote-info))
      (setq filedir (anotes-anote-info-filedir anote-info))
      (setq buffer-info (make-anotes-buffer-info :type type :uri uri :anote-info anote-info :label label :filedir filedir :anote-dir anote-dir :anote-file (anotes--anote-file-name label anote-dir))))

    buffer-info
    )
  )

(defun anotes--buffer-file-name()
  "Return filename of current buffer."
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

(defun anotes--new-note-in-text-buffer (type)
  "Create a new piece of note in a text-mode/prog-mode/eww-mode buffer."
  (let ((id (anotes--id))
        (tags "")
        (context "")
        annotation
        start
        end
        live-note
        label
        note-file
        )
    (setq label (anotes-buffer-info-label anotes--buffer-info))
    (if (region-active-p)
        (progn
          (setq start (copy-marker (region-beginning)))
          (setq end (copy-marker (region-end)))
          (setq context (buffer-substring-no-properties (region-beginning) (region-end)))
          (deactivate-mark))
      (setq start (copy-marker (line-beginning-position)))
      (setq end (copy-marker (line-end-position)))
      (setq context ""))
    (setq annotation (read-string "Annotation: "))
    (setq tags (read-string "Tags(seperated with comma): "))
    (unless tags
      (setq tags ""))

    (unless (and (string-empty-p context)
                 (string-empty-p annotation))
      (setq live-note (make-anotes-live-note :id id :tags tags :context context :annotation annotation :pos-type ANOTES-CHAR-POS :start-pos start :end-pos end :label label :file-type type))
      )

    live-note
    )
  )

(defun anotes--new-note-in-pdf-buffer ()
  "Create a new piece of note in a pdf buffer."
  (user-error "Not Implemented!")
  )

(defvar anotes--overlays (ht-create)
  "All overlays created in current buffer. Key is note id and value is an overlay.")
(make-variable-buffer-local 'anotes--overlays)

(defvar anotes--buffer-notes (ht-create)
  "All live notes in current buffer. Key is note id and value is an `anotes-live-note'.")
(make-variable-buffer-local 'anotes--buffer-notes)

(defvar anotes--buffer-info nil
  "`anotes-buffer-info' of current buffer.")
(make-variable-buffer-local 'anotes--buffer-info)

(defvar anotes--label-notes (ht-create)
  "All notes loaded. Key is label, value is a hash table too, whose key is filename/url and value is hashtable,
in which, key is id, and value is `anotes-note'.")

(defun anotes--save-or-update-note (live-note label uri)
  (let (label-notes id file-notes)
    (setq id (anotes-live-note-id live-note))
    (setq label-notes (ht-get anotes--label-notes label))
    (unless label-notes
      (setq label-notes (ht-create)))
    (setq file-notes (ht-get label-notes uri))
    (unless file-notes
      (setq file-notes (ht-create)))

    (ht-set file-notes id (anotes-from-live-note live-note))
    (ht-set label-notes uri file-notes)
    (ht-set anotes--label-notes label label-notes)
    )
  )

;; (defun anotes--delete-buffer-notes (&optional label uri)
;;   (let ((label (or label (anotes-buffer-info-label anotes--buffer-info)))
;;         (uri (or uri (anotes-buffer-info-uri anotes--buffer-info)))
;;         label-notes)
;;     (setq label-notes (ht-get anotes--label-notes label))
;;     (when label-notes
;;       (ht-remove label-notes uri))))

(defun anotes--delete-note (id &optional redisplay)
  (let (label-notes file-notes uri label ov)
    (setq uri (anotes-buffer-info-uri anotes--buffer-info))
    (setq label (anotes-buffer-info-label anotes--buffer-info))
    (setq label-notes (ht-get anotes--label-notes label))
    (when label-notes
      (setq file-notes (ht-get label-notes uri))
      (when file-notes
        (ht-remove file-notes id)
        (ht-set label-notes uri file-notes)
        (ht-set anotes--label-notes label label-notes)))
    (ht-remove anotes--buffer-notes id)

    (anotes--save-same-label label)
    
    (when redisplay
      (setq ov (ht-get anotes--overlays id))
      (when ov
        (delete-overlay ov)))))

(defun anotes--redisplay-note (live-note)
  "Display note using overlay in text buffer."
  (let (start end text id label old-ov)
    (setq start (anotes-live-note-start-pos live-note))
    (setq end (anotes-live-note-end-pos live-note))
    (setq text (anotes-live-note-annotation live-note))
    (setq id (anotes-live-note-id live-note))

    (setq old-ov (ht-get anotes--overlays id))
    (when old-ov
      (delete-overlay old-ov)
      (ht-remove anotes--overlays id)
      )

    (anotes--add-overlay id start end text)
    )
  )

(defun anotes--add-overlay (id start end text)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'before-string (propertize "o" 'display (list (list 'margin 'right-margin) (propertize text 'face 'anotes-annotation))))
    (overlay-put ov 'face 'anotes-highlight)
    (overlay-put ov 'help-echo text)
    (overlay-put ov :id id)
    (overlay-put ov :type 'anotes)

    (ht-set anotes--overlays id ov)
    )
  )

(defun anotes--clear ()
  (anotes--remove-all-overlays)
  (setq anotes--buffer-info nil)
  (setq anotes--buffer-notes (ht-create))
  )

(defun anotes--remove-all-overlays ()
  (remove-overlays)
  (setq anotes--overlays (ht-create))
  )

(defsubst anotes--overlays-in (beg end)
  "Get a list of anotes overlays between beg and end points."
  (let (L)
    (dolist (ov (overlays-in beg end))
      (when (eq (overlay-get ov :type)
                'anotes)
        (push ov L)))
    L))

(defun anotes--get-overlays-at-point ()
  (let ((pos (point))
        overlays)
    (setq overlays (anotes--overlays-in pos pos))

    overlays
    )
  )

(defun anotes--edit-note (id &optional redisplay)
  (let ((live-note (ht-get anotes--buffer-notes id))
        old-tags old-annotation
        new-tags new-annotation
        changed
        (label (anotes-buffer-info-label anotes--buffer-info))
        (uri (anotes-buffer-info-uri anotes--buffer-info))
        )
    (when live-note
      (setq old-tags (anotes-live-note-tags live-note))
      (setq old-annotation (anotes-live-note-annotation live-note))
      (setq new-tags (read-string "Tags: " old-tags nil old-tags))
      (setq new-annotation (read-string "Annotation: " old-annotation nil old-annotation))
      (when (not (s-equals? old-tags new-tags))
        (setq changed t)
        (setf (anotes-live-note-tags live-note) new-tags)
        )
      (when (not (s-equals? old-annotation new-annotation))
        (setq changed t)
        (setf (anotes-live-note-annotation live-note) new-annotation)
        )

      (when changed
        (anotes--save-or-update-note live-note label uri)
        (anotes--save-same-label label)
        (when redisplay
          (anotes--redisplay-note live-note)
          )
        )
      )
    )
  )

(defun anotes-delete-note-at-point ()
  "Delete existing live-note at point."
  (interactive)
  (when (not anotes-local-mode)
    (user-error "Enable anotes-local-mode first.")
    )
  (let (id live-note label)
    (setq label (anotes-buffer-info-label anotes--buffer-info))
    (dolist (ov (anotes--get-overlays-at-point))
      (setq id (overlay-get ov :id))
      (anotes--delete-note id t)
      )
    )
  )

(defun anotes-add-note ()
  "Add a new piece of note, save it to a note file, and display it in current buffer if possible."
  (interactive)
  (when (not anotes-local-mode)
    (user-error "Enable anotes-local-mode first."))
  (let ((buffer-info anotes--buffer-info)
        label
        live-note
        id
        uri
        type
        anote-file)
    (setq type (anotes-buffer-info-type buffer-info))
    (cond
     ((or (eq type 'text)
          (eq type 'local-webpage)
          (eq type 'remote-webpage)
          )
      (setq label (anotes-buffer-info-label buffer-info))
      (setq uri (anotes-buffer-info-uri buffer-info))
      (setq live-note (anotes--new-note-in-text-buffer type))
      (when live-note
        (setq id (anotes-live-note-id live-note))
        (ht-set anotes--buffer-notes id live-note)

        (anotes--save-or-update-note live-note label uri)

        (anotes--save-same-label label)

        (anotes--redisplay-note live-note)
        )
      )
     (t
      (user-error "Not supported."))
     )
    )
  )

(defun anotes--recover-anotes (data)
  "Load notes of a single file."
  (let ((file (plist-get data :file))
        (label (plist-get data :label))
        (notes (plist-get data :notes))
        live-note
        live-notes
        uri
        )
    (setq anotes--buffer-notes (ht-create))
    (setq uri (f-expand file (anotes--directory-match-label label)))
    (message "uri %s" uri)
    (dolist (note notes)
      (setq live-note (anotes-to-live-note note))
      (message "live-note %S" live-note)
      (anotes--save-or-update-note live-note label uri)
      )
    )
  )

(defun anotes--load-same-label (label)
  "Load notes of label current buffer belongs to."
  (let (label)
    (when (not (ht-contains? anotes--label-notes label))
      (anotes--load-label-notes label)
      )
    )
  )

(defun anotes--load-label-notes (label)
  (let (anote-dir anote-file)
    (setq anote-dir (anotes--note-file-directory-match-label label))
    (setq anote-file (anotes--anote-file-name label anote-dir))
    (when (f-exists-p anote-file)
      (load-file anote-file))))

(defun anotes--retrieve-file-notes ()
  (let (label-notes file-notes label uri id)
    (setq anotes--buffer-notes (ht-create))
    (setq label (anotes-buffer-info-label anotes--buffer-info))
    (setq uri (anotes-buffer-info-uri anotes--buffer-info))
    (setq label-notes (ht-get anotes--label-notes label))
    (when label-notes
      (setq file-notes (ht-get label-notes uri))
      (when file-notes
        (dolist (note (ht-values file-notes))
          (setq id (anotes-note-id note))
          (ht-set anotes--buffer-notes id (anotes-to-live-note note)))))))

(defun anotes--redisplay-file-notes ()
  (anotes--remove-all-overlays)
  (dolist (live-note (ht-values anotes--buffer-notes))
    (anotes--redisplay-note live-note)))

(defun anotes--write-to-file (anote-file notes uri &optional append)
  ""
  (let ((label (anotes-buffer-info-label anotes--buffer-info))
        (filedir (anotes-buffer-info-filedir anotes--buffer-info))
        (writer #'f-write-text)
        relative-file)
    (when notes
      (if (anotes--remote-webpage uri)
          (setq relative-file uri)
        (setq relative-file (f-relative uri filedir)))
      (setq writer #'f-append-text)
      (funcall writer
               (format "\(anotes--recover-anotes '\(
:label \"%s\"
:file \"%s\"
:notes \(
%s
)))\n\n" label relative-file (mapconcat (lambda (s) (format "%S" s)) notes "\n"))
               'utf-8-unix
               anote-file
               )))
  )

(defun anotes--save-same-label (&optional label)
  "Save all notes having same label with current buffer."
  (let ((label (or label (anotes-buffer-info-label anotes--buffer-info)))
        label-notes
        anote-dir
        anote-file
        uri
        file-notes
        )
    (setq anote-dir (anotes--note-file-directory-match-label label))
    (and (not (f-exists-p anote-dir))
         (f-mkdir anote-dir))
    (setq anote-file (anotes--anote-file-name label anote-dir))
    (and (f-exists-p anote-file)
         (f-delete anote-file))
    (setq label-notes (ht-get anotes--label-notes label))
    (when label-notes
      (dolist (uri (ht-keys label-notes))
        (setq file-notes (ht-get label-notes uri))
        (when (> (ht-size file-notes) 0)
          (anotes--write-to-file anote-file (ht-values file-notes) uri t)
          )
        )
      )
    )
  )

(defun anotes--save-buffer-hook ()
  (when anotes-local-mode
    (anotes--save-same-label)
    )
  )

(defvar anotes-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode anotes-local-mode
  "The minor mode for taking notes."
  :keymap anotes-mode-map
  (let ((buffer-info (anotes--buffer-info))
        type label)
    (if anotes-local-mode
        (progn
          (setq type (anotes-buffer-info-type buffer-info))
          (if (eq type 'unsupported)
              (progn
                (anotes-local-mode -1)
                (user-error "Not supported by anotes.")
                )
            (setq label (anotes-buffer-info-label buffer-info))
            (setq anotes--buffer-info buffer-info)

            (setq anotes--overlays (ht-create))
            (setq anotes--buffer-notes (ht-create))

            (setq right-margin-width anotes-right-margin-width)
            (set-window-margins (get-buffer-window (current-buffer)) 0 right-margin-width)

            (anotes--load-same-label label)
            (anotes--retrieve-file-notes)

            (add-hook 'after-save-hook #'anotes--save-buffer-hook nil t)
            (if (or (eq type 'local-webpage)
                    (eq type 'remote-webpage))
                (add-hook 'eww-after-render-hook #'anotes--redisplay-file-notes nil t)
              )
            (anotes--redisplay-file-notes)
            )
          )
      (anotes--save-same-label)
      (anotes--clear)

      (remove-hook 'after-save-hook #'anotes--save-buffer-hook t)

      (set-window-margins (get-buffer-window (current-buffer)) nil nil)
      )
    )
  )



(provide 'anotes)
