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

(defcustom anotes-tmp-note-directory "~/.cache/anotes/"
  "The directory used to store notes temporarily."
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

(defcustom anotes-right-margin-width 30
  "The width of right margin to show annotation."
  :group 'anotes
  :type 'integer)

(defcustom anotes-recent-topic-count 5
  "The number of topics recently added or visited that should be saved."
  :group 'anotes
  :type 'integer)

(defcustom anotes-show-visible-area-anotes-count nil
  "When non-nil, show how many anotes in visible area of current buffer."
  :group 'anotes
  :type 'boolean)


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

(defsubst anotes--note-file-directory (filename)
  "Return xray file directory of filename"
  (let ((label-item (anotes--label-item-contain-filename filename)))
    (if label-item
        (f-slash (cddr label-item))
      (f-slash anotes-default-local-note-directory))))

(defsubst anotes--file-label (filename)
  "Return base directory of filename"
  (let ((label-item (anotes--label-item-contain-filename filename)))
    (if label-item
        (car label-item)
      anotes-default-local-label)))

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
      (f-slash anotes-default-local-note-directory))))

;; (defsubst anotes--directory-contain-file (filename)
;;   "Return base directory of filename"
;;   (let ((label-item (anotes--label-item-contain-filename filename)))
;;     (if label-item
;;         (f-slash (cadr label-item))
;;       "/"
;;       )))

(defsubst anotes--directory-match-label (label)
  "Return base directory of label"
  (let ((label-item (anotes--label-item-match-label label)))
    (if label-item
        (f-slash (cadr label-item))
      (if (s-equals? label anotes-default-remote-webpage-label)
          ""
        "/"
        ))))

;; (defsubst anotes--file-relative-path (filename)
;;   "Return relative path of filename."
;;   (let ((label-item (anotes--label-item-contain-filename filename)))
;;     (if label-item
;;         (s-chop-prefix (f-full (cadr label-item)) filename)
;;       (s-chop-prefix "/" filename))))

;; (defsubst anotes--note-file-relative-path (filename note-file-name)
;;   "Return relative path of filename to anotes--file-base-directory"
;;   (let ((label-item (anotes--label-item-contain-filename filename)))
;;     (if label-item
;;         (s-chop-prefix (f-full (cddr label-item)) note-file-name)
;;       (s-chop-prefix (f-full anotes-default-local-note-directory) note-file-name)
;;       )))

(defsubst anotes--current-time()
  ""
  (time-convert nil 'integer))

(defsubst anotes--current-time-readable()
  ""
  (format-time-string "%Y%m%d%H%M%S"))

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
        buffer-info uri label-item label anote-info)
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
      (setq buffer-info (make-anotes-buffer-info :type type :uri uri :anote-info anote-info)))

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

(defun anotes--remote-webpage-buffer? ()
  (with-current-buffer (current-buffer)
    (when (eq major-mode 'eww-mode)
      (let ((buffer-url (eww-current-url)))
        (anotes--remote-webpage buffer-url)))))

(defun anotes--remote-webpage-url ()
  (let ((buffer-url (eww-current-url)))
    buffer-url))

(defun anotes--local-webpage-buffer? ()
  (with-current-buffer (current-buffer)
    (eq major-mode 'eww-mode)))

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
      (setq live-note (make-anotes-live-note :id id :tags tags :context context :annotation annotation :type ANOTES-CHAR-POS :start start :end end))
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

(defvar anotes--live-notes (ht-create)
  "All live notes in current buffer. Key is note id and value is an `anotes-live-note'.")
(make-variable-buffer-local 'anotes--live-notes)

(defvar anotes--buffer-info nil
  "`anotes-buffer-info' of current buffer.")
(make-variable-buffer-local 'anotes--buffer-info)

(defvar anotes--label-live-notes (ht-create)
  "All notes loaded. Key is label, value is a hash table too, whose key is filename/url and value is hashtable,
in which, key is id, and value is `anote-live-note'.")

(defun anotes--save-or-update-note (live-note label uri)
  (let (label-notes id file-notes uri)
    (setq id (anotes-live-note-id live-note))
    (setq label-notes (ht-get anotes--label-live-notes label))
    (unless label-notes
      (setq label-notes (ht-create)))
    (setq file-notes (ht-get label-notes uri))
    (unless file-notes
      (setq file-notes (ht-create)))

    (ht-set file-notes id live-note)
    (ht-set label-notes uri file-notes)
    (ht-set anotes--label-live-notes label label-notes)
    )
  )

(defun anotes--delete-buffer-notes (&optional label uri)
  (let ((label (or label (anotes-anote-info-label (anotes-buffer-info-anote-info anotes--buffer-info))))
        (uri (or uri (anotes-buffer-info-uri anotes--buffer-info)))
        label-notes)
    (setq label-notes (ht-get anotes--label-live-notes label))
    (when label-notes
      (ht-remove label-notes uri))))

(defun anotes--delete-note (id)
  (let (label-anotes live-note)
    (setq live-note (ht-get anotes--live-notes id))
    (when live-note
      (setq label (anotes-live-note-label live-note))
      (setq label-anotes (ht-get anotes--label-live-notes label))
      (when label-anotes
        (ht-remove label-anotes id)
        (ht-set anotes--label-live-notes label label-anotes))
      (ht-remove anotes--live-notes id)))
  )

(defun anotes--display-note (live-note)
  "Display note using overlay in text buffer."
  (let (start end text id label)
    (setq start (anotes-live-note-start live-note))
    (setq end (anotes-live-note-end live-note))
    (setq text (anotes-live-note-annotation live-note))
    (setq id (anotes-live-note-id live-note))

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
  (setq anotes--live-notes (ht-create))
  )

(defun anotes--remove-all-overlays ()
  (setq anotes--overlays (ht-create))
  (remove-overlays)
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

(defun anotes-delete-note-at-point ()
  "Delete existing live-note at point."
  (interactive)
  (when (not anotes-local-mode)
    (user-error "Enable anotes-local-mode first.")
    )
  (let (id live-note label)
    (dolist (ov (anotes--get-overlays-at-point))
      (setq id (overlay-get ov :id))
      (anotes--delete-note id)
      (delete-overlay ov)
      )

    (anotes--save-to-tmp-file)
    )
  )

(defun anotes-add-note ()
  "Add a new piece of note, save it to a note file, and display it in current buffer if possible."
  (interactive)
  (when (not anotes-local-mode)
    (user-error "Enable anotes-local-mode first.")
    )
  (let ((buffer-info anotes--buffer-info)
        label
        note
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
      (setq label (anotes-anote-info-label (anotes-buffer-info-anote-info buffer-info)))
      (setq uri (anotes-buffer-info-uri buffer-info))
      (setq live-note (anotes--new-note-in-text-buffer))
      (when live-note
        (setq id (anotes-live-note-id live-note))
        (ht-set anotes--live-notes id live-note)

        (anotes--save-to-tmp-file)

        (anotes--save-or-update-note live-note label uri)

        (anotes--display-note live-note)
        )
      )
     (t
      (user-error "Not supported."))
     )
    )
  )

(defun anotes--recover-anotes (data)
  (let ((file (plist-get data :file))
        (label (plist-get data :label))
        (notes (plist-get data :notes))
        live-note
        live-notes
        full-file-name
        )
    (anotes--remove-all-overlays)
    (setq anotes--live-notes (ht-create))
    (setq full-file-name (f-expand file (anotes--directory-match-label label)))
    (when notes
      (dolist (note notes)
        (setq live-note (anotes-to-live-note note))
        (setq id (anotes-live-note-id live-note))
        (anotes--save-or-update-note live-note label full-file-name)
        )
      )
    )
  )

(defun anotes--recover-data ()
  "Load notes of current buffer from the tmp anote file."
  (let (label tmp-file uri)
    (setq uri (anotes-buffer-info-uri anotes--buffer-info))
    (setq label (anotes-anote-info-label (anotes-buffer-info-anote-info anotes--buffer-info)))
    (setq tmp-file (concat (f-full anotes-tmp-note-directory) (md5 uri) ".anote"))

    (anotes--delete-buffer-notes label uri)
    (when (f-exists-p tmp-file)
      (load-file tmp-file)
      )
    )
  )

(defun anotes-recover-data ()
  "Load notes of current buffer from the tmp anote file.
Called interactively to recover data."
  (interactive)
  (anotes--recover-data)
  )

(defun anotes--load-same-label ()
  "Load notes of label current buffer belongs to."
  (let ((label (anotes-anote-info-label (anotes-buffer-info-anote-info anotes--buffer-info)))
        (uri (anotes-buffer-info-uri anotes--buffer-info))
        (anote-dir (anotes-anote-info-anote-dir (anotes-buffer-info-anote-info anotes--buffer-info)))
        anote-file
        )
    (when (not (ht-contains? anotes--label-live-notes label))
      (setq anote-file (anotes--anote-file-name label anote-dir))
      (when (f-exists-p anote-file)
        (load-file anote-file))
      )
    )
  )

(defun anotes--restore-file-notes ()
  (let (label-notes file-notes label uri id)
    (setq label (anotes-anote-info-label (anotes-buffer-info-anote-info (anotes--buffer-info))))
    (setq uri (anotes-buffer-info-uri anotes--buffer-info))
    (setq label-notes (ht-get anotes--label-live-notes label))
    (setq anotes--live-notes (ht-create))
    (when label-notes
      (setq file-notes (ht-get label-notes uri))
      (when file-notes
        (dolist (note (ht-values file-notes))
          (setq id (anotes-live-note-id note))
          (ht-set anotes--live-notes id note))))))

(defun anotes--write-to-file (file-name live-notes &optional append)
  ""
  (let ((label (anotes-anote-info-label (anotes-buffer-info-anote-info anotes--buffer-info)))
        (uri (anotes-buffer-info-uri anotes--buffer-info))
        (filedir (anotes-anote-info-filedir (anotes-buffer-info-anote-info anotes--buffer-info)))
        (writer #'f-write-text)
        relative-file
        notes)
    (when live-notes
      (if (anotes--remote-webpage uri)
          (setq relative-file uri)
        (setq relative-file (f-relative uri filedir)))
      (setq notes (mapcar #'anotes-from-live-note (ht-values live-notes)))
      (setq writer #'f-append-text)
      (funcall writer
               (format "\(anotes--recover-anotes '\(
:label \"%s\"
:file \"%s\"
:notes \(
%s
)))\n\n" label relative-file (mapconcat (lambda (s) (format "%S" s)) notes "\n"))
               'utf-8-unix
               file-name
               )))
  )

(defun anotes--save-to-tmp-file ()
  "Save notes of current buffer to a file in /tmp.
Called whenever `anotes--live-notes' is modified."
  (let (label tmp-file)
    (unless (f-exists-p anotes-tmp-note-directory)
      (f-mkdir anotes-tmp-note-directory))
    (setq uri (anotes-buffer-info-uri anotes--buffer-info))
    (setq tmp-file (concat (f-full anotes-tmp-note-directory) (md5 uri) ".anote"))

    (and (f-exists-p tmp-file)
         (f-delete tmp-file))
    (anotes--write-to-file tmp-file anotes--live-notes nil)
    )
  )

(defun anotes--save-same-label (&optional label)
  "Save all notes having same label with current buffer."
  (let ((label (or label (anotes-anote-info-label (anotes-buffer-info-anote-info anotes--buffer-info))))
        label-notes
        anote-dir
        anote-file
        )
    (setq anote-dir (anotes-anote-info-anote-dir (anotes-buffer-info-anote-info anotes--buffer-info)))
    (and (not (f-exists-p anote-dir))
         (f-mkdir anote-dir))
    (setq anote-file (anotes--anote-file-name label anote-dir))
    (and (f-exists-p anote-file)
         (f-delete anote-file))
    (setq label-notes (ht-get anotes--label-live-notes label))
    (when label-notes
      (dolist (file-notes (ht-values label-notes))
        (anotes--write-to-file anote-file file-notes t)
        )
      )
    )
  )

(defun anotes--save-all ()
  "Save all notes."
  (dolist (label (ht-keys anotes--label-live-notes))
    (anotes--save-same-label label)))

(defun anotes-save-buffer-hook ()
  (when anotes-local-mode
    (anotes--save-to-tmp-file)
    )
  )

(defun anotes--kill-buffer-hook ()
  (when anotes-local-mode
    (anotes--save-to-tmp-file)
    (anotes--save-same-label)
    )
  )

(defun anotes--kill-emacs-hook ()
  (anotes--save-all)
  )

(defvar anotes-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-minor-mode anotes-local-mode
  "The minor mode for taking notes."
  :keymap anotes-mode-map
  (let ((buffer-info (anotes--buffer-info))
        type)
    (if anotes-local-mode
        (progn
          (setq type (anotes-buffer-info-type buffer-info))
          (if (eq type 'unsupported)
              (progn
                (anotes-local-mode -1)
                (user-error "Not supported by anotes.")
                )
            (setq anotes--buffer-info buffer-info)

            (setq anotes--overlays (ht-create))
            (setq anotes--live-notes (ht-create))

            (setq right-margin-width anotes-right-margin-width)
            (set-window-margins (get-buffer-window (current-buffer)) 0 right-margin-width)

            (anotes--load-same-label)
            (anotes--restore-file-notes)

            (add-hook 'after-save-hook #'anotes--save-to-tmp-file nil t)
            (add-hook 'kill-buffer-hook #'anotes--kill-buffer-hook nil t)
            (add-hook 'kill-emacs-hook #'anotes--kill-emacs-hook)
            )
          )
      (anotes--save-same-label)
      (anotes--clear)

      (remove-hook 'after-save-hook #'anotes--save-to-tmp-file t)
      (remove-hook 'kill-buffer-hook #'anotes--kill-buffer-hook t)

      (set-window-margins (get-buffer-window (current-buffer)) nil nil)
      )
    )
  )



(provide 'anotes)
