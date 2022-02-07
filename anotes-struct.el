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

(require 'cl-lib)
(require 'cl-generic)

(defconst ANOTES-CHAR-POS 0)
(defconst ANOTES-PERCENT-POS 1)

(cl-defstruct anotes-char-pos
  "Used to represent position in text files."
  (line) ;; 1 based
  (column) ;; 0 based
  )

(cl-defstruct anotes-percent-pos
  "Used to represent position in non-text files(such as PDF files)."
  (percent)
  )

(cl-defstruct anotes-position
  (type)
  (char-pos)
  (percent-pos)
  )

(cl-defstruct anotes-note
  (id)
  (tags)
  (context)
  (note)
  (note-file)
  (start)
  (end)
  )
