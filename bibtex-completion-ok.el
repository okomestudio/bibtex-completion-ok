;;; bibtex-completion-ok.el --- bibtex-completion-ok  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/bibtex-completion-ok
;; Version: 0.1.1
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'bibtex-completion)
(require 's)
(require 'dash)
(require 'mulex)

(defun bibtex-completion-chicago-format-reference (key &optional variant)
  "Return a plain text reference in Chicago format for KEY.
VARIANT may be `note', `bibliography' (default), `in-text'."
  (defun has-values-for-p (entry fields &optional not-fields)
    (let ((fields
           (--map (if (symbolp it) (symbol-name it) it)
                  (if (listp fields) fields (list fields))))
          (not-fields
           (--map (if (symbolp it) (symbol-name it) it)
                  (if (listp not-fields) not-fields (list not-fields))))
          results)
      (--map (push (bibtex-completion-get-value it entry) results)
             fields)
      (--map (push (null (bibtex-completion-get-value it entry)) results)
             not-fields)
      (eval `(and ,@results))))

  (defun ensure-american-style (&rest s)
    (replace-regexp-in-string "\\([”\"']\\)\\([.,]\\)" "\\2\\1"
                              (string-join s)))

  (let* ((entry (bibtex-completion-get-entry key))
         (entry-type (downcase (bibtex-completion-get-value "=type=" entry)))
         (entry-subtype
          (if-let* ((s (bibtex-completion-get-value "entrysubtype" entry)))
              (downcase s)))
         (lang (alist-get
                (replace-regexp-in-string
                 "\\`{\\([^}].*\\)}\\'" "\\1"
                 (bibtex-completion-get-value "langid" entry))
                mulex-languages nil nil #'equal))
         (space (mulex-s " " '((ja . " "))))
         (comma (mulex-s ", " '((ja . "、"))))
         (period (mulex-s ". " '((ja . "。"))))
         (colon (mulex-s ": " '((ja . "："))))
         (paren (mulex-s "(%s)" '((ja . "（%s）"))))
         (dq (mulex-s "“%s”" '((ja . "「%s」"))))
         (it (mulex-s (if (derived-mode-p 'org-mode) "/%s/" "%s")
                      '((ja . "『%s』"))))
         s-tmpl)
    (defun comma () (mulex-case ('ja "") (_ comma)))
    (defun period () (mulex-case ('ja "") (_ period)))
    (defun space () (mulex-case ('ja "") (_ space)))

    (defun author-year ()
      (concat "${author-or-editor}" space "${year}"))
    (defun author-in-text-year ()
      (concat "${author-or-editor}" (space) (format paren "${year}")))

    (setq
     s-tmpl
     (pcase (s-join ":" (-non-nil (list entry-type entry-subtype)))
       ("article:magazine"
        (pcase variant
          ('author-year (author-year))
          ('author-in-text-year (author-in-text-year))
          ('note
           (concat
            "${author-or-editor}" (comma)
            (ensure-american-style (format dq "${title}") (comma))
            (format it "${journaltitle}") (comma) "${date}"
            (if (bibtex-completion-get-value "url" entry)
                (concat comma "${url}")
              (when (bibtex-completion-get-value "pages" entry)
                (concat comma "${pages}")))))
          (_
           (concat "${author-or-editor/i}" (period)
                   (ensure-american-style (format dq "${title}") (period))
                   (format it "${journaltitle}") (comma) "${date}"
                   (if (bibtex-completion-get-value "url" entry)
                       (concat comma "${url}")
                     (when (bibtex-completion-get-value "pages" entry)
                       (concat comma "${pages}")))))))
       ("article:newspaper"
        (pcase variant
          ('author-year (author-year))
          ('author-in-text-year (author-in-text-year))
          ('note
           (concat "${author-or-editor}" (comma)
                   (ensure-american-style (format dq "${title}") (comma))
                   (format it "${journaltitle}") (comma) "${date}"
                   (when (bibtex-completion-get-value "url" entry)
                     (concat comma "${url}"))))
          (_
           (concat "${author-or-editor/i}" (period)
                   (ensure-american-style (format dq "${title}") (period))
                   (format it "${journaltitle}") (comma) "${date}"
                   (if (bibtex-completion-get-value "url" entry)
                       (concat period "${url}"))))))
       ("article"
        (pcase variant
          ('author-year (author-year))
          ('author-in-text-year (author-in-text-year))
          ('note
           (concat "${author}" (comma)
                   (ensure-american-style (format dq "${title}") (comma)) (space)
                   (format it "${journaltitle}") space
                   "${volume}" comma "no. ${number}" (space)
                   (format paren "${date}") colon "${pages}"
                   (when (bibtex-completion-get-value "doi" entry)
                     (concat comma "${doi}"))))
          (_
           (concat "${author/i}" (period)
                   (ensure-american-style (format dq "${title}") (period)) (space)
                   (format it "${journaltitle}") space
                   "${volume}" comma "no. ${number}" (space)
                   (format paren "${date}") colon "${pages}" period
                   (when (bibtex-completion-get-value "doi" entry)
                     "${doi}")))))
       ("book"
        (pcase variant
          ('author-year (author-year))
          ('author-in-text-year (author-in-text-year))
          ('note
           (concat
            "${author-or-editor}"
            (when (has-values-for-p entry 'editor 'author)
              (mulex-s ", ed." '((ja . "編"))))
            (comma)
            (format it "${title}") (comma)
            (s-join
             comma
             (-non-nil
              (list
               (when (has-values-for-p entry '(author editor))
                 (mulex-case ('ja "${editor}編")
                             (_ "ed. ${editor}")))
               (when (has-values-for-p entry 'translator)
                 (mulex-case ('ja "${translator}訳")
                             (_ "trans. ${translator}"))))))
            (space)
            (format paren
                    (concat (mulex-case ('ja "")
                                        (_ (concat "${location}" colon)))
                            "${publisher}" comma "${year}"))))
          (_
           (concat
            "${author-or-editor/i}" (period)
            (format it "${title}") (period)
            (when (has-values-for-p entry '(author editor))
              (concat (mulex-case ('ja "${editor}編")
                                  (_ "Edited by ${editor}"))
                      period))
            (when (has-values-for-p entry 'translator)
              (concat (mulex-case ('ja "${translator}訳")
                                  (_ "Translated by ${translator}"))
                      period))
            (mulex-case ('ja "")
                        (_ (concat "${location}" colon)))
            "${publisher}" comma "${year}"))))
       ("incollection"
        (pcase variant
          ('author-year (author-year))
          ('author-in-text-year (author-in-text-year))
          ('note
           (concat
            "${author}" comma
            (ensure-american-style (format dq "${title}") comma)
            (mulex-case
             ('ja (concat "${editor}編" (format it "${title}") "所収"))
             (_ (concat "in " (format it "${title}") comma "ed. ${editor}")))
            (space)
            (format paren (concat "${location}" colon "${publisher}" comma
                                  "${year}"))))
          (_
           (concat
            "${author/i}" period
            (ensure-american-style (format dq "${title}") period)
            (mulex-case
             ('ja (concat "${editor}編" (format it "${title}") "所収"))
             (_ (s-join
                 comma
                 (-non-nil
                  (list (concat "In " (format it "${title}"))
                        "edited by ${editor}"
                        (when (has-values-for-p entry 'pages) "${pages}"))))))
            period
            "${location}" colon "${publisher}" comma "${year}"))))
       ("online"              ; web page item and blog item in Zotero
        (pcase variant
          ('author-year (author-year))
          ('author-in-text-year (author-in-text-year))
          ('note
           (concat
            "${author-or-editor}" (comma)
            (ensure-american-style (format dq "${title}") (comma))
            (concat (format it "${organization}")
                    (when (has-values-for-p entry 'type)
                      (concat (space) (format paren "${type}")))
                    (comma))
            (concat (if (has-values-for-p entry 'date)
                        (mulex-s "last modified ${date}"
                                 '((ja . "最終更新日：${date}")))
                      (mulex-s "accessed ${urldate}"
                               '((ja . "アクセス日：${urldate}"))))
                    comma "${url}")))
          (_
           (concat
            "${author-or-editor/i}" (period)
            (ensure-american-style (format dq "${title}") (period))
            (concat (format it "${organization}")
                    (when (has-values-for-p entry 'type)
                      (concat (space) (format paren "${type}")))
                    (period))
            (concat (if (has-values-for-p entry 'date)
                        (mulex-s "last modified ${date}"
                                 '((ja . "最終更新日：${date}")))
                      (mulex-s "accessed ${urldate}"
                               '((ja . "アクセス日：${urldate}"))))
                    comma "${url}")))))
       ))

    (string-trim
     (replace-regexp-in-string
      "[.]+" "."
      (replace-regexp-in-string
       "\s+" space
       (s-format s-tmpl 'bibtex-completion-chicago-get-value
                 `(,entry ,variant ,lang)))))))

(defun bibtex-completion-chicago-get-value (field extra)
  "Return Field of ENTRY formatted following the Chicago style.
Return DEFAULT (empty string if undefined) if FIELD is not present in ENTRY."
  (defun get-value (field entry &optional default)
    (let ((value (bibtex-completion-get-value field entry default)))
      (when value
        (replace-regexp-in-string
         "{{\\([^}]*\\)}}" "\\1"
         (replace-regexp-in-string
          "\\\\&" "&"
          (replace-regexp-in-string "\\`{\\([^}].*\\)}\\'" "\\1" value))))))

  (or
   (let ((entry (nth 0 extra))
         (variant (nth 1 extra))
         (lang (nth 2 extra)))
     (pcase field
       ("author/i"
        (bibtex-completion-chicago-format-authors
         (get-value "author" entry) lang t variant))
       ("author-or-editor"
        (if-let* ((value (get-value "author" entry)))
            (bibtex-completion-chicago-format-authors value lang nil variant)
          (if-let* ((value (get-value "editor" entry)))
              (bibtex-completion-chicago-format-editors value lang nil variant))))
       ("author-or-editor/i"
        (if-let* ((value (get-value "author" entry)))
            (bibtex-completion-chicago-format-authors value lang t variant)
          (if-let* ((value (get-value "editor" entry)))
              (bibtex-completion-chicago-format-editors value lang t variant))))
       (_
        (let ((value (get-value field entry)))
          (if value
              (pcase field
                ("author" (bibtex-completion-chicago-format-authors value lang nil variant))
                ("editor" (bibtex-completion-chicago-format-editors value lang nil variant))
                ("translator" (bibtex-completion-chicago-format-translators value lang nil variant))
                ("title" value)
                ("journal" value)
                ("booktitle" value)
                ("pages" (s-join "–" (s-split "[^0-9]+" value t)))
                ("doi" (s-concat " https://doi.org/" value))
                ("year" value)
                ("date" (let ((dt (parse-time-string value)))
                          (mulex-date-format (decoded-time-year dt)
                                             (decoded-time-month dt)
                                             (decoded-time-day dt))))
                (_ value))
            ;; Handle common derived fields:
            (pcase field
              ("year" (car (split-string (get-value "date" entry "") "-")))
              ("journal" (get-value "journaltitle" entry ""))))))))
   ""))

(defun bibtex-completion-ok-names--parse (value)
  "Parse string VALUE into name list.
The function returns a list of cons `(last-name . first-name)'."
  (setq value (replace-regexp-in-string "[{}]" "" value))
  (cl-loop for a in (s-split " and " value t)
           if (s-index-of "," a)
           collect (s-split " *, *" a t) into names
           else
           ;; collect (s-split " " a t) into names
           collect (cons a nil) into names
           finally return names))

(defun bibtex-completion-ok-names--concat (names lang etal)
  "TBD."
  (let* ((l (length names))
         (abbrev (mulex-s "%s et al." '((ja . "%s他"))))
         (delim
          (mulex-s ", "
                   `((ja . ,(if (string-match-p "[\u30FB]" (s-join "" names))
                                "／" "・")))))
         (delim-last
          (mulex-s " and "
                   `((ja . ,(if (string-match-p "[\u30FB]" (s-join "" names))
                                "／" "・"))))))
    (cond
     ((> l etal) (format abbrev (s-join ", " (take etal names))))
     ((= l 1) (car names))
     ((= l 2) (s-join delim-last names))
     (t (concat (s-join delim (-butlast names))
                (mulex-s "," '((ja . ""))) delim-last
                (-last-item names))))))

(defun bibtex-completion-ok-names--format (names lang invert-first variant)
  "TBD."
  (defun contains-katakana-p (s)
    (string-match-p "[\u30A0-\u30FF]" s))

  (defun flatten (n)
    (let ((fn (cadr n)) (ln (car n)))
      (pcase lang
        ('ja (if (contains-katakana-p ln)
                 (concat (and fn (concat fn "・")) ln)
               (concat ln fn)))
        (_ (concat (and fn (concat fn " ")) ln)))))

  (defun flatten-invert (n)
    (let ((fn (cadr n)) (ln (car n)))
      (pcase lang
        ('ja (if (contains-katakana-p ln)
                 (concat ln (and fn (concat "、" fn)))
               (concat ln fn)))
        (_ (concat ln (and fn (concat ", " fn)))))))

  (let* ((l (length names))
         (names (append (list
                         (let ((n (car names)))
                           (if invert-first (flatten-invert n) (flatten n))))
                        (--map (flatten it) (nthcdr 1 names)))))
    (cond
     ((and (eq variant 'note) (> l 3)) (bibtex-completion-ok-names--concat names lang 1))
     ((< l 11) (bibtex-completion-ok-names--concat names lang 10))
     (t (bibtex-completion-ok-names--concat names lang 7)))))

(defun bibtex-completion-chicago-format-authors
    (value lang &optional invert-first variant loc)
  "Format author list in VALUE in Chicago style.
If FOR-NOTE is non-nil, generate author list for note."
  (let ((authors (bibtex-completion-ok-names--parse value)))
    (bibtex-completion-ok-names--format authors lang invert-first variant)))

(defun bibtex-completion-chicago-format-editors
    (value lang &optional invert-first variant)
  ""
  (let ((authors (bibtex-completion-ok-names--parse value)))
    (bibtex-completion-ok-names--format authors lang invert-first variant)))

(defun bibtex-completion-chicago-format-translators
    (value lang &optional invert-first variant)
  ""
  (let ((authors (bibtex-completion-ok-names--parse value)))
    (bibtex-completion-ok-names--format authors lang invert-first variant)))

;;;###autoload
(defun bibtex-completion-ok-insert-org-ref-link (&optional _arg)
  "Insert or update a cite link with a reference description.
When the point is on an existing link, the citekey is used to get the reference
key. Otherwise, the command will prompt for a key.

When the point is on an existing link, the description will be updated based on
the user request. Otherwise, a new link will be created.

Use the prefix argument to choose the style for description:

  - 1: 'Authors (Year)'
  - 2: 'Authors Year'
  - 3: APA
  - 4: Trigger prompt for interactive selection

The default style is 'Authors _Title_ (Publisher, Year)'."
  (interactive "P")
  (when (not (featurep 'org-ref))
    (warn "`org-ref' is not available."))

  (let* ((style
          (pcase (if (listp _arg) (car _arg) _arg)
            ('0 'bibliography)
            ('1 'author-year)
            ('2 'author-in-text-year)
            ('3 'apa)
            ('4 (let ((collection
                       '(("APA" . apa)
                         ("Chicago Note" . note)
                         ("Chicago Bibliography" . bibliography)
                         ("Authors Year" . author-year)
                         ("Authors (Year)" . author-in-text-year))))
                  (alist-get
                   (completing-read "Reference style: " collection nil t)
                   collection nil nil #'equal)))
            (_ 'note)))
         (range nil)
         (key
          (if-let*
              ((link (and (org-in-regexp org-link-any-re)
                          (substring-no-properties (match-string 0))))
               (start (match-beginning 0))
               (end (match-end 0))
               (key (and (string-match "^\\[\\[cite:&\\([^]]*\\)\\].*" link)
                         (match-string 1 link))))
              (progn
                (setq range (list start end))
                key)
            (org-ref-read-key)))
         (formatted
          (pcase style
            ('apa (bibtex-completion-apa-format-reference key))
            (_ (bibtex-completion-chicago-format-reference key style)))))
    ;; Delete the existing link before replacement.
    (when-let* ((start (car range)) (end (cadr range)))
      (delete-region start end)
      (goto-char start))

    (insert (format "[[cite:&%s][%s]]" key formatted))))

(provide 'bibtex-completion-ok)
;;; bibtex-completion-ok.el ends here
