;;; bibtex-completion-ok.el --- bibtex-completion-ok  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2025 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/bibtex-completion-ok
;; Version: 0.2.6
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1") (bibtex-completion "1.0.0") (dash "2.20.0") (mulex "0.1.3") (s "1.13.1"))
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
;; An extension to `bibtex-completion' for rendering references in the Chicago
;; styles.
;;
;; See The Chicago Manual of Style (17th ed.) for the guidelines.
;;
;;; Code:

(require 'bibtex-completion)
(require 'dash)
(require 'mulex)
(require 's)

(defcustom bibtex-completion-value-getter #'bibtex-completion-chicago-get-value
  "The value getter for reference formatting."
  :group 'bibtex-completion
  :type 'function)

(defun bibtex-completion-when-entry (entry fields &rest body)
  "When ENTRY has values in FIELDS, evaluate BODY.
FIELDS is a plist with keys `:has' and/or `:has-not' with their values holding
associated field names as symbols."
  (declare (indent 2))
  (let* ((has (let ((x (plist-get fields :has)))
                (--map (if (symbolp it) (symbol-name it) it)
                       (if (listp x) x (list x)))))
         (has-not (let ((x (plist-get fields :has-not)))
                    (--map (if (symbolp it) (symbol-name it) it)
                           (if (listp x) x (list x)))))
         result)
    (when has
      (setq result
            (--map
             (if-let* ((s (funcall bibtex-completion-value-getter it entry))
                       (_ (not (string-empty-p s))))
                 s)
             has)))
    (when has-not
      (setq result
            (append
             result
             (--map
              (null
               (if-let* ((s (funcall bibtex-completion-value-getter it entry))
                         (_ (not (string-empty-p s))))
                   s))
              has-not))))
    (when (and result (eval `(and ,@result)))
      (car (last (mapc #'eval body))))))

(defmacro bibtex-completion-chicago-format--pcase (variant &rest cases)
  "The wrapper for pcase of CASES given VARIANT.
This wrapper makes a few functions available via let-bindings."
  (declare (indent 1))
  `(cl-flet ((comma () (mulex-case ('ja "") (_ comma)))
             (period () (mulex-case ('ja "") (_ period)))
             (space () (mulex-case ('ja "") (_ space))))
     (pcase variant
       ,@cases)))

(defmacro bibtex-completion-chicago-format--article ()
  "Format the article reference."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat "${author}" (comma)
              (format dq "${title}") (comma) (space)
              (format it "${journaltitle}") space
              "${volume}" comma "no. ${number}" (space)
              (format paren "${date}") colon "${pages}"
              (when (bibtex-completion-get-value "doi" entry)
                (concat comma "${doi}"))))
     (_
      (concat "${author/i}" (period)
              (format dq "${title}") (period) (space)
              (format it "${journaltitle}") space
              "${volume}" comma "no. ${number}" (space)
              (format paren "${date}") colon "${pages}" period
              (when (bibtex-completion-get-value "doi" entry)
                "${doi}")))))

(defmacro bibtex-completion-chicago-format--article-magazine ()
  "Format the magazine article reference."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat
       "${author-or-editor}" (comma)
       (format dq "${title}") (comma)
       (format it "${journaltitle}") (comma) "${date}"
       (if (bibtex-completion-get-value "url" entry)
           (concat comma "${url}")
         (when (bibtex-completion-get-value "pages" entry)
           (concat comma "${pages}")))))
     (_
      (concat "${author-or-editor/i}" (period)
              (format dq "${title}") (period)
              (format it "${journaltitle}") (comma) "${date}"
              (if (bibtex-completion-get-value "url" entry)
                  (concat comma "${url}")
                (when (bibtex-completion-get-value "pages" entry)
                  (concat comma "${pages}")))))))

(defmacro bibtex-completion-chicago-format--article-newspaper ()
  "Format the newspaper article reference."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat "${author-or-editor}" (comma)
              (format dq "${title}") (comma)
              (format it "${journaltitle}") (comma) "${date}"
              (when (bibtex-completion-get-value "url" entry)
                (concat comma "${url}"))))
     (_
      (concat "${author-or-editor/i}" (period)
              (format dq "${title}") (period)
              (format it "${journaltitle}") (comma) "${date}"
              (if (bibtex-completion-get-value "url" entry)
                  (concat period "${url}"))))))

(defmacro bibtex-completion-chicago-format--book ()
  "Format the book reference."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year
      (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat
       "${author-or-editor}"
       (bibtex-completion-when-entry entry '(:has editor :has-not author)
         (mulex-s ", ed." '((ja . "編"))))
       (comma)
       (format it "${title}") (comma)
       (s-join
        comma
        (-non-nil
         (list
          (bibtex-completion-when-entry entry '(:has (author editor))
            (mulex-case ('ja "${editor}編")
                        (_ "ed. ${editor}")))
          (bibtex-completion-when-entry entry '(:has translator)
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
       (bibtex-completion-when-entry entry '(:has (author editor))
         (concat (mulex-case ('ja "${editor}編")
                             (_ "Edited by ${editor}"))
                 period))
       (bibtex-completion-when-entry entry '(:has translator)
         (concat (mulex-case ('ja "${translator}訳")
                             (_ "Translated by ${translator}"))
                 period))
       (mulex-case ('ja "")
                   (_ (concat "${location}" colon)))
       "${publisher}" comma "${year}"))))

(defmacro bibtex-completion-chicago-format--mvbook ()
  "Format the multi-volume book reference."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year
      (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat
       "${author-or-editor}"
       (bibtex-completion-when-entry entry '(:has editor :has-not author)
         (mulex-s ", ed." '((ja . "編"))))
       (comma)
       (format it "${title}") (comma)
       (s-join
        comma
        (-non-nil
         (list
          (bibtex-completion-when-entry entry '(:has (author editor))
            (mulex-case ('ja "${editor}編")
                        (_ "ed. ${editor}")))
          (bibtex-completion-when-entry entry '(:has translator)
            (mulex-case ('ja "${translator}訳")
                        (_ "trans. ${translator}")))
          (mulex-case ('ja "第${volume}巻")
                      (_ "vol. ${volume}")))))
       (space)
       (format paren
               (concat (mulex-case ('ja "")
                                   (_ (concat "${location}" colon)))
                       "${publisher}" comma "${year}"))))
     (_
      (concat
       "${author-or-editor/i}" (period)
       (format it "${title}") (comma)
       (mulex-case ('ja "第${volume}巻")
                   (_ "vol. ${volume}"))
       period
       (bibtex-completion-when-entry entry '(:has (author editor))
         (concat (mulex-case ('ja "${editor}編")
                             (_ "Edited by ${editor}"))
                 period))
       (bibtex-completion-when-entry entry '(:has translator)
         (concat (mulex-case ('ja "${translator}訳")
                             (_ "Translated by ${translator}"))
                 period))
       (mulex-case ('ja "")
                   (_ (concat "${location}" colon)))
       "${publisher}" comma "${year}"))))

(defmacro bibtex-completion-chicago-format--incollection ()
  "Format the section in a title reference."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year
      (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat
       "${author}" comma
       (format dq "${title}") comma
       (mulex-case
        ('ja (concat "${editor}編" (format it "${title}") "所収"))
        (_ (concat "in " (format it "${title}") comma "ed. ${editor}")))
       (space)
       (format paren (concat "${location}" colon "${publisher}" comma
                             "${year}"))))
     (_
      (concat
       "${author/i}" period
       (format dq "${title}") period
       (mulex-case
        ('ja (concat "${editor}編" (format it "${title}") "所収"))
        (_ (s-join
            comma
            (-non-nil
             (list (concat "In " (format it "${title}"))
                   "edited by ${editor}"
                   (bibtex-completion-when-entry entry '(:has pages)
                     "${pages}"))))))
       period
       "${location}" colon "${publisher}" comma "${year}"))))

(defmacro bibtex-completion-chicago-format--online ()
  "Fromat the online reference.
This includes web page item and blog item in Zotero."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year
      (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat
       "${author-or-editor}" (comma)
       (format dq "${title}") (comma)
       (concat (format it "${organization}")
               (bibtex-completion-when-entry entry '(:has type)
                 (concat (space) (format paren "${type}")))
               (comma))
       (concat (or (bibtex-completion-when-entry entry '(:has date)
                     (mulex-s "last modified ${date}"
                              '((ja . "最終更新日：${date}"))))
                   (mulex-s "accessed ${urldate}"
                            '((ja . "アクセス日：${urldate}"))))
               comma "${url}")))
     (_
      (concat
       "${author-or-editor/i}" (period)
       (format dq "${title}") (period)
       (concat (format it "${organization}")
               (bibtex-completion-when-entry entry '(:has type)
                 (concat (space) (format paren "${type}")))
               (period))
       (concat (or (bibtex-completion-when-entry entry '(:has date)
                     (mulex-s "last modified ${date}"
                              '((ja . "最終更新日：${date}"))))
                   (mulex-s "accessed ${urldate}"
                            '((ja . "アクセス日：${urldate}"))))
               comma "${url}")))))

(defmacro bibtex-completion-chicago-format--podcast ()
  "Format the podcast reference."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year
      (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat
       "${author-or-editor}" (comma)
       (format dq "${title}") (comma)
       "${date}" (comma)
       "in " (format it "${seriestitle}") (comma)
       (concat (space) "podcast") (comma)
       "${url}"))
     (_
      (concat
       "${author-or-editor/i}" (period)
       (format dq "${title}") (period)
       "${date}" (period)
       "In " (format it "${seriestitle}") (period)
       (concat (space) "Podcast") (period)
       "${url}"))))

(defmacro bibtex-completion-chicago-format--video-tvbroadcast ()
  "Format the TV broadcast reference.
This also fomats YouTube video, for example."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year
      (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat
       (bibtex-completion-when-entry entry '(:has author-or-editor)
         "${author-or-editor}" (comma))
       (bibtex-completion-when-entry entry '(:has booktitle)
         (format it "${booktitle}") (comma))
       (format dq "${title}") (comma)
       (mulex-case
        ('ja (concat "${publisher}" comma "${date}放送"))
        (_ (concat "${date}" (comma) "on ${publisher}")))
       (bibtex-completion-when-entry entry '(:has url)
         (concat comma "${url}"))))
     (_
      (concat
       (bibtex-completion-when-entry entry '(:has 'author-or-editor/i)
         "${author-or-editor/i}" (period))
       (bibtex-completion-when-entry entry '(:has booktitle)
         (format it "${booktitle}") (period))
       (format dq "${title}") (period)
       (mulex-case
        ('ja (concat "${publisher}" comma "${date}放送"))
        (_ (concat "${date}" (comma) "on ${publisher}")))
       (bibtex-completion-when-entry entry '(:has url)
         (concat period "${url}"))))))

(defmacro bibtex-completion-chicago-format--video-video ()
  "Format the video reference."
  '(bibtex-completion-chicago-format--pcase variant
     ('author-year (concat "${author-or-editor/s}" space "${year}"))
     ('author-in-text-year
      (concat "${author-or-editor/s}" (space) (format paren "${year}")))
     ('note
      (concat
       "${author-or-editor}" (comma)
       (format dq "${title}") (comma)
       (concat "${date}"
               (bibtex-completion-when-entry entry '(:has place)
                 (space) "in ${place}")
               (comma))
       (concat "video"
               (bibtex-completion-when-entry entry '(:has running-time)
                 (concat (comma) "${running-time}"))
               (comma))
       (bibtex-completion-when-entry entry '(:has url)
         "${url}")))
     (_
      (concat
       "${author-or-editor/i}" (period)
       (format dq "${title}") (period)
       (concat "${date}"
               (bibtex-completion-when-entry entry '(:has place)
                 (space) "In ${place}")
               (period))
       (concat "Video"
               (bibtex-completion-when-entry entry '(:has running-time)
                 (concat (comma) "${running-time}"))
               (period))
       (bibtex-completion-when-entry entry '(:has url)
         "${url}")))))

(defcustom bibtex-completion-chicago-lang-default "en"
  "Default language (langid).")

(defun bibtex-completion-chicago-format-reference (key &optional variant)
  "Return a plain text reference in Chicago format for KEY.
VARIANT may be `author-year', `author-in-text-year', `note', or
`bibliography' (default)."
  (let* ((entry (bibtex-completion-get-entry key))
         (entry-type (downcase (bibtex-completion-get-value "=type=" entry)))
         (entry-subtype
          (if-let* ((s (bibtex-completion-get-value "entrysubtype" entry)))
              (downcase s)))
         (lang (alist-get (replace-regexp-in-string
                           "\\`{\\([^}].*\\)}\\'" "\\1"
                           (bibtex-completion-get-value
                            "langid" entry bibtex-completion-chicago-lang-default))
                          mulex-languages nil nil #'equal))
         (space (mulex-s " " '((ja . " "))))
         (comma (mulex-s ", " '((ja . "、"))))
         (period (mulex-s ". " '((ja . "。"))))
         (colon (mulex-s ": " '((ja . "："))))
         (paren (mulex-s "(%s)" '((ja . "（%s）"))))
         (dq (mulex-s "“%s”" '((ja . "「%s」"))))
         (it (mulex-s (if (derived-mode-p 'org-mode) "/%s/" "%s")
                      '((ja . "『%s』"))))
         (s-tmpl
          (pcase variant
            ('title-only "${title}")
            (_ (pcase (s-join ":" (-non-nil (list entry-type entry-subtype)))
                 ("article" (bibtex-completion-chicago-format--article))
                 ("article:magazine" (bibtex-completion-chicago-format--article-magazine))
                 ("article:newspaper" (bibtex-completion-chicago-format--article-newspaper))
                 ("book" (bibtex-completion-chicago-format--book))
                 ("mvbook" (bibtex-completion-chicago-format--mvbook))
                 ("incollection" (bibtex-completion-chicago-format--incollection))
                 ("online" (bibtex-completion-chicago-format--online))
                 ("podcast" (bibtex-completion-chicago-format--podcast))
                 ("video:tvbroadcast" (bibtex-completion-chicago-format--video-tvbroadcast))
                 ("video:video" (bibtex-completion-chicago-format--video-video))
                 (_ "${title}"))))))
    (string-trim
     (replace-regexp-in-string
      "[.]+" "."
      (replace-regexp-in-string
       "\s+" space
       (replace-regexp-in-string ; enforce American-style comma/period location
        "\\([”\"']\\)\\([.,]\\)" "\\2\\1"
        (s-format s-tmpl
                  (lambda (field extra)
                    (let-alist extra
                      (funcall bibtex-completion-value-getter
                               field .entry nil .variant .lang)))
                  (list (cons 'entry entry)
                        (cons 'variant variant)
                        (cons 'lang lang)))))))))

(defun bibtex-completion-chicago-get-value (field entry &optional default variant lang)
  "Return the FIELD value from ENTRY formatted following the Chicago style.
If the field value is nil, DEFAULT or an empty string (if DEFAULT is nil) is
returned. VARIANT and LANG are passed to the formatter function."
  (or
   (let-alist
       (list (cons 'entry entry) (cons 'variant variant) (cons 'lang lang))
     (cl-flet
         ((get-value (field &optional default)
            (when-let*
                ((value (bibtex-completion-get-value field .entry default)))
              (replace-regexp-in-string
               "{{\\([^}]*\\)}}" "\\1"
               (replace-regexp-in-string
                "\\\\&" "&"
                (replace-regexp-in-string
                 "\\`{\\([^}].*\\)}\\'" "\\1" value)))))
          (format-names (value lang &optional invert-first variant surname-only)
            (bibtex-completion-chicago-names-format
             value lang invert-first variant surname-only)))
       (pcase field
         ("author/i"
          (format-names (get-value "author") .lang t .variant))
         ("author-or-editor"
          (if-let* ((value (get-value "author")))
              (format-names value .lang nil .variant)
            (if-let* ((value (get-value "editor")))
                (format-names value .lang nil .variant))))
         ("author-or-editor/i"
          (if-let* ((value (get-value "author")))
              (format-names value .lang t .variant)
            (if-let* ((value (get-value "editor")))
                (format-names value .lang t .variant))))
         ("author-or-editor/s"
          (if-let* ((value (get-value "author")))
              (format-names value .lang t .variant 'surname-only)
            (if-let* ((value (get-value "editor")))
                (format-names value .lang t .variant 'surname-only))))
         (_
          (if-let* ((value (get-value field)))
              (pcase field
                ("author" (format-names value .lang nil .variant))
                ("editor" (format-names value .lang nil .variant))
                ("translator" (format-names value .lang nil .variant))
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
              ("year" (car (split-string (get-value "date" "") "-")))
              ("journal" (get-value "journaltitle" ""))))))))
   default
   ""))

(defun bibtex-completion-chicago-names--parse (value &optional useprefix)
  "Parse VALUE from a person name field in a BibTex entry.
The function returns a list of cons `(surname . given-name)'.
USEPREFIX is a symbol (either `true' or `false')."
  (let* ((s (replace-regexp-in-string "[{}]" "" value)))
    (cl-flet*
        ((-parse (a)
           (let* ((ts (--map (let ((x (s-split "=" it t)))
                               (if (eq (length x) 2)
                                   (apply #'cons x)
                                 (car x)))
                             (s-split " *, *" a t)))
                  (up (alist-get "useprefix" ts nil nil #'equal)))
             (if (and up
                      (or (eq useprefix 'true)
                          (and (not (eq useprefix 'false))
                               (string= up "true"))))
                 (list (format "%s %s"
                               (alist-get "prefix" ts nil nil #'equal)
                               (alist-get "family" ts nil nil #'equal))
                       (alist-get "given" ts nil nil #'equal))
               (list (car ts) (cadr ts))))))
      (cl-loop for a in (s-split " and " s t)
               if (s-index-of "," a)
               collect (-parse a) into names
               else
               collect (cons a nil) into names
               finally return names))))

(defun bibtex-completion-chicago-names--concat (names etal)
  "Concatenate multiple NAMES into a string.
ETAL is an integer threshold above which the names will be abbreviated to et al."
  (let* ((len (length names))
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
     ((> len etal) (format abbrev (s-join ", " (take etal names))))
     ((= len 1) (car names))
     ((= len 2) (s-join delim-last names))
     (t (concat (s-join delim (-butlast names))
                (mulex-s "," '((ja . ""))) delim-last
                (-last-item names))))))

(defun bibtex-completion-chicago-names-format
    (value lang &optional invert-first variant surname-only)
  "Format names in VALUE in the Chicago style.
LANG is the language used to interpret the value, not necessarily the current
language used for rendering formatted reference.

When non-nil, INVERT-FIRST inverts the given-surname order of first entry.

VARIANT will be passed to the underlying functions to control name rendering.

When SURNAME-ONLY is non-nil, only surnames will be used."
  (cl-flet*
      ((contains-katakana-p (s) (string-match-p "[\u30A0-\u30FF]" s))
       (surname (n) (car n))
       (flatten (n)
         (let ((fn (cadr n)) (ln (car n)))
           (pcase lang
             ('ja (if (contains-katakana-p ln)
                      (concat (and fn (concat fn "・")) ln)
                    (concat ln fn)))
             (_ (concat (and fn (concat fn " ")) ln)))))
       (flatten-invert (n)
         (let ((fn (cadr n)) (ln (car n)))
           (pcase lang
             ('ja (if (contains-katakana-p ln)
                      (concat ln (and fn (concat "、" fn)))
                    (concat ln fn)))
             (_ (concat ln (and fn (concat ", " fn))))))))
    (when-let*
        ((names (bibtex-completion-chicago-names--parse value 'true))
         (len (length names))
         (names
          (append
           (list (let ((n (car names)))
                   (if surname-only (surname n)
                     (if invert-first (flatten-invert n) (flatten n)))))
           (--map (if surname-only (surname it) (flatten it))
                  (nthcdr 1 names)))))
      (cond
       ((and (member variant '(note author-year author-in-text-year))
             (> len 3))
        (bibtex-completion-chicago-names--concat names 1))
       ((< len 11) (bibtex-completion-chicago-names--concat names 10))
       (t (bibtex-completion-chicago-names--concat names 7))))))

;;;###autoload
(defun bibtex-completion-ok-insert-org-ref-link (&optional _arg)
  "Insert or update the reference description of a cite link.
When the point is on an existing link, the citekey is used to get the reference
key. Otherwise, the command will prompt for a key.

When the point is on an existing link, the description will be updated based on
the user request. Otherwise, a new link will be created.

Use the prefix argument to choose the style for description:

  - 0: Chicago Bibliography
  - 1: 'Authors Year'
  - 2: 'Authors (Year)'
  - 3: APA
  - 4: Prompt user for interactive selection
  - 5: 'Title'

The default is the Chicago Note style."
  (interactive "P")
  (when (not (featurep 'org-ref))
    (warn "`org-ref' is not available."))
  (let* ((collection '(("APA" . apa)
                       ("Chicago Note" . note)
                       ("Chicago Bibliography" . bibliography)
                       ("'Author Year'" . author-year)
                       ("'Author (Year)'" . author-in-text-year)
                       ("'Title'" . title-only)))
         (style
          (pcase (if (listp _arg) (car _arg) _arg)
            ('0 'bibliography)
            ('1 'author-year)
            ('2 'author-in-text-year)
            ('3 'apa)
            ('4 (alist-get (completing-read "Reference style: "
                                            collection nil t)
                           collection nil nil #'equal))
            ('5 'title-only)
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
            (org-ref-read-key))))
    ;; Delete the existing link before replacement.
    (when-let* ((start (car range)) (end (cadr range)))
      (delete-region start end)
      (goto-char start))

    (insert
     (format "[[cite:&%s][%s]]" key
             (pcase style
               ('apa (bibtex-completion-apa-format-reference key))
               (_ (bibtex-completion-chicago-format-reference key style)))))))

(provide 'bibtex-completion-ok)

;; Local Variables:
;; nameless-aliases: (("c" . "bibtex-completion-chicago"))
;; End:
;;; bibtex-completion-ok.el ends here
