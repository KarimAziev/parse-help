;;; parse-help.el --- Parse CLI help output -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/parse-help
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (vterm "0.0.2") (transient "0.3.7"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Parse CLI help output

;;; Code:


(require 'fp)
(require 'transient)


(defcustom parse-help-use-command-long-descriptions nil
  "Whether to use long command descriptions from help string."
  :group 'parse-help
  :type 'boolean)

(defvar parse-help-flags-regexp
  "\\[?\\(\\([-<][-]?[^\s\t\n>]+\\)[\s\t\n>][>]?\\)")


(defun parse-help-capitalize-variants (word)
  "Return list of words of WORD, but it with upcased letter."
  (let ((cands)
        (parts (split-string word "" t)))
    (dotimes (i (length parts))
      (let ((val (string-join (remove nil (list
                                           (when (> i 0)
                                             (string-join (seq-take parts i) ""))
                                           (upcase (nth i parts))
                                           (string-join (seq-drop parts (1+ i))
                                                        "")))
                              "")))
        (push val
              cands)))
    (reverse cands)))

(defun parse-help-safe-substring (len word)
  "Substring WORD from zero to LEN."
  (if (> (length word) len)
      (substring-no-properties word 0 len)
    word))

(defun parse-help--get-all-key-strategies (word len)
  "Generate preffered shortcut from WORD with length LEN."
  (let* ((parts (append (split-string word "[^a-zz-a]" t)
                        (list (replace-regexp-in-string "[^a-zz-a]" "" word))))
         (parts-len (length parts))
         (finalize (lambda (short)
                     (while (> len (length short))
                       (setq short (concat short (number-to-string (random 10)))))
                     (parse-help-safe-substring len short)))
         (vars
          (mapcar finalize (parse-help-capitalize-variants
                            (parse-help-safe-substring len
                                                       (replace-regexp-in-string
                                                        "[^a-zz-a]"
                                                        ""
                                                        word))))))
    (seq-sort-by
     (lambda (it)
       (cond ((string-match-p "[0-9]" it)
              -2)
             ((member it vars)
              -1)
             (t (length (parse-help-s-shared-start word it)))))
     #'>
     (seq-uniq (append
                vars
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'parse-help-safe-substring n)
                                      parts)))
                 (number-sequence 1 (min len parts-len)))
                (mapcar
                 (lambda (n)
                   (funcall finalize (mapconcat
                                      (apply-partially
                                       #'parse-help-safe-substring n)
                                      (reverse parts))))
                 (number-sequence 1 (min len parts-len))))))))

(defun parse-help--generate-shortcuts (items &optional key-fn value-fn)
  "Generate shortcuts from list of ITEMS.
If KEY-FN is nil, ITEMS should be list of strings or symbols.
If KEY-FN is a function, it will be called with every item of list, and should
return string that will be as basis for shortcut.
If VALUE-FN is nil, result is an alist of generated keys and corresponding
items.
If VALUE-FN is non nil, return a list of results of calling VALUE-FN with two
arguments - generated shortcut and item."
  (let* ((total (length items))
         (random-variants (append
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "a")
                                                    (string-to-char
                                                     "z")))
                           (mapcar #'char-to-string
                                   (number-sequence (string-to-char
                                                     "A")
                                                    (string-to-char
                                                     "Z")))
                           (mapcar #'number-to-string (number-sequence 0 9))))
         (variants-len (length random-variants))
         (min-len
          (cond ((>= variants-len total)
                 1)
                ((>= variants-len (/ total 2))
                 2)
                (t 3))))
    (let ((shortcuts '())
          (used '())
          (result))
      (dotimes (i (length items))
        (let* ((def (nth i items))
               (word (if key-fn
                         (funcall key-fn def)
                       (if (symbolp def)
                           (symbol-name def)
                         def))))
          (when (not (member word used))
            (push word used)
            (let ((short
                   (downcase
                    (substring-no-properties word 0
                                             (min min-len
                                                  (length word))))))
              (setq short (replace-regexp-in-string "[^a-z]" "" short))
              (setq short
                    (seq-find
                     (lambda (it)
                       (not
                        (seq-find (apply-partially
                                   #'string-prefix-p it)
                                  shortcuts)))
                     (append (parse-help--get-all-key-strategies word
                                                                 min-len)
                             (when (= min-len 1)
                               random-variants))))
              (while (and
                      (< (length short) min-len))
                (setq short (concat short (number-to-string (random 10)))))
              (push short shortcuts)
              (push (if value-fn
                        (funcall value-fn short def)
                      (cons short def))
                    result)))))
      (reverse result))))

(defcustom parse-help-transient-prefix ""
  "Prefix in generated commands."
  :type 'string
  :group 'parse-help)

(defun parse-help-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `parse-help-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun parse-help-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `parse-help-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun parse-help-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (with-syntax-table emacs-lisp-mode-syntax-table
    (when-let ((str-start (nth 8 (syntax-ppss (point)))))
      (goto-char str-start))
    (let ((init-pos (point))
          (pos)
          (count n))
      (while (and (not (= count 0))
                  (when-let ((end (ignore-errors
                                    (funcall fn)
                                    (point))))
                    (unless (= end (or pos init-pos))
                      (setq pos end))))
        (setq count (1- count)))
      (if (= count 0)
          pos
        (goto-char init-pos)
        nil))))

(defun parse-help-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'parse-help-re-search-backward-inner)
               ((> count 0) #'parse-help-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defmacro parse-help-with-temp-elisp-buffer (&rest body)
  "Execute BODY inside temporarily buffer in `emacs-lisp-mode' without hooks."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let (emacs-lisp-mode-hook) (emacs-lisp-mode))
     (progn
       ,@body)))

(defun parse-help-backward-up-list (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (parse-help-move-with 'backward-up-list arg))

(defun parse-help-transient-indent-vector ()
  "Indent vector."
  (while (parse-help-re-search-forward "\\[+" nil t 1)
    (when (looking-at "\"")
      (forward-char -1)
      (newline-and-indent)
      (forward-char)
      (while (parse-help-move-with 'forward-sexp)
        (newline-and-indent)))))

(defun parse-help--get-alphabete (&optional start-char n)
  "Return N letters from alphabete starting at START-CHAR.
Default value for START-CHAR is \"a\" and for N - 26."
  (unless n (setq n 26))
  (let ((start-char (if (stringp start-char)
                        (string-to-char start-char)
                      (or start-char
                          (string-to-char "a"))))
        (letters))
    (dotimes (i n)
      (let ((str (char-to-string (+ i start-char))))
        (push str letters)))
    (reverse letters)))

(defun parse-help-group-with (fn items &optional transform-fn)
  "Group ITEMS by calling FN with every item.
FN should return key.
TRANSFORM-FN should return transformed item."
  (seq-reduce (lambda (acc it)
                (let* ((key (funcall fn it))
                       (val (if transform-fn (funcall transform-fn it) it))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell)
                                          (list val))
                                (list val))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              (seq-copy items) '()))

(defun parse-help-s-shared-start (s1 s2)
  "Return the longest prefix S1 and S2 have in common."
  (declare (pure t) (side-effect-free t))
  (let ((search-length (min (length s1) (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i) (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun parse-help-transient-description-to-doc (description)
  "Format DESCRIPTION as documentation string."
  (when-let* ((parts
               (when description
                 (seq-drop-while #'string-empty-p
                                 (split-string
                                  (string-trim description) "[\n\r\f]"))))
              (first-line (pop parts)))
    (setq first-line (string-trim first-line))
    (setq first-line (if (and first-line
                              (not (or (string-suffix-p "." first-line)
                                       (string-suffix-p ":" first-line))))
                         (concat first-line ".")
                       first-line))
    (concat (capitalize (substring first-line 0 1))
            (substring first-line 1)
            (mapconcat
             (lambda (it)
               (setq it (string-trim it))
               (substring-no-properties
                (or
                 (when-let ((words
                             (when (>
                                    (length
                                     it)
                                    fill-column)
                               (split-string
                                it
                                nil
                                t))))
                   (seq-reduce
                    (lambda (acc
                             s)
                      (if
                          (>
                           (length
                            (concat
                             (car
                              (last
                               (split-string
                                acc
                                "\n"
                                t)))
                             s))
                           fill-column)
                          (concat
                           acc
                           "\n"
                           s)
                        (concat
                         acc
                         "\s"
                         s)))
                    words ""))
                 it)
                0
                (min fill-column (length it))))
             parts "\n"))))

(defun parse-help-alist-get (key alist)
  "Find the first element of ALIST whose car equals KEY and return its cdr."
  (cdr (assoc key alist)))

(defun parse-help-transient-normalize-command-name (cmd)
  "Remove arguments from CMD and replace whitespaces from CMD."
  (string-join
   (seq-take-while
    (fp-compose not
                (apply-partially #'string-prefix-p "-"))
    (split-string cmd nil t))
   "-"))

(defun parse-help-transient-render-sexps (sexps)
  "Insert SEXPS into the current buffer, formatted as Emacs Lisp code."
  (parse-help-with-temp-elisp-buffer
      (dolist (sexp sexps)
        (newline-and-indent)
        (if (not (symbolp (nth 0 sexp)))
            (insert (or (parse-help-transient-render-sexps sexp) ""))
          (insert "("
                  (symbol-name (nth 0 sexp))
                  " "
                  (symbol-name (nth 1 sexp))
                  " ()"
                  ")")
          (forward-char -1)
          (newline-and-indent)
          (insert (prin1-to-string (nth 3 sexp)))
          (let ((item)
                (items (seq-subseq sexp 4)))
            (while (setq item (pop items))
              (newline-and-indent)
              (if (keywordp item)
                  (insert (prin1-to-string item) " "
                          (if (and (eq item :choices)
                                   (not (eq (car-safe (car-safe items))
                                            'quote)))
                              "'"
                            "")
                          (prin1-to-string (pop items)))
                (insert (prin1-to-string item)))))
          (save-excursion
            (parse-help-backward-up-list)
            (parse-help-transient-indent-vector))
          (parse-help-prettify)
          (forward-char 1)))
      (replace-regexp-in-string "\\[[\n\s\t]+" "[" (buffer-string))))

(defun parse-help-prettify ()
  "Fix whitespaces in vectors."
  (save-excursion
    (goto-char (point-max))
    (while (re-search-backward "\\(\\[\\]\\|\\(\\]\\|)\\)[\n][\s]?+\\(\\]\\|)\\)\\)"
                               nil t 1)
      (if (looking-at "\\[\\]")
          (delete-char 2)
        (forward-char 1)
        (let ((pos (point)))
          (delete-region pos (+ pos (skip-chars-forward "\s\t\n\t\r\f"))))))))

(defvar parse-help-flags-regexp-2 (concat "^[\s]?+" parse-help-flags-regexp))

(defvar parse-help-flags-first-flag-regexp
  (concat "^[\s]+" parse-help-flags-regexp))

(defvar parse-help-all-specifiers nil)

(defmacro parse-help-with-output (output &rest body)
  "Expand BODY in temp buffer with OUTPUT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (save-excursion (insert ,output))
     ,@body))

(defun parse-help-word-upcased-p (word)
  "Return t if WORD is upcased."
  (equal word (upcase word)))

(defun parse-help-upcased-specifier-at-point ()
  "Forward word at point if it stars with two upcased letters.
Return substring of forwarded word."
  (let ((case-fold-search nil))
    (when (looking-at "[A-Z]\\{2\\}")
      (let* ((beg (point))
             (end (+ beg (skip-chars-forward "-*_~$A-Za-z0-9:.#\\+"))))
        (buffer-substring-no-properties beg end)))))

(defun parse-help-split-argument (item)
  "Split ITEM into cons of argument and specifier."
  (let ((splitted (split-string item "\\[?=" t)))
    (cond ((and (= 1 (length splitted))
                (string-suffix-p "=" item))
           (cons (concat (car splitted) "=") "..." ))
          ((>= (length splitted) 2)
           (cons (concat (car splitted) "=") (string-join (seq-subseq splitted 1) "="))))))

(defun parse-help-flags-at-point (&optional regexp)
  "Parse flags that match REGEXP at point.
Return the plist with properties :argument, :shortarg and :specifier.
Values of :argument and :shortarg is a list and specifier - string or nil."
  (unless regexp
    (setq regexp parse-help-flags-regexp))
  (let ((long)
        (specifier)
        (short)
        (plist))
    (while
        (cond ((looking-at regexp)
               (when (re-search-forward regexp nil t 1)
                 (let ((item (string-trim (replace-regexp-in-string
                                           ",$"
                                           ""
                                           (match-string-no-properties 2)))))
                   (cond
                    ((string-prefix-p "--" item)
                     (let ((splitted (split-string item "\\[?=" t)))
                       (cond ((and (= 1 (length splitted))
                                   (string-suffix-p "=" item))
                              (push (concat (car splitted) "=") long)
                              (push "..." specifier))
                             ((= 2 (length splitted))
                              (push (car splitted) long)
                              (push (car (last splitted)) specifier))
                             (t (push item long)))))
                    ((string-prefix-p "-" item)
                     (setq short (push item short)))
                    (t (setq item (push item specifier)))))))
              ((let ((case-fold-search nil))
                 (looking-at "[A-Z]\\{2\\}"))
               (push (parse-help-upcased-specifier-at-point) specifier))))
    (when (or long short specifier)
      (setq plist (list
                   :argument long
                   :shortarg short
                   :specifier specifier))
      plist)))

(defun parse-help-all-flags ()
  "Parse arguments without description."
  (let ((matches))
    (re-search-forward "^ *[^$] *\\(?:[-]\\([]:a-z-Z-A=\\[|]+\\),?[ 	]+\\([a-z][]:a-z-Z-A=\\[|]+[ ]+\\)?\\)?-+\\([]:a-z-Z-A=\\[|]+\\) +\\(.*\\)$" nil t 1)
    (beginning-of-line)
    (while (looking-at "^ *[^$] *\\(?:[-]\\([]:a-z-Z-A=\\[|]+\\),?[ 	]+\\([a-z][]:a-z-Z-A=\\[|]+[ ]+\\)?\\)?-+\\([]:a-z-Z-A=\\[|]+\\) +\\(.*\\)$")
      (let ((flag  (match-string-no-properties 1))
            (argument (match-string-no-properties 3))
            (specifier (match-string-no-properties 2) )
            (parsed-arg))
        (setq parsed-arg (when argument
                           (parse-help-split-argument argument)))
        (setq specifier (if specifier
                            (string-trim specifier)
                          (cdr parsed-arg)))
        (push (list :argument (when-let ((f (or (car parsed-arg) argument)))
                                (concat "--" f))
                    :shortarg (when flag
                                (concat "-" flag))
                    :specifier specifier
                    :description (or argument flag specifier ""))
              matches)
        (forward-line 1)))
    (when (> (length matches) 1)
      (reverse matches))))

(defun parse-help-flag-row (&optional regexp)
  "Parse current line with REGEXP at point.
Return the plist with :argument, :shortarg, :specifier and :description."
  (unless regexp
    (setq regexp parse-help-flags-regexp))
  (when-let* ((flag-pos (progn (skip-chars-forward "\s\t")
                               (point)))
              (flags (or (parse-help--parse-squared-brackets)
                         (parse-help-flags-at-point regexp)))
              (flags-end (point)))
    (let ((regexp (save-excursion
                    (goto-char flag-pos)
                    (concat "^" (make-string (current-column)
                                             (char-before))
                            regexp)))
          (descr-end (line-end-position))
          (description))
      (setq description (string-join (split-string
                                      (buffer-substring-no-properties
                                       flags-end
                                       (if (or (re-search-forward
                                                (concat "^[\s]+" regexp)
                                                nil t 1)
                                               (re-search-forward
                                                regexp
                                                nil t 1))
                                           (let ((m (match-beginning 0)))
                                             (goto-char m)
                                             (point))
                                         descr-end))
                                      nil t)
                                     "\s"))
      (when (car (plist-get flags :specifier))
        (add-to-list
         'parse-help-all-specifiers
         (car (plist-get flags :specifier))))
      (when (or (plist-get flags :argument)
                (plist-get flags :shortarg))
        (parse-help-flatten-plists
         (append flags
                 (list :description description)) )))))

(defun parse-help-parse-rows-forward ()
  "Starting from current line, parse and forward help arguments.
Result is a list of rows, where each row is a list of four strings:
- argument
- shortarg
- specifier
- description."
  (let ((rows)
        (case-fold-search nil))
    (while (re-search-forward parse-help-flags-regexp-2 nil t 1)
      (beginning-of-line)
      (while
          (when-let ((parsed (parse-help-flag-row)))
            (setq rows (append rows parsed)))))
    (reverse rows)))

(defun parse-help-get-matches (regexp num str)
  "Return list of REGEXP matches in STR with.
NUM specifies which parenthesized expression in the last regexp."
  (let ((choices))
    (parse-help-with-output str
      (while (re-search-forward regexp nil t 1)
        (let ((m (match-string-no-properties num)))
          (push m choices))))
    choices))

(defun parse-help-choices-from-description (description)
  "Return list of possible options in DESCRIPTION."
  (when description
    (or (parse-help-get-matches "[']\\([^']+\\)[']:" 1 description)
        (parse-help-get-matches "[']\\([^']+\\)[']" 1 description))))

(defun parse-help-plist-remove-nils (plist)
  "Return the keys in PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

(defun parse-help-flatten-plists (plist)
  "Normalize PLIST with values as list to single properties."
  (let ((shortargs (plist-get plist :shortarg))
        (args (plist-get plist :argument))
        (descr (plist-get plist :description))
        (specifier (plist-get plist :specifier))
        (no-variants)
        (result))
    (setq no-variants
          (mapcar
           (lambda (arg)
             (list :argument (replace-regexp-in-string "\\[\\|\\]" "" arg)
                   :description descr
                   :specifier (car specifier)))
           (seq-filter (apply-partially #'string-match-p "\\[no\\]") args)))
    (when no-variants
      (setq args (mapcar (apply-partially #'replace-regexp-in-string
                                          "\\[no\\]" "")
                         args)))
    (setq result (cond
                  ((and (stringp args)
                        (stringp shortargs))
                   (list plist))
                  ((>= (length args)
                       (length shortargs))
                   (seq-map-indexed
                    (lambda (flag i)
                      (list :argument flag
                            :shortarg (nth i shortargs)
                            :description descr
                            :specifier (or (nth i specifier) (car specifier))))
                    args))
                  (t
                   (seq-map-indexed
                    (lambda (shortarg i)
                      (list :argument (or (nth i args) shortarg)
                            :shortarg shortarg
                            :description descr
                            :specifier (or (nth i specifier) (car specifier))))
                    shortargs))))
    (append result no-variants)))

(defun parse-help-plist-props (keywords plist)
  "Take values of KEYWORDS props from PLIST."
  (mapcar (apply-partially #'plist-get plist) keywords))

(defun parse-help--parse-squared-brackets ()
  "Parse flags wrapped in squared brackets at point.
Return plist with keywords :specifier, :argument and :specifier."
  (let ((flags))
    (while (looking-at "\\[")
      (when-let* ((beg (point))
                  (end (or (parse-help-move-with 'forward-sexp)
                           (line-end-position)))
                  (str (buffer-substring-no-properties (1+ beg)
                                                       (1- end))))
        (save-excursion
          (goto-char (1+ beg))
          (while (re-search-forward
                  "\\(-[$A-Za-z0-9][$A-Za-z0-9]?\\)?|?\\(--[$A-Za-z0-9-]+\\)[\s\t]*\\([<][^>]+>\\)?"
                  (1- end) t 1)
            (push (parse-help-plist-remove-nils
                   (list
                    :shortarg (match-string-no-properties 1)
                    :argument (match-string-no-properties 2)
                    :specifier (match-string-no-properties 3)
                    :description ""))
                  flags)))
        (skip-chars-forward "\s\t\n\r\f")))
    (delete nil flags)))

(defun parse-help-squared-brackets-flags (str)
  "Search in STR for flags wrapped in squared brackets.
Return list of plists with keywords :specifier, :argument and :specifier."
  (let ((options))
    (with-temp-buffer
      (erase-buffer)
      (save-excursion
        (insert str))
      (while (re-search-forward "^[\s\t]?+[\\[]" nil t 1)
        (forward-char -1)
        (setq options (append options (parse-help--parse-squared-brackets)))))
    options))

(defun parse-help-plist-to-row (plist)
  "Take PLIST props."
  (parse-help-plist-props
   '(:argument :shortarg :specifier :description)
   plist))

(defun parse-help-plist-omit (keywords plist)
  "Omit KEYWORDS keys and values from PLIST."
  (let ((result)
        (len (length plist)))
    (dotimes (idx len)
      (when (eq (logand idx 1) 0)
        (let ((prop-name (nth idx plist)))
          (unless (memq prop-name keywords)
            (setq result (plist-put result prop-name
                                    (plist-get plist prop-name)))))))
    result))

(defun parse-help-get-usage-doc (output)
  "Retrieve usage docstring from OUTPUT."
  (let* ((regexs
          (list
           parse-help-flags-regexp-2
           "^[\s\t]?+\\([-]+[a-zZ-A-0-9_]+\\)[:]?[\s][\s]+"
           "^[\s\t]?+\\([a-zZ-A]+[a-zZ-A-0-9_]+\\)[:]?[\s][\s]+[a-zZ-A]"))
         (candidates (mapcar
                      (fp-compose
                       (fp-partial #'substring-no-properties
                                   output 0)
                       (fp-rpartial #'string-match-p
                                    output))
                      regexs)))
    (car (seq-sort-by #'length '<
                      (delete nil
                              candidates)))))

(defun parse-help-group-columns (commands-or-vars)
  "Return alist with groupped COMMANDS-OR-VARS."
  (seq-reduce
   (lambda (acc val)
     (let* ((key (if (parse-help-word-upcased-p (plist-get val :argument))
                     :variables
                   :commands))
            (cell (assoc key acc))
            (group (if cell
                       (append (cdr cell)
                               (list (parse-help-plist-remove-nils val)))
                     (list (parse-help-plist-remove-nils val)))))
       (if cell
           (setcdr cell group)
         (push (cons key group) acc))
       acc))
   (seq-copy commands-or-vars) '()))

(defun parse-help--columns (regexp num)
  "Search and parse COMMAND flags using REGEXP and NUM."
  (let ((rows))
    (while (re-search-forward regexp nil t 1)
      (let ((str (match-string-no-properties num))
            (descr-start (point))
            (description)
            (descr-end)
            (subflags)
            (row))
        (setq descr-end (or (save-excursion
                              (when-let ((found (re-search-forward
                                                 regexp nil t 1)))
                                (forward-line -1)
                                (point)))
                            (line-end-position)))
        (setq description (buffer-substring-no-properties
                           descr-start
                           descr-end))
        (setq subflags
              (when (not (parse-help-word-upcased-p str))
                (parse-help-squared-brackets-flags description)))
        (setq row (append
                   (list
                    :argument str
                    :description description)
                   (if subflags
                       (list :arguments subflags)
                     (when-let ((choices (parse-help-choices-from-description
                                          description)))
                       (list :choices
                             choices)))))
        (setq rows (nconc rows (list row)))))
    rows))

(defun parse-help-make-choices (specifier)
  "Return list of choices from string SPECIFIER."
  (when (string-match-p "|" (or specifier ""))
    (split-string (replace-regexp-in-string "^<\\|>$" "" specifier) "|" t)))

(defun parse-help-normalize-option (option)
  "Normalize argument or switch OPTION."
  (let* ((specifier (plist-get option :specifier))
         (argument (or (plist-get option :argument)
                       (plist-get option :shortarg)))
         (choices (or (parse-help-make-choices specifier)
                      (parse-help-choices-from-description
                       (plist-get option :description)))))
    (if (or specifier
            (string-suffix-p "=" argument)
            choices)
        (parse-help-plist-remove-nils
         (append
          (parse-help-plist-omit '(:argument) option)
          (list
           :choices choices
           :argument (if (string-suffix-p "=" argument)
                         argument
                       (concat argument " "))
           :class 'transient-option)))
      option)))

(defun parse-help-group-options (options)
  "Group OPTIONS into arguments and inline switch options."
  (seq-reduce
   (lambda (acc item)
     (let* ((specifier (plist-get item :specifier))
            (argument (or (plist-get item :argument)
                          (plist-get item :shortarg)))
            (choices (or (parse-help-make-choices specifier)
                         (parse-help-choices-from-description
                          (plist-get item :description))))
            (key (if (or specifier
                         (string-suffix-p "=" argument)
                         choices)
                     :arguments
                   :switches))
            (val
             (pcase key
               (:arguments
                (append
                 (parse-help-plist-omit '(:argument) item)
                 (list
                  :choices choices
                  :argument (if (string-suffix-p "=" argument)
                                argument
                              (concat argument " "))
                  :class 'transient-option)))
               (_ item)))
            (cell (assoc key acc))
            (group (if cell
                       (append (cdr cell)
                               (list (parse-help-plist-remove-nils val)))
                     (list (parse-help-plist-remove-nils val)))))
       (if cell
           (setcdr cell group)
         (push (cons key group) acc))
       acc))
   options '()))

(defun parse-help-generate-key (flag &optional used-keys)
  "Generate hydra key for option FLAG that not present in USED-KEYS."
  (when (> (length flag) 2)
    (setq flag (replace-regexp-in-string "^[-][-]" "" flag)))
  (if (and flag (member flag '("--" "-"))
           (not (member "-" used-keys)))
      "-"
    (or (seq-find (fp-and key-valid-p (fp-compose not (fp-rpartial member
                                                                   used-keys)))
                  (mapcar (fp-rpartial substring 0 1)
                          (split-string
                           flag "-" t)))
        (seq-find (fp-and key-valid-p (fp-compose not (fp-rpartial member
                                                                   used-keys)))
                  (split-string flag "" t))
        (seq-find (fp-and key-valid-p
                          (fp-compose not (fp-rpartial member used-keys)))
                  (seq-difference
                   (nconc (parse-help--get-alphabete "a")
                          (parse-help--get-alphabete "A")
                          (delete "\""
                                  (parse-help--get-alphabete "!"
                                                             25)))
                   used-keys)))))

(defun parse-help-transient-ensure-keys (plists all-keys)
  "Generate key for PLISTS excluding ALL-KEYS."
  (let ((used-keys (append all-keys '("-M")))
        (args))
    (dolist (item plists)
      (when-let ((key (and item (parse-help-generate-key
                                 (or
                                  (plist-get item :shortarg)
                                  (plist-get item :argument)
                                  (plist-get item :description))
                                 used-keys))))
        (push key used-keys)
        (setq item (if (plist-get item :shortarg)
                       (plist-put item :shortarg key)
                     (plist-put item :key key)))
        (push item args)))
    (reverse args)))

(defun parse-help-transient-get-all-keys (plists)
  "Retrieve :shortarg and :key from PLISTS."
  (let ((result (remove nil (mapcar (fp-or (fp-rpartial plist-get :shortarg)
                                           (fp-rpartial plist-get :key))
                                    plists))))
    result))

(defun parse-help-transient-name (prefix-name plist)
  "Return symbol from from PREFIX-NAME and :argument or :shortarg from PLIST."
  (let ((argument (plist-get plist :argument))
        (shortarg (plist-get plist :shortarg)))
    (unless argument (setq argument shortarg))
    (intern (replace-regexp-in-string "---" "--"
                                      (concat prefix-name "-" (string-trim
                                                               (replace-regexp-in-string
                                                                "=$"
                                                                ""
                                                                argument)))))))

(defun parse-help-define-argument (prefix-name plist)
  "Convert PLIST to `transient-define-argument' form.
Name is generated from PREFIX-NAME and argument or shortarg."
  (let ((argument (plist-get plist :argument))
        (shortarg (plist-get plist :shortarg))
        (sym (parse-help-transient-name prefix-name plist))
        (description (string-join
                      (seq-remove #'not
                                  (list (plist-get plist :argument)
                                        (plist-get plist :description)))
                      "\s")))
    (unless argument (setq argument shortarg))
    `(transient-define-argument ,sym
       ()
       ,(parse-help-transient-description-to-doc
         (if
             (or (not (plist-get plist :description))
                 (string-empty-p
                  (plist-get plist
                             :description)))
             (format "Set argument %s." (or argument
                                            shortarg))
           (plist-get plist :description)))
       :description ,(parse-help-short-description description)
       :argument ,(if (or (string-suffix-p "=" argument))
                      argument
                    (concat (string-trim argument) " "))
       :class 'transient-option
       ,@(parse-help-plist-omit
          '(:key :argument
                 :shortarg
                 :specifier
                 :class
                 :description)
          plist))))

(defun parse-help-swtiches-from-arguments (arguments)
  "REturn ARGUMENTS without specifier or choices."
  (let ((switches (seq-remove
                   (fp-or (fp-rpartial plist-get :specifier)
                          (fp-rpartial plist-get :choices))
                   arguments)))
    switches))

(defun parse-help-maybe-split (column-name mapped-commands)
  "Group MAPPED-COMMANDS to vectors with COLUMN-NAME."
  (seq-map-indexed
   (lambda (items idx)
     (apply #'vector (append (list (format "%s %d" column-name idx))
                             items)))
   (seq-remove #'null  (list (seq-take mapped-commands 30)
                             (seq-drop mapped-commands 30)))))

(defun parse-help-normalize-args (cmd &optional args)
  "Trim `parse-help-transient-prefix' from CMD and format ARGS.
Return string with command CMD and ARGS."
  (let ((sh-cmd (substring (if (symbolp cmd) (symbol-name cmd) cmd)
                           (length parse-help-transient-prefix)))
        (str-args (string-join (mapcar (fp-compose
                                        'string-trim (apply-partially #'format "%s"))
                                       args)
                               "\s")))
    (concat sh-cmd " " str-args)))


(defun parse-help-group-by-prefixes (items)
  "Group ITEMS by longest common argument prefixes."
  (let ((strings (mapcar (fp-rpartial plist-get :argument) items)))
    (parse-help-group-with
     (lambda (pl)
       (car
        (seq-sort-by
         #'length
         '>
         (delq nil
               (mapcar
                (apply-partially
                 #'parse-help-s-shared-start
                 (plist-get pl :argument))
                (remove (plist-get pl :argument) strings))))))
     items)))







(defun parse-help-generate-commands-keys (cmds)
  "Add key property to CMDS."
  (setq cmds (if (or (seq-find 'stringp cmds)
                     (seq-find 'symbolp cmds))
                 (mapcar (fp-compose
                          (fp-when
                            (fp-or symbolp stringp)
                            (lambda (it)
                              (list :argument (format "%s" it)))))
                         cmds)
               cmds))
  (let ((alist
         (parse-help--generate-shortcuts cmds
                                          (lambda (it)
                                            (if (stringp it)
                                                (replace-regexp-in-string
                                                 "[-\s]+" ""
                                                 (plist-get
                                                  it
                                                  :argument))
                                              (downcase
                                               (or
                                                (plist-get it :shortarg)
                                                (replace-regexp-in-string
                                                 "[-\s]+" ""
                                                 (plist-get
                                                  it
                                                  :argument))))))))
        (result))
    (dolist (cell alist)
      (let ((value (plist-put (cdr cell) :key (car cell))))
        (push value result)))
    result))

(defmacro parse-help-evolve (&rest spec)
  "Return function that apply SPEC to plist."
  (declare
   (indent defun))
  `(lambda (plist) (let ((result (seq-copy plist)))
                (progn ,@(seq-map-indexed
                          (lambda (_v idx)
                            (when (eq (logand idx 1) 0)
                              (let* ((prop-name (nth idx spec))
                                     (fn (plist-get spec prop-name)))
                                `(when (memq ,prop-name plist)
                                   (setq result
                                         (plist-put
                                          result
                                          ,prop-name
                                          (funcall ,fn (plist-get
                                                        result
                                                        ,prop-name))))))))
                          spec)
                       result))))

(defun parse-help-short-description (str)
  "Make short description from STR."
  (let ((result
         (let ((parts (seq-drop-while
                       (lambda (word) (not (string-match-p "^[a-z]" word)))
                       (split-string str nil t))))
           (if-let ((pos (seq-position parts "." (lambda (it _b) (if (string-match-p "[.]$" it)
                                                                t
                                                              nil)))))
               (string-join (seq-take parts (1+ pos)) " ")
             (seq-reduce (lambda (acc word)
                           (setq acc (concat
                                      acc
                                      (if (> (length acc) 30)
                                          ""
                                        (concat " " word)))))
                         parts
                         "")))))
    (if (string-empty-p result)
        (truncate-string-to-width str 30)
      result)))

(defun parse-help-map-switches-arr (switch-options)
  "Return inline SWITCH-OPTIONS."
  (let ((result (parse-help-maybe-split
                 "Switches"
                 (mapcar
                  (fp-compose
                   (apply-partially #'remove nil)
                   (apply-partially #'parse-help-plist-props
                                    '(:shortarg
                                      :key
                                      :description
                                      :argument))
                   (fp-when (fp-compose
                             (apply-partially #'string-empty-p)
                             (fp-rpartial
                              plist-get
                              :description))
                            (fp-converge
                             plist-put
                             [identity
                              (fp-const :description)
                              (fp-compose
                               (apply-partially
                                #'replace-regexp-in-string
                                "^[-]+" "")
                               (fp-rpartial
                                plist-get
                                :argument))]))
                   (fp-when (fp-rpartial plist-get :shortarg)
                            (apply-partially #'parse-help-plist-omit '(:key)))
                   (parse-help-evolve
                     :description
                     (fp-compose (fp-when stringp
                                          parse-help-short-description))))
                  switch-options))))
    result))



(defun parse-help-map-commands-vectors (prefix-name commands)
  "Return vector with COMMANDS.
PREFIX-NAME is parent command."
  (parse-help-maybe-split
   "Commands"
   (mapcar (fp-converge
            append
            [(fp-compose
              (apply-partially #'remove nil)
              (apply-partially #'parse-help-plist-props
                               '(:shortarg :key)))
             (if parse-help-use-command-long-descriptions
                 (fp-compose
                  list
                  (fp-rpartial 'string-join " - ")
                  (apply-partially #'remove nil)
                  (fp-converge list
                               [(fp-rpartial plist-get :argument)
                                (fp-compose
                                 (fp-when stringp
                                   (fp-compose car
                                               (fp-rpartial split-string
                                                            "[\n\r\f]" t)))
                                 (fp-rpartial plist-get :argument)
                                 (fp-or (fp-rpartial plist-get :description)
                                        (fp-rpartial plist-get :argument)))]))
               (fp-compose list (fp-rpartial plist-get :argument)))
             (fp-compose list (apply-partially #'parse-help-transient-name
                                               prefix-name))])
           commands)))

(defun parse-help-map-arguments-vector-short (arguments)
  "Return vector with ARGUMENTS.
PREFIX-NAME is parent command."
  (parse-help-maybe-split
   "Arguments"
   (mapcar
    (lambda (pl)
      (let* ((arg (or (plist-get pl :argument)
                      (plist-get pl :shortarg)))
             (key (or (plist-get pl :key)
                      (plist-get pl :shortarg)))
             (descr (seq-remove #'string-empty-p
                                (remove nil
                                        (parse-help-plist-props
                                         '(:description
                                           :argument
                                           :key
                                           :shortarg)
                                         pl))))
             (props (parse-help-plist-omit '(:key
                                             :shortarg
                                             :specifier
                                             :description
                                             :argument)
                                           pl))
             (result (append (list
                              key
                              (parse-help-short-description
                               (or (car-safe descr)
                                   ""))
                              (if (or (string-suffix-p
                                       "=" arg)
                                      (string-suffix-p
                                       " " arg))
                                  arg
                                (concat arg " ")))
                             props)))
        result))
    arguments)))
(defun parse-help-map-arguments-vector (prefix-name arguments)
  "Return vector with ARGUMENTS.
PREFIX-NAME is parent command."
  (parse-help-maybe-split "Arguments"
                          (mapcar
                           (fp-converge
                            append
                            [(fp-compose
                              (apply-partially #'remove nil)
                              (apply-partially #'parse-help-plist-props
                                               '(:shortarg :key))
                              (fp-when (fp-rpartial plist-get :shortarg)
                                (apply-partially #'parse-help-plist-omit
                                                 '(:key))))
                             (fp-compose list
                                         (apply-partially
                                          #'parse-help-transient-name
                                          prefix-name))])
                           arguments)))

(defun parse-help-transient-map-to-prefix (parent-prefix-name plist)
  "Convert PLIST to `transient-define-argument' form.
Name is generated from PARENT-PREFIX-NAME and argument or shortarg."
  (let* ((prefix-name (format "%s" (parse-help-transient-name
                                    parent-prefix-name
                                    plist)))
         (arguments (plist-get plist :arguments))
         (switches (parse-help-swtiches-from-arguments arguments))
         (all-keys (parse-help-transient-get-all-keys arguments))
         (arguments (seq-difference arguments switches)))
    (setq arguments (parse-help-transient-ensure-keys arguments all-keys))
    (setq all-keys (parse-help-transient-get-all-keys arguments))
    (setq switches (parse-help-transient-ensure-keys switches all-keys))
    (append
     ;; (seq-uniq (mapcar
     ;;            (apply-partially #'parse-help-define-argument prefix-name)
     ;;            arguments))
     `((transient-define-prefix ,(intern prefix-name)
         ()
         ,(or (parse-help-transient-description-to-doc
               (plist-get plist :description))
              "")
         [,@(parse-help-map-arguments-vector-short arguments)
          ,@(parse-help-map-switches-arr switches)]
         ["Actions"
          ("RET" parse-help-transient-run-command)
          ("<return>" parse-help-transient-run-command)
          ("C-c TAB" parse-help-transient-insert)
          ("C-c M-m" parse-help-transient-identity)])))))

(defun parse-help--generate-transient (cmd-cell)
  "Generate transient from CMD-CELL."
  (let* ((help-cmd (parse-help-alist-get :command
                                         cmd-cell))
         (cmd (parse-help-transient-normalize-command-name help-cmd))
         (prefix-name (concat parse-help-transient-prefix cmd))
         (arguments
          (parse-help-generate-commands-keys (parse-help-alist-get :arguments
                                                                   cmd-cell)))
         (switches (parse-help-swtiches-from-arguments arguments))
         (commands (parse-help-generate-commands-keys
                    (reverse (parse-help-alist-get :commands cmd-cell))))
         (usage (parse-help-alist-get :usage cmd-cell))
         (arguments (seq-difference arguments switches)))
    (append
     (seq-uniq (mapcar (apply-partially #'parse-help-transient-map-to-prefix
                                        prefix-name)
                       commands))
     `((transient-define-prefix ,(intern prefix-name)
         ()
         ,(parse-help-transient-description-to-doc
           (or usage prefix-name))
         [,@(parse-help-map-arguments-vector-short arguments)
          ,@(parse-help-map-commands-vectors prefix-name commands)
          ,@(parse-help-map-switches-arr switches)
          ["Actions"
           ("RET" parse-help-transient-run-command)
           ("<return>" parse-help-transient-run-command)
           ("C-c TAB" parse-help-transient-insert)
           ("C-c M-m" parse-help-transient-identity)]])))))

;;;###autoload (autoload 'parse-help-transient-show-args "parse-help.el" nil t)
(transient-define-suffix parse-help-transient-show-args ()
  "Show current infix args."
  :description "show arguments"
  :transient t
  (interactive)
  (if-let ((cmd (parse-help-normalize-args
                 transient-current-command
                 (transient-args transient-current-command))))
      (message (concat (propertize "Current args: " 'face 'success))
               cmd)
    (message (concat (propertize "No args for %s " 'face 'error)
                     (format "%s" transient-current-command)))))
;;;###autoload (autoload 'parse-help-transient-run-command "parse-help.el" nil t)
(transient-define-suffix parse-help-transient-run-command ()
  "Show current infix args."
  :description "Run"
  (interactive)
  (save-selected-window
    (selected-window)
    (require 'vterm)
    (let* ((args (transient-args transient-current-command))
           (cmd (parse-help-normalize-args
                 transient-current-command
                 args))
           (buffer (format "*%s*"
                           (string-join
                            (delete nil
                                    (list "vterm"
                                          (car (split-string cmd nil t))
                                          (vc-root-dir)))
                            "-")))
           (live-p (buffer-live-p (get-buffer buffer))))
      (when live-p
        (switch-to-buffer (get-buffer buffer))
        (when (fboundp 'vterm--invalidate)
          (vterm--invalidate))
        (kill-buffer (get-buffer buffer)))
      (let ((default-directory (or (vc-root-dir) default-directory)))
        (when (fboundp 'vterm)
          (vterm buffer)))
      (run-at-time
       0.5 nil 'vterm-send-string cmd))))
;;;###autoload (autoload 'parse-help-transient-identity "parse-help.el" nil t)
(transient-define-suffix parse-help-transient-identity ()
  "Show current infix args."
  :description "Identity"
  (interactive)
  (transient-args transient-current-command))
;;;###autoload (autoload 'parse-help-transient-insert "parse-help.el" nil t)
(transient-define-suffix parse-help-transient-insert ()
  "Insert current infix args."
  :description "Insert"
  (interactive)
  (insert (mapconcat (apply-partially #'format "%s")
                     (transient-args transient-current-command)
                     "\s")))
(defun parse-help-call-backends (backends)
  "Invoke BACKENDS on current buffer.
BACKENDS is alist of functions and arguments."
  (let ((results))
    (dolist (backend backends)
      (condition-case err
          (save-excursion
            (let ((res (apply (car backend)
                              (cdr backend))))
              (when res (setq results (push res results)))))
        (error (message "An error ocuured %s" err))))
    results))

(defun parse-help--parse-output (output backends)
  "Invoke BACKENDS in temp buffer with OUTPUT.
BACKENDS is alist of functions and arguments."
  (parse-help-with-output
      output
    (parse-help-call-backends backends)))

(defun parse-help-parse-output (output command)
  "Parse OUTPUT from COMMAND."
  (let* ((results (parse-help--parse-output
                   output
                   '((parse-help-all-flags)
                     (parse-help-parse-rows-forward)
                     (parse-help--columns
                      "^[\s\t]?+\\([-]+[a-zZ-A-0-9_]+\\)[:]?[\s][\s]+"
                      1))))
         (options (car (seq-sort-by #'length #'> results)))
         (commands-or-vars
          (parse-help-group-columns
           (parse-help-with-output output
             (parse-help--columns
              "^[\s\t]?+\\([a-zZ-A]+[a-zZ-A-0-9_]+\\)[:]?[\s][\s]+"
              1))))
         (usage (parse-help-get-usage-doc output)))
    (append `((:usage . ,usage)
              (:command . ,command)
              (:arguments . ,(mapcar #'parse-help-normalize-option options)))
            commands-or-vars)))

;;;###autoload
(defun parse-help-transient-switch-from-region (beg end)
  "Parse region between BEG and END to transient switch.
Region should have form --email-obfuscation=none|javascript|references."
  (interactive "r")
  (let* ((input (if (region-active-p)
                    (buffer-substring-no-properties
                     beg
                     end)
                  (re-search-forward
                   "\\(--\\([^= ]+\\)[ =]+\\(\\(\\([^|,\s\n]+\\)[|]\\)+\\)\\)"
                   nil t 1)
                  (concat
                   (buffer-substring-no-properties (match-beginning 0)
                                                   (line-end-position)))))
         (result (parse-help-switch-from-string
                  (read-string "Ok?" (string-trim
                                      input)))))
    (pp result)))


(defun parse-help-switch-from-string (str &optional prefix-name plist)
  "Parse STR --email-obfuscation=none|javascript|references.
If PLIST is omitted or nil, return full `transient-define-argument' form,
othervise return  only props.
PREFIX-NAME is added to the name of argument."
  (let ((option)
        (choices))
    (with-temp-buffer
      (insert str)
      (when (re-search-backward
             "\\(--\\([^= ]+\\)[ =]+<?\\(\\(\\([^|\s>]+\\)[|]?\\)+\\)\\)" nil t
             1)
        (setq option (match-string-no-properties 2))
        (setq choices (match-string-no-properties 3))
        (when choices
          (setq choices (mapcar (lambda (it)
                                  (when (string-prefix-p "<" it)
                                    (setq it
                                          (substring-no-properties it 1)))
                                  (when (string-suffix-p ">" it)
                                    (setq it
                                          (substring-no-properties it
                                                                   0
                                                                   (1-
                                                                    (length
                                                                     it)))))
                                  (when (string-prefix-p "[" it)
                                    (setq it
                                          (substring-no-properties it 1)))
                                  (when (string-suffix-p "]" it)
                                    (setq it
                                          (substring-no-properties it
                                                                   0
                                                                   (1-
                                                                    (length
                                                                     it)))))
                                  it)
                                (split-string choices "|" t))))))
    (if plist
        (list
         :class 'transient-switches
         :argument-format (concat "--" option "=%s")
         :argument-regexp (concat "\\(" (concat "--" option "=")
                                  (regexp-opt choices t)
                                  "\\)")
         :choices choices)
      `(transient-define-argument ,(intern (concat
                                            (or prefix-name
                                                (replace-regexp-in-string
                                                 "[^a-z-]" "" (file-name-base
                                                               (buffer-name))))
                                            "-"
                                            option))
         ()
         ,(format "Argument for %s." option)
         :class 'transient-switches
         :argument-format ,(concat "--" option "=%s")
         :argument-regexp ,(concat "\\(" (concat "--" option "=")
                                   (regexp-opt choices t)
                                   "\\)")
         :choices ',choices))))

;;;###autoload
(defun parse-help-extract-all-switches (prefix)
  "Parse region between BEG and END to transient switch.
PREFIX is added to the name of argument.
Region should have form --email-obfuscation=none|javascript|references."
  (interactive "sPrefix name:")
  (let ((opts)
        (end
         (when (region-active-p)
           (region-end))))
    (while (re-search-forward
            "\\(--\\([^= ]+\\)[ =]+\\(\\(\\([^|,\s\n]+\\)[|]\\)+\\)\\)" end t 1)
      (setq opts
            (push (string-trim
                   (buffer-substring-no-properties (match-beginning
                                                    0)
                                                   (line-end-position)))
                  opts)))
    (with-current-buffer (get-buffer-create "*generated-switches*")
      (erase-buffer)
      (insert (parse-help-transient-render-sexps
               (mapcar (lambda (o)
                         (parse-help-switch-from-string o prefix))
                       opts)))
      (emacs-lisp-mode)
      (font-lock-ensure)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun parse-help-command (command)
  "Parse output from COMMAND."
  (interactive (list (read-string "Shell command")))
  (let ((output (shell-command-to-string command))
        (buff (format "*help-output-%s*" command)))
    (if (called-interactively-p 'any)
        (with-current-buffer (get-buffer-create buff)
          (erase-buffer)
          (let ((emacs-lisp-mode-hook nil))
            (emacs-lisp-mode)
            (insert (pp-to-string (parse-help-parse-output output command)))
            (font-lock-ensure)
            (setq header-line-format command)
            (pop-to-buffer (current-buffer))))
      (parse-help-parse-output output command))))

;;;###autoload
(defun parse-help-generate-transient (&optional command)
  "Read help COMMAND and insert generated transient arguments."
  (interactive)
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create "*parse-help-transient-generic*")
      (erase-buffer)
      (let ((emacs-lisp-mode-hook nil))
        (emacs-lisp-mode)
        (insert "(require 'transient)\n")
        (insert (parse-help-transient-render-sexps
                 (let ((default-directory dir))
                   (parse-help--generate-transient
                    (parse-help-command
                     (or command (read-string "Command ")))))))
        (when (called-interactively-p 'any)
          (font-lock-ensure)
          (pop-to-buffer (current-buffer)))
        (buffer-string)))))

(provide 'parse-help)
;;; parse-help.el ends here