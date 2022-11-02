;;; parse-help.el --- Parse CLI help output -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/parse-help
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "26.1"))

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

(defvar parse-help-flags-regexp "\\[?\\(\\([-<][-]?[^\s\t\n>]+\\)[\s\t\n>][>]?\\)")
(defvar parse-help-flags-regexp-2 (concat "^[\s]?+" parse-help-flags-regexp))
(defvar parse-help-flags-first-flag-regexp
  (concat "^[\s]+" parse-help-flags-regexp))

(defvar parse-help-all-specifiers nil)

(defmacro parse-help-with-output (output &rest body)
  "Expand BODY in temp buffer with OUTPUT."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
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
    (setq plist (list
                 :argument long
                 :shortarg short
                 :specifier specifier))
    plist))

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

(defun parse-help-get-matches (regexp num str)
  "Return list of REGEXP matches in STR with.
NUM specifies which parenthesized expression in the last regexp."
  (let ((choices))
    (parse-help-with-output str
      (while (re-search-backward regexp nil t 1)
        (let ((m (match-string-no-properties num)))
          (push m choices))))))

(defun parse-help-choices-from-description (description)
  "Return list of possible options in DESCRIPTION."
  (when description
    (or (parse-help-get-matches "[']\\([^']+\\)[']:" 1 description)
        (parse-help-get-matches "[']\\([^']+\\)[']" 1 description))))

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
           (seq-filter (apply-partially 'string-match-p "\\[no\\]") args)))
    (when no-variants
      (setq args (mapcar (apply-partially #'replace-regexp-in-string "\\[no\\]" "")
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

(defun parse-help-parse-rows-forward ()
  "Starting from current line, parse and forward help arguments.
Result is a list of rows, where each row is a list of four strings:
- argument
- shortarg
- specifier
- description."
  (let ((rows)
        (case-fold-search nil))
    (while
        (when-let ((parsed (parse-help-flag-row)))
          (setq rows (append rows parsed))))
    (reverse (seq-uniq rows
                       (fp-use-with equal
                                    [(fp-rpartial plist-get
                                                  :argument)
                                     (fp-rpartial plist-get
                                                  :argument)])))))

(defun parse-help-plist-props (keywords plist)
  "Take values of KEYWORDS props from PLIST."
  (mapcar (apply-partially #'plist-get plist) keywords))

(defun parse-help--parse-squared-brackets ()
  "Parse flags wrapped in squared brackets at point.
Return plist with keywords :specifier, :argument and :specifier."
  (let ((flags))
    (while (looking-at "\\[")
      (when-let* ((beg (point))
                  (end (ignore-errors (forward-sexp 1)
                                      (when (looking-back "\\]" 0)
                                        (point))))
                  (str (buffer-substring-no-properties (1+ beg) (1- end))))
        (save-excursion
          (goto-char (1+ beg))
          (while (re-search-forward
                  "\\(-[$A-Za-z0-9][$A-Za-z0-9]?\\)?|?\\(--[$A-Za-z0-9-]+\\)[\s\t]*\\([<][^>]+>\\)?"
                  (1- end) t 1)
            (push (parse-help-plist-remove-nils
                   (list :shortarg (match-string-no-properties 1)
                         :argument (match-string-no-properties 2)
                         :specifier (match-string-no-properties 3)))
                  flags)))
        (skip-chars-forward "\s\t\n\r\f")))
    (delete nil flags)))

(defun parse-help-squared-brackets-flags (str)
  "Search in STR for flags wrapped in squared brackets.
Return list of plists with keywords :specifier, :argument and :specifier."
  (let ((options))
    (with-temp-buffer
      (erase-buffer)
      (save-excursion (insert str))
      (while (re-search-forward "[\\[]" nil t 1)
        (forward-char -1)
        (setq options (append options (parse-help--parse-squared-brackets)))))
    options))

(defun parse-help-rows-from-string (help-output)
  "Parse flags from HELP-OUTPUT.
Return list of plists."
  (parse-help-with-output help-output
    (let ((rows))
      (while (re-search-forward parse-help-flags-regexp-2 nil t 1)
        (beginning-of-line)
        (setq rows (nconc rows (parse-help-parse-rows-forward))))
      rows)))

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

(defun parse-help-columns (regexp num command)
  "Search and parse COMMAND flags using REGEXP and NUM."
  (let ((rows))
    (while (re-search-forward regexp nil t 1)
      (forward-char -1)
      (let ((str (match-string-no-properties num))
            (descr-start (point))
            (description)
            (descr-end)
            (subflags)
            (end)
            (row))
        (setq descr-end (line-end-position))
        (let ((current-col (current-column)))
          (while
              (when (= (forward-line 1) 0)
                (when-let ((search (re-search-forward "[^\s\t\n]" (+ current-col
                                                                     (point))
                                                      t 1)))
                  (setq end search))
                (null end))))
        (setq description (buffer-substring-no-properties
                           (1- descr-start)
                           (or end
                               descr-end)))
        (setq subflags (when (not (parse-help-word-upcased-p str))
                         (parse-help-parse-output description command)))
        (setq row (append (list :argument str
                                :description (string-join
                                              (split-string description
                                                            nil t) "\s"))
                          (if subflags
                              (list :children subflags)
                            (list :choices
                                  (parse-help-choices-from-description
                                   description)))))
        (setq rows (nconc rows (list row)))))
    rows))

(defun parse-help-make-choices (specifier)
  "Return list of choices from string SPECIFIER."
  (when (string-match-p "|" (or specifier ""))
    (split-string (replace-regexp-in-string "^<\\|>$" "" specifier) "|" t)))

(defun parse-help-group-options (options)
  "Group OPTIONS into arguments and switchers."
  (seq-reduce
   (lambda (acc item)
     (let* ((specifier (plist-get item :specifier))
            (argument (plist-get item :argument))
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

(defun parse-help-parse-output (output command)
  "Parse OUTPUT from COMMAND."
  (let ((options
         (or (parse-help-rows-from-string
              output)
             (parse-help-with-output output
               (parse-help-columns "^[\s\t]?+\\([-]+[a-zZ-A-0-9_]+\\)[:]?[\s][\s]+" 1
                                   command))
             (parse-help-squared-brackets-flags
              output)))
        (commands-or-vars
         (parse-help-group-columns
          (parse-help-with-output output
            (parse-help-columns "^[\s\t]?+\\([a-zZ-A]+[a-zZ-A-0-9_]+\\)[:]?[\s][\s]+[a-zZ-A]"
                                1 command))))
        (usage (parse-help-get-usage-doc output)))
    (append `((:usage . ,usage)
              (:command . ,command))
            commands-or-vars
            (parse-help-group-options options))))

(defun parse-help-command (command)
  "Parse output from COMMAND."
  (interactive (list (read-string "Shell command")))
  (let ((output (shell-command-to-string command))
        (buff (format "*help-output-%s*" command)))
    (if (called-interactively-p 'any)
        (with-current-buffer (get-buffer-create buff)
          (pp-display-expression (parse-help-parse-output output command)
                                 (current-buffer))
          (setq header-line-format command))
      (parse-help-parse-output output command))))

(provide 'parse-help)
;;; parse-help.el ends here