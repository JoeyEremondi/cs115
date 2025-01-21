;;; tform.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Joey Eremondi
;;
;; Author: Joey Eremondi <joey@eremondi.com>
;; Maintainer: Joey Eremondi <joey@eremondi.com>
;; Created: December 05, 2024
;; Modified: December 05, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/joey/tform
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'strie)
(require 'seq)
;; (require '(emacs 28))
;; (require 'cl-lib)

(setq type-pat (rx (| "int" "bool" "void" "float" "double")))

(defun matches-code-pattern (line)
  (cl-some (lambda (pattern)
             (string-match-p pattern line))
           (list (rx (regex type-pat)  (* nonl) "(" (* (regex type-pat) (* nonl) ",") ")")
                 (rx "{")
                 (rx "}")
                 (rx ";")
                 (rx "//")
                 (rx (| "while" "if") (* space) "(")
                 (rx "case "  (* nonl) ":")
                 (rx "else" (* space ) eol)
                 (rx "else" (+ space))
                 (rx "else" (* space) "{")
                 (rx "#" (| "define" "include" "pragma" "DEFINE" "INCLUDE" "PRAGMA"
                            "ifdef" "ifndef" "endif" "IFDEF" "IFNDEF" "ENDIF"
                            "if" "IF" "else" "ELSE" "elif" "ELIF" "undef" "UNDEF"))
                 (rx "default" (* space) ":" )
                 (rx "public" (* space) ":")
                 (rx "private" (* space) ":")
                 (rx "protected" (* space) ":")
                 )))


(defun is-code (line current-in)
  (and (not (string-match-p (rx bol (* space) "**") line))
       ;; Either we're whitespace in the middle of a code block, or a code line
       (or (and current-in (or
                            (string-match-p "^\\s-*$" line)
                            (string-match-p (rx "...") line)))
           (matches-code-pattern line))))

(defun write-result-line (str)
  (write-region (concat str "\n") nil "result.org" 'append))

(defun my/walk-line-by-line ()
  "Process each line in the buffer one by one."
  (interactive)
  (save-excursion
    (shell-command "rm result.org")
    ;; Make a trie with all the strings of the txt version of the file that has bullet points
    (setq tr (strie-new))
    ;; Add the lines to it
    (with-temp-buffer
      (insert-file-contents "all.txt")
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line 1)
        (strie-add tr
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))
                   nil)))
    ;; Track whether we're currently in a code block
    (setq currently-in-code nil)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((lb (line-beginning-position))
             (le (line-end-position))
             (ln-raw (buffer-substring-no-properties lb le))
             (replacement-pairs '(("É" . "...")
                                  ("Ê" . " ")
                                  ("ÕÕ" . "''")
                                  ("Õ" . "'")
                                  ("Ò" . "\"")
                                  ("Ó" . "\"")
                                  ("Ñ" . "--")))
             (ln (seq-reduce (lambda (str pr) (string-replace (car pr) (cdr pr) str)) replacement-pairs ln-raw)))
        (setq is-c (is-code ln currently-in-code))
        (when (and is-c (not currently-in-code))
          ;; First line of code, emit start of block
          (write-result-line  "\n#+begin_src c++" )
          (setq currently-in-code t))
        (when (and (not is-c) (and currently-in-code))
          ;; After last  line of code, emit end of block
          (write-result-line "#+end_src\n")
          (setq currently-in-code nil))
        ;; Check if we need to add an org-bullet
        ;;     ;; If empty, don't do matching
        (cond ((string-match-p "^\\s-*$" ln)
               (write-result-line " "))
              ;; Level 2 bullet
              ((strie-complete tr (concat "  • " ln))
               (write-result-line (format "   + %s " ln)))
              ;; Headline
              ((string-match-p (rx (* space) (+ "*")) ln)
               (write-result-line (format "%s" ln)))
              ;; No bullets for code
              (currently-in-code (write-result-line (format "    %s" ln)))
              ;; Default: Level 1 bullet
              ;; ((strie-complete tr (concat "• " ln))
              ;;  (write-result-line  (format " - %s " ln)))
              (t (write-result-line (format " - %s" ln))))
        (forward-line 1)))))


(defun org-format-all-src-blocks ()
  "Format all source blocks in the current org-mode buffer."
  (interactive)
  (org-save-outline-visibility t
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+begin_src" nil t)
      (org-indent-block)
      ))) ;; Save and exit the block
