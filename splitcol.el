(defun org-split-src-block-at-cursor ()
  "Split the current `begin_src` block into two new source blocks under level-4 headlines.
The split occurs at the current cursor position, and `:BMCOL:` is added to the enclosing level-3 headline."
  (interactive)
  (save-excursion
    (let* ((block (org-element-at-point))
           ;; Ensure the cursor is inside a valid src block
           (block-type (org-element-type block))
           (lang (and (eq block-type 'src-block)
                      (org-element-property :language block)))
           (begin (and block (org-element-property :begin block)))
           (end (and block (org-element-property :end block)))
           (foo (message (format "begin %s    end %s   " begin end)))
           (cursor-pos (point))

           (foo (message (format "Got point %s" cursor-pos)))
           (reg-start (save-excursion
                        (goto-char begin)
                        (forward-line 1)
                        (back-to-indentation)
                        (point)))
           (foo (message (format "Reg start %s" reg-start)))
           (reg-end (save-excursion
                      (goto-char end)
                      (forward-line -1)
                      (back-to-indentation)
                      (point)))
           (foo (message (format "Reg end %s" reg-end)))
           ;; Extract content safely
           (content-before (and begin
                                (< cursor-pos end)
                                (buffer-substring-no-properties
                                 reg-start ;; Skip #+begin_src
                                 cursor-pos)))
           (foo (message "Got before"))
           (content-after (and end
                               (buffer-substring-no-properties
                                cursor-pos
                                reg-end))) ;; Skip #+end_src
           (foo (message "Got after"))
           )
      (unless (eq block-type 'src-block)
        (error "Cursor is not inside a valid source block"))
      ;; Add :BMCOL: to the enclosing level-3 headline
      ;; Delete the original block
      (goto-char begin)
      (delete-region begin end)
      ;; Insert new level-4 headlines with split source blocks
      (insert (format
               "**** Column 1
:PROPERTIES:
:BEAMER_col: 0.5
:END:
#+begin_src %s
%s
#+end_src

**** Column 2
:PROPERTIES:
:BEAMER_col: 0.5
:END:
#+begin_src %s
%s
\n"
               lang (or content-before "") lang (or content-after "")))
      (let* ((block (org-element-at-point))
             (parent-headline-pos (and block
                                       (save-excursion
                                         (or (org-up-heading-safe) (error "No enclosing headline"))
                                         (point)))))

        (goto-char parent-headline-pos)
        (org-set-property "BMCOL" "")
        ))))
