;; kde-emacs-utils.el
;;
;; Copyright (C)  2002  KDE Development Team <www.kde.org>
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307  USA


(require 'kde-emacs-vars)
(require 'kde-emacs-general)
(require 'kde-emacs-compat)

(if (eq kde-emacs-type 'xemacs)
    (progn
      (require 'func-menu)
      (add-hook 'find-file-hooks 'fume-add-menubar-entry))
  (require 'imenu))

;; Switch between the declaration of a class member in .cc/.cpp/.C, and its definition in the .h file
;; Written by David and Reggie after much hair tearing
(defun switch-to-function-def ()
  (interactive)
  (let ((n (buffer-file-name))
        (class "")
        (fn "")
	(save)
	)
    (if (or (string-match "\\.cc$" n)
            (string-match "\\.cpp$" n)
            (string-match "\\.C$" n))
        (let ((a (fume-function-before-point)))
          (and (string-match "^\\(.*\\)::\\(.*\\)$" a)
               (progn
                 (setq class (match-string 1 a))
                 (setq fn (match-string 2 a))
                 (kde-switch-cpp-h)
                 (goto-char 0)
                 (re-search-forward
		  (concat "\\(class\\|struct\\|namespace\\)\\s-+"
			  class "[^;]+{") nil t)
                 ;; TODO keep looking, until we find a match that's not inside a comment
                 (re-search-forward (concat "[ \t]+" fn "[ \t]*(") nil t)))))
    (if (string-match "\\.h$" n)
        (progn
          (save-excursion
            (forward-line 0)
            (re-search-forward "[ \t]+\\([^ \t(]+\\)[ \t]*(" nil t)
            (setq fn (match-string 1))
            (re-search-backward "^\\(class\\|namespace\\) \\([a-zA-Z0-9_]+\\)[ \t]*\\([a-zA-Z0-9_]*\\)" nil t)
            (setq class (match-string 2))
            (setq save (match-string 3))
            (and (string-match "Q_EXPORT" class)
                 (setq class save))
            (message (concat class "::" fn))
            )
          (kde-switch-cpp-h)
          (goto-char 0)
          (re-search-forward (concat "^[^()]*" class "::" fn "[ \t]*(") nil t)
          (message c-syntactic-context)
          )
)))

; Credits to Arnt
(defun agulbra-make-member ()
  "make a skeleton member function in the .cpp or .cc file"
  (interactive)
  (let ((class nil)
        (function nil)
        (file (buffer-file-name))
        (insertion-string nil)
	(msubstr nil)
        (start nil))
    (save-excursion
      (and (re-search-backward "^\\(class\\|namespace\\)[ \t]" nil t)
           (progn
             (forward-word 1)
             (while (looking-at "[ \t]*Q_EXPORT")
               (forward-word 2))
             (while (looking-at "[ \t]")
               (forward-char 1))
             (setq start (point))
             (while (looking-at "[A-Za-z0-9_]")
               (forward-char 1))
             (setq class (buffer-substring start (point))))))
    (progn
      (and (looking-at "$")
           (progn
             (search-backward ")" nil t)
             (forward-char)
             (backward-sexp)))
      (and (stringp class)
           (re-search-backward "^[ \t]")
           (progn
             (while (looking-at "[ \t]")
               (forward-char 1))
             (setq start (point))
             (and (search-forward "(" nil t)
                  (progn
                    (forward-char -1)
                    (forward-sexp)))
             (and (looking-at "[ \t]+const")
                  (forward-word 1))
             (and (looking-at ";")
                  (setq function (buffer-substring start (point))))
             (re-search-forward "(" nil t))))
    (and (stringp function)
         (progn ;; get rid of virtual, static, multiple spaces, default values.
           (and (string-match "[ \t]*\\<virtual\\>[ \t]*" function)
                (setq function (replace-match " " t t function)))
           (and (string-match "^\\(virtual\\>\\)?[ \t]*" function)
                (setq function (replace-match "" t t function)))
           (and (string-match "^\\(static\\>\\)?[ \t]*" function)
                (setq function (replace-match "" t t function)))
           (while (string-match "  +" function)
             (setq function (replace-match " " t t function)))
           (while (string-match "\t+" function)
             (setq function (replace-match " " t t function)))
           (while (string-match " ?=[^,)]+" function)
             (setq function (replace-match " " t t function)))
           (while (string-match " +," function)
             (setq function (replace-match "," t t function)))))
    (and (stringp function)
         (stringp class)
         (stringp file)
         (progn
           (cond ((string-match (concat "^ *" class "[ \\t]*(") function)
                  (progn
                  (setq insertion-string
                        (concat
                         (replace-match
                          (concat class "::" class "(")
                          t t function)
                         "\n{\n    \n}\n"))))
                  ((string-match (concat "^ *~" class "[ \\t]*(") function)
                   (progn
                     (setq insertion-string
                           (concat
                            (replace-match
                             (concat class "::~" class "(")
                             t t function)
                            "\n{\n    \n}\n"))))
                  ((string-match " *\\([a-zA-Z0-9_]+\\)[ \\t]*(" function)
                   (progn
                     (setq insertion-string
                           (concat
                            (replace-match
                             (concat " " class "::" "\\1(")
                             t nil function)
                            "\n{\n    \n}\n"))))
                  (t
                   (error (concat "Can't parse declaration ``"
                                  function "'' in class ``" class
                                  "'', aborting"))))
           (stringp insertion-string)))
	 (kde-switch-cpp-h)
         (progn
           (goto-char (point-max))
	   (kde-comments-begin)
	   (kde-skip-blank-lines)
	   (setq msubstr (buffer-substring (point-at-bol) (point-at-eol)))
	   (if (string-match "^#include.*moc.*" msubstr)
	       (progn 
		 (forward-line -1)
		 (end-of-line)
		 (insert "\n")))
	   (if (string-match "}" msubstr)
	       (progn
		 (end-of-line)
		 (insert "\n")
		 (forward-line 1)
	     ))
	   (insert insertion-string)
	   (forward-char -3)
	   (save-excursion
	     (and (string-match ".*/" file)
		  (setq file (replace-match "" t nil file)))
             (if (functionp 'kdab-insert-include-file)
                 (kdab-insert-include-file file 't nil)))))
  (when (featurep 'fume-rescan-buffer)
    (fume-rescan-buffer))
  )


; Adds the current file to Makefile.am.
; Written by David.
(defun add-file-to-makefile-am ()
  "add the current file to the _SOURCES tag in the Makefile.am"
  (interactive)
  (let ((file (buffer-name))
        (makefile "Makefile.am"))
    (if (not (file-readable-p makefile))
	(error "Makefile.am not found!")
      )
    (find-file makefile)
    (goto-char (point-min))
    (if (re-search-forward "_SOURCES" nil t)
	(progn
	  (end-of-line)
          ; check if line ends with '\' [had to read make-mode.el to find this one!]
	  (while (= (char-before) ?\\)
	    (end-of-line 2)) ; moves to end of next line
	  (insert " ")
	  (insert file)
	  )
      (error "_SOURCES not found")
      )
    )
  )


; Inserts a kdDebug statement showing the name of the current method.
; You need to create the empty line first.
(defun insert-kdDebug ()
  (interactive)
  (insert "kdDebug() << ")
  ;; no unnecessary fume-* functions which aren't available on GNU/Emacs
  (insert "k_funcinfo")
  (insert " << endl;")
  )

; finds a string to be used in the header-protection function ( see below )
(defun kde-header-protection-definable-string ()
   (let* ((definablestring "")
	  (f (buffer-file-name))
	  (parts (nreverse (split-string f "/")))
	  (i)
	  (first-iter t)
	  (iters (min (length parts) kde-header-protection-parts-to-show)))
     (dotimes (i iters)
       (let ((part (pop parts)))
	 (setq definablestring
	       (concat
		(upcase (replace-in-string part "[\\.-]" "_"))
		(if (not first-iter) "_" "")
		definablestring
		)
	       )
	 (setq first-iter nil)
	 )
       )
     definablestring
     )
   )

; Creates the ifndef/define/endif statements necessary for a header file
(defun header-protection ()
  (interactive)
  (let ((s (kde-header-protection-definable-string)))
    (save-excursion
      (goto-char (point-min))
      (insert "#ifndef " s "\n#define " s "\n\n")
      (goto-char (point-max))
      (insert "\n#endif\n")
      )
    )
  )

; Makes '(' insert '(' or ' ( ' where appropiate
(defun insert-parens (arg) (interactive "*P")
  (if (not (c-in-literal))
      (let ((n nil))
        (save-excursion
          (setq n (or (progn (forward-char -2) (looking-at "if"))
                      (progn (forward-char -1) (looking-at "for"))
                      (progn (forward-char -1) (looking-at "case"))
                      (progn (forward-char -1) (looking-at "while"))
                      )
                )
          )
        (cond
         (n (progn
              (insert " ")
              (self-insert-command (prefix-numeric-value arg))
              (insert kde-emacs-after-parent-string)
              ))
         (t ;else
          (self-insert-command (prefix-numeric-value arg))
          (insert kde-emacs-after-parent-string)
          )))
    (self-insert-command (prefix-numeric-value arg)))
  )

(defun insert-parens2 (arg) (interactive "*P")
  (if (not (c-in-literal))
      (let ((remv nil) (nospac nil))
        (forward-char -2)
        (setq remv (looking-at "( ")) ; () -> we'll have to remove that space
        (forward-char 1)
        (setq nospac (or (looking-at " ") (looking-at "(")) ) ; no space to be added
        (forward-char 1)
        (cond
         (remv (progn
                 (delete-backward-char 1)
                 (self-insert-command (prefix-numeric-value arg)))) ; the () case
         (nospac (self-insert-command (prefix-numeric-value arg))) ; no space to be added
         (t ;else
          (if abbrev-mode ; XEmacs
              (expand-abbrev))
          (insert kde-emacs-after-parent-string)
          (self-insert-command (prefix-numeric-value arg))
          ))) ; normal case, prepend a space
    ;;(blink-matching-open) ; show the matching parens
    (self-insert-command (prefix-numeric-value arg)))
  )

; Makes ',' insert ', '
(defun insert-comma (arg)
  (interactive "*P")
  (let* ((ch (char-after))
         (spacep (not (or (eq ch ? )
                          (c-in-literal)
                          arg))))
    (self-insert-command (prefix-numeric-value arg))
    (if spacep
	(insert " "))))

(defun insert-semicolon (arg)
  (interactive "*P")
    (self-insert-command (prefix-numeric-value arg))
    (newline-and-indent))

(defun insert-curly-brace (arg) (interactive "*P")
  (if (not (c-in-literal))
      (let ((n nil) (o nil)
	    (spacep nil))
        (save-excursion
	  (forward-char -1)              ; These three lines are for the situation where
          (if (not (looking-at " "))     ; the user already have inserted a space after
              (forward-char 1)           ; the closing parenthesis
	    (setq spacep t))
          (forward-char -2)
          (setq o (looking-at "()"))
          (forward-char 1)
          (setq n (looking-at ")"))
          )
        (cond
         (n (progn
              (if (not spacep) (insert " "))
              (self-insert-command (prefix-numeric-value arg))
              (newline-and-indent)
             (save-excursion
              (insert "\n}")
              (c-indent-line)
              )))
         (o (progn
              (newline)
              (self-insert-command (prefix-numeric-value arg))
              (newline-and-indent)))
         (t (progn ;else
              (self-insert-command (prefix-numeric-value arg))
              (save-excursion
                (beginning-of-line)
                (c-indent-command))))
         ))
    (self-insert-command (prefix-numeric-value arg))
    )
)

;; have PelDel mode work
(put 'insert-parens 'pending-delete t)
(put 'insert-parens2 'pending-delete t)
(put 'insert-comma 'pending-delete t)
(put 'insert-curly-brace 'pending-delete t)
(put 'newline-and-indent 'pending-delete t)

; A wheel mouse that doesn't beep, unlike mwheel-install
(defun scroll-me-up () (interactive) (scroll-up 4))
(defun scroll-me-down () (interactive) (scroll-down 4))
(defun scroll-me-up-a-bit () (interactive) (scroll-up 1))
(defun scroll-me-down-a-bit () (interactive) (scroll-down 1))

; Compilation
(defun makeclean ()
  "Executes a \"make clean\" in the current directory"
  (interactive)
  (compile "make clean")
  )

(defun make ()
  "Executes a \"make\" in the current directory"
  (interactive)
  (compile "make -k")
  )

(defun makeinstall ()
  "Executes a \"make install\" in the current directory"
  (interactive)
  (compile "make -k install")
  )

(defun makeinstallexec ()
  "Executes a \"make install-exec\" in the current directory"
  (interactive)
  (compile "make -k install-exec")
  )

(defun makethisfile ()
  "Try to compile the currently opened file"
  (interactive)
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (if (string-match "\.cpp$" f) (setq f (replace-match "\.lo" t t f)))
    (if (string-match "\.cc$" f) (setq f (replace-match "\.lo" t t f)))
    (compile (concat "make " f )))
  )

;; pc-like textmarking
(when kde-use-pc-select
  (load "pc-select")
  (funcall 'pc-select-mode))

; Move in other window
(defun scroll-other-up () (interactive) (scroll-other-window-down 1)) ; hehe :)
(defun scroll-other-down () (interactive) (scroll-other-window 1))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun kde-start-c++-header ()
  "Start a new C++ header by inserting include guards ( see \
   header-protection function ), inserting a license statement \
   and putting (point) at the correct position"
  (interactive)
  (header-protection)
  (insert "\n")
  (beginning-of-buffer)
  (kde-license-insert "GNU GPL")
  (next-line 1)
  (kill-line)
  (end-of-buffer)
  (next-line -3)
  (insert "\n")
)

(defun kde-year-range-parse-years-string (string)
  "parses something like \"2000, 2008-2010\" into a list of the form \
   ((2008 . 2010)(2000 . 2000))"
  (let ((pos -1)
	(oldpos)
	(l (length string))
	(currange "")
	(startyear)
	(endyear)
	(ret)
	)
    (while (< pos l)
      (setq oldpos (+ pos 1))
      (setq pos (string-match "[,]" string (+ pos 1)))
      (unless pos (setq pos l))
      (setq currange (substring string oldpos pos))
      (string-match "[0-9]+" currange)
      (setq startyear (string-to-int (match-string 0 currange)))
      (setq endyear
	    (if (string-match "-" currange)
		(string-to-int (substring currange (match-end 0)))
	      startyear))
      (setq ret (cons (cons startyear endyear) ret))
      )
    ret
    )
  )

(defun kde-year-range-contains-year (ranges year)
  "checks whether year is in ranges.. ( ranges is a list as \
   kde-year-range-parse-years-string returns.. "
  (let ((ret))
    (dolist (range ranges ret)
      (when (and (>= year (car range)) (<= year (cdr range)))
	(setq ret t))
      )))

(defun kde-year-range-to-string (ranges)
  "converts ranges to a string.."
  (let ((ret ""))
    (dolist (range ranges)
      (setq ret 
	    (concat
	     (int-to-string (car range))
	     (if (/= (cdr range) (car range))
		 (concat "-" (int-to-string (cdr range)))
	       "")
	     ", "
	     ret) 
	    )
      )
    ; remove extraneous ", "
    (setq ret (substring ret 0 (- (length ret) 2)))
    )
  )

; merges adjacent year ranges into one..
(defun kde-year-range-cleanup (range)
  (let ((origrange range))
    (while (and range (cdr range))
      (let ((years (car range)) (nyears (cadr range)))
	(when (>= (+ (cdr nyears) 1) (car nyears))
	  (setcar range (cons (car nyears) (cdr years)))
	  (setcdr range (cddr range)))
	)
      (setq range (cdr range))
      )
    origrange
    )
  )

; adds year to range..
(defun kde-year-range-add-year (range year)
  (while range
    (let ((years (car range)))
      (cond
       ((and (>= year (car years)) (<= year (cdr years))
	     ; year is already in the range..
	     (setq range nil)))
       ((= year (+ (cdr years) 1))
	(setcdr years year)
	(setq range nil))
       ((= year (- (car years) 1))
	(setcar years year)
	(setq range nil))
       )
      )
    (setq range (cdr range))
    )
  (kde-year-range-cleanup range)
  )

(defun kde-add-copyright () (interactive)
  "Tries to add your kde-full-name and kde-email to the Copyright \
   statements at the top of a file...  It tries to figure out \
   if it's already there, and if so, updates the line to include the \
   current year.. ( well, replaces it by a new one, anyway :) )"
  (let ((wascomment ""))
    (save-excursion
      (beginning-of-buffer)
      (if (re-search-forward (concat "Copyright ([Cc]) \\([0-9 ,-]*\\) " kde-full-name) nil t)
	  (progn
	    (beginning-of-line)
	    (let ((years (kde-year-range-cleanup (kde-year-range-parse-years-string (match-string 1))))
		  (new-copyright-string "Copyright (C) ")
		  (this-year (string-to-int (format-time-string "%Y"))))
	      (when (not (kde-year-range-contains-year years this-year))
		(kde-year-range-add-year years this-year))
	      (setq new-copyright-string
		    (concat new-copyright-string (kde-year-range-to-string years)))
					; finish new-copyright-string 
	      (setq new-copyright-string
		    (concat new-copyright-string "  " kde-full-name " <" kde-email ">"))
	      (beginning-of-line)
	      (re-search-forward "Copyright ([Cc])")
	      (beginning-of-line)
	      (setq wascomment 
		    (buffer-substring (point)
				      (match-beginning 0)
				      ))
	      (kill-line nil)
	      (insert new-copyright-string)
	      )
	    )
	(beginning-of-buffer)
	(let ((first-copyright-str (re-search-forward "Copyright ([Cc])" nil t)))
	  (if first-copyright-str
	      (progn
		(goto-char first-copyright-str)
		(beginning-of-line)
		(setq wascomment (buffer-substring (point) (match-beginning 0)))
		(forward-line 1)
		)
	    (goto-line 2))
	  )
	(beginning-of-line)
	(insert "Copyright (C) " (format-time-string "%Y") "  "
		kde-full-name " <" kde-email ">\n")
	(forward-line -1)
	)
      (end-of-line)
      (let ((end (point)))
	(beginning-of-line)
	(insert wascomment)
	)
      )
    )
  )

(defun kde-emacs-file-style-update ()
  "Updates the style header of this file"
  (interactive)
  (if (or (eq major-mode 'c++-mode)
	  (eq major-mode 'c-mode))
      (let ((startpoint) (endpoint)
	    (firstline) (strings)
	    (str) (m) (m2) (var) (value)
	    (final))
	(save-excursion
	  (beginning-of-buffer)
	  (setq startpoint (point))
	  (setq endpoint (point-at-eol)))
	(setq firstline (buffer-substring startpoint endpoint))
	(if (string-match "-\*-\\([A-Za-z0-9\-\+\:\; ]+\\)-\*-" firstline)
	    (delete-region startpoint endpoint))
	(setq final (concat "-*- "
			    "Mode: " mode-name "; "
			    "c-basic-offset: " (prin1-to-string c-basic-offset)  "; "
			    "indent-tabs-mode: " (prin1-to-string indent-tabs-mode)  "; "
			    "tab-width: " (prin1-to-string tab-width) "; "
			    "-*-"))
	(save-excursion
	  (beginning-of-buffer)
	  (insert final)
	  (comment-region (point-at-bol) (point-at-eol))
	  (newline)))))


(provide 'kde-emacs-utils)
