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
		  (concat "\\(class\\|struct\\|namespace\\)[^{;]+"
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
           (stringp insertion-string))
         (string-match "\\.h$" file)
         (setq f (replace-match ".cpp" t t file))
	 (if (file-readable-p f )
	     t
           (progn
	     (string-match "\\.h$" file)
	     (setq f (replace-match ".cc" t t file))
	     t
	     )))
         (find-file f)
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
	     (or (re-search-backward
		  (concat "^#include *[<\"]" file "[>\"]$") nil t)
		 (progn
		   (goto-char (point-min))
		   (re-search-forward "^$" nil t)
		   (insert "\n#include \"" file "\"\n"))))
	   ))
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

; Creates the ifndef/define/endif statements necessary for a header file
(defun header-protection ()
  (interactive)
  (let ((f (buffer-file-name)))
    (if (string-match "^.*/" f)
      (setq f (replace-match "" t t f)))
    (while (string-match "\\." f)
      (setq f (replace-match "_" t t f)))
    (save-excursion
      (goto-char (point-min))
      (insert "#ifndef " (upcase f) "\n#define " (upcase f) "\n\n")
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
              ;(insert " ")
              ))
         (t ;else
          (self-insert-command (prefix-numeric-value arg))
          ;(insert " ")
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
          ;(insert " ")
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

(defun insert-curly-brace (arg) (interactive "*P")
  (if (not (c-in-literal))
      (let ((n nil) (o nil))
        (save-excursion
          (forward-char -2)
          (setq o (looking-at "()"))
          (forward-char 1)
          (setq n (looking-at ")"))
          )
        (cond
         (n (progn
              (insert " ")
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
  (let ((f (buffer-name)))
    (if (string-match "\.cpp$" f) (setq f (replace-match "\.lo" t t f)))
    (if (string-match "\.cc$" f) (setq f (replace-match "\.lo" t t f)))
    (compile (concat "make " f )))
  )

;; pc-like textmarking
(load "pc-select")
(if (eq kde-emacs-type 'xemacs)
    (funcall 'pc-select-mode)
  (funcall 'pc-selection-mode))

; Move in other window
(defun scroll-other-up () (interactive) (scroll-other-window-down 1)) ; hehe :)
(defun scroll-other-down () (interactive) (scroll-other-window 1))

(provide 'kde-emacs-utils)
