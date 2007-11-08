;; kde-emacs-utils.el
;;
;; Copyright (C)  2002-2005  KDE Development Team <www.kde.org>
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301  USA


(require 'kde-emacs-vars)
(require 'kde-emacs-general)
(require 'kde-emacs-compat)

(if (eq kde-emacs-type 'xemacs)
    (progn
      (require 'func-menu)
      (add-hook 'find-file-hooks 'fume-add-menubar-entry))
  (require 'imenu))

(defmacro c-safe-scan-lists (from count depth)
  "Like `scan-lists' but returns nil instead of signalling errors.
This function does not do any hidden buffer changes."
  (if (featurep 'xemacs)
      `(scan-lists ,from ,count ,depth nil t)
    `(c-safe (scan-lists ,from ,count ,depth))))

(if (not (eq kde-include-directory nil))
    (setq sourcepair-header-path (list "." kde-include-directory "/*")))

;; returns non-nil if the given file has a using declaration
;; with the passed namespace
(defun kde-file-has-using (namespace)
  (let (found)
    (save-excursion
      (beginning-of-buffer)
      (setq found (re-search-forward "^using" nil 1))
      (if found
	  (setq found (search-forward namespace (line-end-position) 1))
	)
      )
    found)
  )
;; returns non-nill if the given file has a "namespace SomeNM" declaration
;; where SomeNM is passed via the namespace argument
(defun kde-file-is-in-namespace (namespace)
  (let (found)
    (save-excursion
      (beginning-of-buffer)
      (setq found (re-search-forward "^namespace" nil 1))
      (if found
	  (setq found (search-forward namespace (line-end-position) 1))
	)
      )
    found)
  )

; Helper function for getting the baseclass of the current class - in a C++ header file
; Only supports single inheritance
(defun baseclass-under-point ()
  (let ((pos (c-safe-scan-lists (point) -1 1)))
    (save-excursion
      (goto-char (if pos pos (point-min)))
      (backward-word 2)			; move back over "public baseclass"
      (if (looking-at "public\\|protected\\|private[ \t]*")
	  (progn
	    (forward-word)
	    (while (looking-at "[ \t]")
	      (forward-char 1))
	    (let ((start (point)))
	      (forward-word)
	      (buffer-substring start (point))))
	nil
	)))
    )

; Helper function for parsing our current position in a C++ header file
; returns (namespace (class function)) where (a b) is a cons.
(defun method-under-point ()
  (let ((class nil)
        (namespace "") ; will contain A::B::
        (function nil))
    (save-excursion
      (backward-char)             ; in case we're after the ';'
      (search-forward ";" nil t)  ; look for the ';'
      (backward-char)
      (save-excursion
        ; Go up a level, skipping entire classes etc.
        ; This is a modified version of (backward-up-list) which doesn't
        ; throw an error when not found.
	(let ((pos (c-safe-scan-lists (point) -1 1)))
	; +1 added here so that the regexp in the while matches the { too.
	  (goto-char (if pos (+ pos 1) (point-min))))
	(while (re-search-backward "^[ ]*\\(class\\|namespace\\|struct\\)[ \t][^};]*{" nil t)
	  (save-excursion
	    (forward-word 1)
	   (when (looking-at "[ \t]*[A-Z_]*_EXPORT[A-Z_]*[ \t]")
	     (forward-word 1)
	     (re-search-forward "[ \t]" nil t))
	    (while (looking-at "[ \t]")
	      (forward-char 1))
	    (setq start (point))
	    ; Parse class name ("Foo" or "Foo::Bar::Blah"). 
	    ; Beware of "Foo:"
	    (while (or (looking-at "[A-Za-z0-9_]") (looking-at "::"))
	      (while (looking-at "[A-Za-z0-9_]")
		(forward-char 1))
	      (while (looking-at "::")
		(forward-char 2))
	      )
	    (cond
	     (class			; class found already, so the rest goes into the namespace
	      (setq namespace (concat (buffer-substring start (point)) "::" namespace)))
	     (t				; class==nil
	      (setq class (buffer-substring start (point)))))
	    )
        ; Go up one level again
	(let ((pos (c-safe-scan-lists (point) -1 1)))
	  (goto-char (if pos (+ pos 1) (point-min))))
	))

    ; Back to where we were, parse function name
    (let ((end (point)))          ; remember where the function decl ends
      (search-backward ")" nil t) ; look back for the end of the argument list
      (forward-char)
      (backward-sexp)             ; brings us back to the '('
      (backward-word 1)
      (when (looking-at "throw[ \t]") ; exception specification, look for () again
             (search-backward ")" nil t)
             (forward-char)
             (backward-sexp))
      ; now that we moved back enough, go to beginning of line.
      ; (we assume that the return type, function name, and '(' are on the same line)
      (re-search-backward "^[ \t]*")
      (while (looking-at "[ \t]")
	(forward-char 1))
      (when (looking-at "/\\*") ; C-style comment, like /*! \reimp */
	(re-search-forward "\\*/" nil t))
      (while (looking-at "[ \t]")
	(forward-char 1))
      (setq function (buffer-substring (point) end))
      )
    ) ; end of global save-excursion
    (cons namespace (cons class function)) ; the returned value
    )
  )

; get rid of virtual, static, multiple spaces, default values.
(defun canonical-function-sig (function)
  (and (string-match "[ \t]*\\<virtual\\>[ \t]*" function)
       (setq function (replace-match " " t t function)))
  (and (string-match "^\\(virtual\\>\\)?[ \t]*" function)
       (setq function (replace-match "" t t function)))
  (and (string-match "^\\(explicit\\>\\)?[ \t]*" function)
       (setq function (replace-match "" t t function)))
  (and (string-match "^\\(static\\>\\)?[ \t]*" function)
       (setq function (replace-match "" t t function)))
  (while (string-match "  +" function) ; simplifyWhiteSpace
    (setq function (replace-match " " t t function)))
  (while (string-match "\t+" function)
    (setq function (replace-match " " t t function)))
  (while (string-match "^ " function)  ; remove leading whitespace
    (setq function (replace-match "" t t function)))
  (let ((startargs (string-match "(" function)))
    (while (string-match " ?=[^,)]+" function startargs) ; remove default values
      (setq function (replace-match " " t t function))))
  (while (string-match " +," function) ; remove space before commas
    (setq function (replace-match "," t t function)))
  function ; the return value
)

; Helper method which turns the function as seen in the header
; into the signature for its implementation
; Returns the fully-qualified signature of the function implementation
(defun kde-function-impl-sig (namespace class _function)
  (let (
	(function (canonical-function-sig _function))
	(insertion-string nil))
    (and (stringp class)
	 (cond
	  ((string-match (concat "^ *" class "[ \\t]*(") function) ; constructor
	   (setq insertion-string
		 (replace-match
		  (concat namespace class "::" class "(")
		  t t function)
		 ))
	  ((string-match (concat "^ *~" class "[ \\t]*(") function) ; destructor
	   (setq insertion-string
		 (replace-match
		  (concat namespace class "::~" class "(")
		  t t function)
		 ))
	  ))				; end of "class required"
    (if (not (stringp insertion-string)) ; no ctor nor dtor
	(if (or (string-match " *\\([a-zA-Z0-9_]+\\)[ \\t]*(" function) ; normal method
		(string-match " *\\(operator[^ \\t]+\\)[ \\t]*(" function)) ; operator
	      (setq insertion-string
		    (replace-match
		     (if class
			 (concat " " namespace class "::" "\\1(") ; c++ method
		       (concat " " "\\1(")) ; c function
		     t nil function)
		    )
					; else
	  (error (concat "Can't parse declaration ``"
			 function "'' in class ``" class
			 "'', aborting"))))
    insertion-string ; the return value
    )
  )

;; Switch between the declaration of a class member in .cc/.cpp/.C, and its definition in the .h file
;; Written by David and Reggie after much hair tearing
;; Found since, might be worth looking at: http://www.hendawi.com/emacs/sourcepair.el
(defun switch-to-function-def ()
  (interactive)
  (let ((n (buffer-file-name))
        (namespace "")
        (class "")
        (function "")
        found
	)
    (if (member (concat "." (file-name-extension n)) sourcepair-source-extensions)
        ; TODO replace fume-function-before-point, needed for emacs,
        ; and for better namespace support.
	;(progn
	;  (let ((pos (kde-scan-lists (point) -1 1 nil t))) ; Go up a level
	;    (goto-char (if pos (+ pos 1) (point-min))))
        (let ((a (fume-function-before-point))
	      (functionregexp ""))
          
          (if (eq a nil)
              (progn
                (kde-switch-cpp-h)
                (message "point is not in a method"))
            (if (string-match "^\\(.*\\)::\\(.*\\)$" a)
                (progn
                  (setq class (match-string 1 a))
                  (setq function (match-string 2 a))
                  (kde-switch-cpp-h)
                  (goto-char 0)
                                        ; Look for beginning of class ("\\s-+" means whitespace including newlines)
                  (re-search-forward
                   (concat "\\(class\\|struct\\|namespace\\)\\s-+"
                           "\\([A-Z_]+_EXPORT[A-Z_]*\\s-+\\)?"  ; allow for optional EXPORT macro
                           class "\\b"                          ; the classname - with word separator
                           "[^;]+{"                             ; the optional inheritance and the '{'
                           ) nil t)                             ; no error, just return nil if not found

                                        ; Look for function - with \\b prepended, unless this is about ~Foo.
                  (setq functionregexp (kde-function-regexp-quote function))
                  (and (not (string-match "^~" functionregexp))
                       (setq functionregexp (concat "\\b" functionregexp)))
                  ;; TODO keep looking, until we find a match that's not inside a comment
                  (re-search-forward (concat functionregexp "[ \t]*(") nil t))
                                        ; else: not a member method, maybe just a c function
              (progn
                (setq function a)
                (kde-switch-cpp-h)
                (goto-char 0)
                (re-search-forward (concat "\\b" (kde-function-regexp-quote function) "[ \t]*(") nil t))
              )
            )
          ))
    (if (string-match "\\.h$" n)
        (progn
	  (let ((mup (method-under-point))
		(sig "")
		(pos 0))
        (setq namespace (car mup))
	    (setq class (cadr mup))
	    (setq function (cddr mup))
	    (kde-switch-cpp-h)

        ;; First search with namespace prefixed
	    (goto-char 0)
	    (setq sig (kde-remove-newline (kde-function-impl-sig namespace class function)))
	    (if (string-match "(.*" sig) ; remove args
		(setq sig (replace-match "" nil t sig)))
	    (setq found (re-search-forward (concat "^[^()]*" (kde-function-regexp-quote sig) "[ \t]*(") nil t) )

        (if (not found)
            (progn
              ; Now search without name space prefix

              (goto-char 0)
              (setq sig (kde-remove-newline (kde-function-impl-sig "" class function)))
              
              (if (string-match "(.*" sig) ; remove args
                  (setq sig (replace-match "" nil t sig)))
              (re-search-forward (concat "^[^()]*" (kde-function-regexp-quote sig) "[ \t]*(") nil t) ) )
	    )))))

(defun kde-remove-newline (str) 
    (replace-in-string str "\n" " "))
; quote for use as regexp, but replace spaces with "any whitespace"
(defun kde-function-regexp-quote (str)
  (replace-in-string (regexp-quote str) "[ \n\t]" "[ \n\t]"))

; Initial implementation by Arnt Gulbransen
; Current maintainer: David Faure
(defun agulbra-make-member ()
  "make a skeleton member function in the .cpp or .cc file"
  (interactive)
  (let* (
	 (mup (method-under-point))
	 (namespace (car mup))  ; will contain A::B::
	 (class (cadr mup))
	 (function (cddr mup))
	 (file (buffer-file-name))
	 (insertion-string (kde-function-impl-sig namespace class function))
	 (function-sig (canonical-function-sig function))
	 (msubstr nil)
	 (start nil)
	 (newcppfile nil)
	 (baseclass nil)
	 )
    ; First, assemble the skeleton text into insertion-string
    ; At this point it already contains the method signature

    ; If constructor: add call to base class 
    (and (stringp class)
	 (string-match (concat "^ *" class "[ \\t]*(") function-sig) ; constructor
	 (setq baseclass (baseclass-under-point))
	 ; TODO: passing the parent parameter if baseclass starts with Q :)
	 (setq insertion-string (concat insertion-string "\n    : " baseclass "()" )))

    ; Method body
    (setq insertion-string 
	  (concat insertion-string "\n{\n"
		  (replace-in-string kde-make-member-default-impl "FUNCTION" 
				     ; the function name and args, without newlines
				     (replace-in-string insertion-string "\n" " " t)
				     t)
		  "}\n"))

    ; Move to next method, to be ready for next call
    (backward-char)                ; in case we're after the ';'
    (re-search-forward ";" nil t)  ; end of this method decl
    (let ((moveToNext t))
      (while moveToNext
	(re-search-forward ";" nil t)  ; end of next method decl
	(save-excursion
	  (forward-char -2) ; -1 goes to ';' itself, so go before that
	  (while (looking-at "[ \t0=]")
	    (forward-char -1))
	  (forward-char 1)
          ; move to next method again if we're at a pure virtual method
	  (setq moveToNext (looking-at "[ \t]*=[ \t]*0;"))
	  )
	)
      )

    ; Switch to .cpp if the declaration was in a header file
    (if (member (concat "." (file-name-extension file)) sourcepair-header-extensions)
	(kde-switch-cpp-h)
      )
    ;(setq newcppfile (= (point-max) 1))
    (goto-char (point-max))
    (kde-comments-begin)
    (kde-skip-blank-lines)
    (setq msubstr (buffer-substring (point-at-bol) (point-at-eol)))
    (if (string-match "^#include.*moc.*" msubstr) ; TODO refine regexp
	(progn 
	  (forward-line -1)
	  (end-of-line)
	  (insert "\n"))
    ; else
      (progn
	  (end-of-line)
	  (insert "\n")
	  (forward-line 1)
	  ))
    (insert insertion-string)
    (forward-char -3)
    (c-indent-defun)
    ; Insert #include for the header if necessary
    (save-excursion
      (and (string-match ".*/" file)
	   (setq file (replace-match "" t nil file)))
      (and (string-match "\\.h$" file)
	   (functionp 'kdab-insert-header-non-interactive)
       (kdab-insert-header-non-interactive (file-name-sans-extension file))))
    (when (featurep 'fume-rescan-buffer)
      (fume-rescan-buffer))
    ))

(defun add-file-to-buildsystem ()
  "Add the current (C++) file to either Makefile.am or a .pro file, whichever exists."
  ; Author: David
  (interactive)
  (if (file-readable-p "Makefile.am")
      (add-file-to-makefile-am)
    ; else: find a .pro file and add it there
    (let* ((files (directory-files "." nil ".pro$" nil t))
	   (projfile (car files)))
      (if projfile
	  (add-file-to-project projfile "^SOURCES[ \t]*") ; could be SOURCES= or SOURCES+=
	; else: error
	(error "No build system file found")
	)))
  )

; internal helper for add-file-to-*
(defun add-file-to-project (makefile searchString)
  (let ((file (buffer-name)))
    (if (not (file-readable-p makefile))
	(error (concat makefile " not found!"))
      )
    (find-file makefile)
    (goto-char (point-min))
    (if (re-search-forward searchString nil t)
	(progn
	  (end-of-line)
          ; check if line ends with '\' [had to read make-mode.el to find this one!]
	  (while (= (char-before) ?\\)
	    (end-of-line 2)) ; moves to end of next line
	  (insert " ")
	  (insert file)
	  )
      (error (concat searchString " not found"))
      ))
  )

(defun add-file-to-makefile-am ()
  "Add the current file to the first _SOURCES line in the Makefile.am"
  ; Author: David
  (interactive)
  (add-file-to-project "Makefile.am" "_SOURCES")
  )


; Inserts a kDebug statement showing the name of the current method.
; You need to create the empty line first.
(defun insert-kDebug ()
  (interactive)
  (if (and (boundp 'kdab-qt-version) (eq kdab-qt-version 4))
      (insert "kDebug() << ")
    (insert "kdDebug() << k_funcinfo "))
  (insert "<< endl;")
  (backward-char 8)
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
      (let ((n nil) (except nil))
        (save-excursion
          (setq n (or (progn (forward-char -2) (looking-at "\\bif"))
                      (progn (forward-char -1) (looking-at "\\bfor"))
                      (progn (forward-char -1) (looking-at "\\bcase"))
                      (progn (forward-char -1) (looking-at "\\bwhile"))
                      )
                )
          (setq except (or (progn (forward-char -2) (looking-at "kDebug"))
                           (looking-at "kError")
                           (progn (forward-char -2) (looking-at "kWarning"))
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
          (cond ((not except) (insert kde-emacs-after-parent-string)))
          )))
    (self-insert-command (prefix-numeric-value arg)))
  )

(defun insert-parens2 (arg) (interactive "*P")
  (if (not (c-in-literal))
      (let ((remv nil) (nospac nil))
        (forward-char -2)
        (setq remv (looking-at "( ")) ; () -> we'll have to remove that space
        (forward-char 1)
        (setq nospac ; no space to be added
              (or (looking-at " ")
                  (looking-at "(")
                  (save-excursion ; check for kDebug(123
                    (while (looking-at "[0-9]")
                      (forward-char -1))
                    (forward-char -7)
                    (or (looking-at "kDebug(")
			(looking-at "kError(")
                           (progn (forward-char -2) (looking-at "kWarning("))
			   )
		    )
                  )
              )
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
	    (spacep nil) (c nil)
	    (oneliner nil))
        (save-excursion
	  (save-excursion
	    (if (re-search-forward "[a-zA-Z]" (point-at-eol) t)
		(setq oneliner t)))
	  (forward-char -1)              ; These three lines are for the situation where
          (if (not (looking-at " "))     ; the user already have inserted a space after
              (forward-char 1)           ; the closing parenthesis
	    (setq spacep t))
          (forward-char -2)
          (setq o (looking-at "()"))
          (forward-char 1)
          (setq n (looking-at ")"))
	  (if (and 
	       (not oneliner)
	       (not (eq
		    (count-lines (point-min) (point))
		    (count-lines (point-min) (point-max)))))
	      (progn
		(next-line 1)
		(beginning-of-line)
		(if (re-search-forward "[a-zA-Z]" (point-at-eol) t)
		    (setq c (eq (car (car (c-guess-basic-syntax))) 'substatement)))
		)
	    )
          )
        (cond
         (n (progn
              (if (not spacep) (insert " "))
              (self-insert-command (prefix-numeric-value arg))
              (if (not c) (newline-and-indent))
	      (if oneliner (end-of-line))
             (save-excursion
	      (if c
		  (progn
		    (next-line 1)
		    (end-of-line)
		    ))
	      (newline-and-indent)
              (insert "}")(c-indent-line))
              (c-indent-line)
	     ))
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
  (compile (concat kde-emacs-make " clean"))
  )

(defun make ()
  "Executes a \"make\" in the current directory"
  (interactive)
  (compile (concat kde-emacs-make " -k"))
  )

(defun makeinstall ()
  "Executes a \"make install\" in the current directory"
  (interactive)
  (compile (concat kde-emacs-make " -k install"))
  )

(defun makeinstallexec ()
  "Executes a \"make install-exec\" in the current directory"
  (interactive)
  (compile (concat kde-emacs-make " -k install-exec"))
  )

(defun makethisfile ()
  "Try to compile the currently opened file"
  (interactive)
  (let ((f (file-name-nondirectory (buffer-file-name)))
	(objext nil))

    (if (and (file-readable-p "Makefile.am") (not (file-readable-p "CMakeLists.txt")))
	(setq objext "\.lo")
      (setq objext "\.o"))
    (if (string-match "\.cpp$" f) (setq f (replace-match objext t t f)))
    (if (string-match "\.cc$" f) (setq f (replace-match objext t t f)))
    (compile (concat kde-emacs-make " " f)))
  )

;; pc-like textmarking
(when kde-use-pc-select
  (progn
    (load "pc-select")
    (if (eq kde-emacs-type 'xemacs)
       (funcall 'pc-select-mode)
      (funcall 'pc-selection-mode))))


; Move in other window
(defun scroll-other-up () (interactive) (scroll-other-window-down 1)) ; hehe :)
(defun scroll-other-down () (interactive) (scroll-other-window 1))

(defun match-paren ()
  "Go to the matching parenthesis if on parenthesis otherwise do nothing."
  (interactive)
  (cond ((looking-at "[ \t]*[\({]") (forward-sexp) (backward-char))
	((looking-at "[\)}]") (forward-char) (backward-sexp))))

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
      (if (re-search-forward (concat "Copyright ([Cc]) \\([0-9 ,-]*\\) " (regexp-quote kde-full-name)) nil t)
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

; Helper for qt-open-header, for Qt 4. Opens a file if it says #include "../foo/bar.h",
; close it and open that file instead; recursively until finding a real file.
(defun qt-follow-includes (file)
  (let ((line "")
	(begin nil)
	(buffer nil))
    (find-file file)
    (goto-char 0)
    (if (looking-at "#include \"")
	(progn
	  (forward-char 10)
	  (setq begin (point))
	  (re-search-forward "\"" nil t)
	  (backward-char 1)
	  (setq file (buffer-substring begin (point)))
	  (setq buffer (current-buffer))
	  (qt-follow-includes file)
	  (kill-buffer buffer)
	  )
      ; else: this is the right file, skip the comments and go to the class
      (progn
	(re-search-forward "^class" nil t)
	(beginning-of-line))
    )))

(defun qt-open-header ()
  "Open the Qt header file for the class under point"
  (interactive)
  (let* ((qtinc (concat (getenv "QTDIR") "/include/"))
	(class (thing-at-point 'word))
	(f nil)
	(file nil)
	(files nil)
	)
    (save-excursion
      ; The Qt3 case: the includes are directly in $QTDIR/include/, lowercased
      (setq f (concat qtinc (downcase class) ".h" ))
      (if (file-readable-p f)
	  (setq file f)
        ; For some Qt3/e classes: add _qws
	(setq f (concat qtinc (downcase class) "_qws.h" ))
	(if (file-readable-p f)
	    (setq file f)
        ; The Qt4 case: the includes are in $QTDIR/include/QSomething/, in original case
	  (setq files (directory-files qtinc t nil "dirsonly"))
	  (dolist (f files nil)
	    (if (file-readable-p (concat f "/" class) )
		(setq file (concat f "/" class))))
	  ))
      (and file
	   (qt-follow-includes file))
      )
  ))

(provide 'kde-emacs-utils)
