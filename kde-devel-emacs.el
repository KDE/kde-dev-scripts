;; -*- emacs-lisp -*-

; To use this file, add this to your .emacs, uncommented :
;(load "cc-engine.elc")
;(load "~/kde2/kdesdk/scripts/kde-devel-emacs.el")
; (setq auto-mode-alist
;          (append '(("\\.h$"    . c++-mode)) auto-mode-alist))


; Tip: also add (gnuserv-start), to be able to use gnuclient to open new files from a shell

; Add (setq magic-keys-mode t) to your .xemacs/init.el or ~/.emacs (before loading this file)
; to enable the magic keys in C++ mode (auto-insertion of spaces and newlines).

; See the end of this file for the list of key bindings and for customizing them

; This file is maintained by David Faure <faure@kde.org>


; Global variables used to differentiate between different emacs
; versions :
; emacs - t if GNU/Emacs is used
; xemacs - t if XEmacs is being used

(if (string= (substring (emacs-version) 0 6) "XEmacs")
    (progn
      (setq emacs nil)
      (setq xemacs t))
  (progn
    (setq emacs t)
    (setq xemacs nil)))


;; -------  First part, from Arnt's "c++ stuff"

(defun agulbra-c++-tab (arg)
  "Do the right thing about tabs in c++ mode"
  (interactive "*P")
  (cond
   ((and (not (looking-at "[A-Za-z0-9]"))
         (save-excursion 
           (forward-char -1) 
           (looking-at "[A-Za-z0-9:>_\\-\\&\\.(){}\\*\\+/]")))
         (dabbrev-expand arg))
   (t 
    (save-excursion
     (beginning-of-line)
     (c-indent-command)))))

(defun agulbra-clean-out-spaces ()
  "Remove spaces at ends of lines"
  (interactive)
  (and (not buffer-read-only)
       (save-excursion
         (goto-char (point-min))
         (let ((count 0)
               (bmp (buffer-modified-p)))
           (while (re-search-forward "[ \t]+$" nil t)
             (setq count (1+ count))
             (replace-match "" t t))
           (set-buffer-modified-p bmp)
           (and (buffer-modified-p)
                  (basic-save-buffer))))))

; the above used to contain (untabify (point-min) (point-max)) too

;; it seems that recursion in agulbra-clean-out-spaces trashes Gnu/Emacs stack
;; one of the functions in there has to behave differently than its XEmacs
;; counterpart does, if you're reading this in a middle of may 2002 then
;; please email me (Zack) at zackrat@att.net and bug me to finally fix this
(defun agulbra-c++-clean-out-spaces ()
  "Remove spaces at ends of lines, only in c++ mode"
  (interactive)
  (and (eq major-mode 'c++-mode)
       (if xemacs 
	   (agulbra-clean-out-spaces)
  )))

(add-hook 'find-file-hooks 'agulbra-c++-clean-out-spaces)
(add-hook 'write-file-hooks 'agulbra-c++-clean-out-spaces)

(defun agulbra-delete-into-nomenclature (&optional arg)
  "Delete forward until the end of a nomenclature section or word.
With arg, to it arg times."
  (interactive "p")
  (save-excursion
    (let ((b (point-marker)))
      (c-forward-into-nomenclature arg)
      (delete-region b (point-marker)))))


(setq c++-mode-hook
      (lambda ()
         (font-lock-mode)
         (c-set-style "stroustrup")
         (setq c-tab-always-indent nil
	       insert-tab-mode nil
	       indent-tabs-mode nil
               fume-auto-rescan-buffer-p nil
               c-basic-offset 4
               c-access-key "\\<\\(signals\\|k_dcop\\|\\(public\\|protected\\|private\\)\\([     ]+slots\\)?\\)\\>:"
               c-hanging-comment-ender-p nil
               c-offsets-alist (append '((case-label   . 0)
                                         (access-label . -)
                                         (label        . 0)
                                         (statement-cont . c-lineup-math)
                                         ) c-offsets-alist))
         (cond ((string-match "^\\(.*/qt/src\\)/.*/" buffer-file-truename)
                (progn
                  (make-local-variable 'compile-command)
                  (setq compile-command
                        (concat "make -k -j 3 -C "
                                (substring buffer-file-truename
                                        (match-beginning 1) (match-end 1))
                                " GNUmakefile.debug && make -k -j 3 -C "
                                (substring buffer-file-truename
                                        (match-beginning 1) (match-end 1))
                                " -f GNUmakefile.debug"))))
               ((string-match "^\\\(.*/2x/src\\\)/.*/" buffer-file-truename)
                (progn
                  (make-local-variable 'compile-command)
                  (setq compile-command
                        (concat "make -k -C "
                                (substring buffer-file-truename
                                           (match-beginning 1)
                                           (match-end 1)))))))
         (define-key c++-mode-map "\C-m" 'newline-and-indent)
         (define-key c++-mode-map "\C-i" 'agulbra-c++-tab)
         (define-key c++-mode-map "\ef" 'c-forward-into-nomenclature)
         (define-key c++-mode-map "\ed" 'agulbra-delete-into-nomenclature)
         (define-key c++-mode-map "\eb" 'c-backward-into-nomenclature)

         ; Add (setq magic-keys-mode t) to your .emacs (before loading this file)
         ; to enable the magic keys in C++ mode.
	 (and (boundp 'magic-keys-mode)
	      (progn
		(define-key c++-mode-map [\(] 'insert-parens)
		(define-key c++-mode-map [\)] 'insert-parens2)
		(define-key c++-mode-map [,] 'insert-comma)
		(define-key c++-mode-map [\{] 'insert-curly-brace)
		))
))

(setq c-mode-hook
      (lambda ()
         (font-lock-mode)
         (setq c-tab-always-indent nil
               c-basic-offset 4
               c-offsets-alist (append '((case-label   . 4)
                                         (access-label . -)
                                         (label        . 0)
                                         (statement-cont . c-lineup-math)
                                         ) c-offsets-alist))))


(defun agulbra-make-member ()
  "make a skeleton member function in the .cpp or .cc file"
  (interactive)
  (let ((class nil)
        (function nil)
        (file (buffer-file-name))
        (insertion-string nil)
        (start nil))
    (save-excursion
      (and (re-search-backward "^class[ \t]" nil t)
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
               (message "")
           (progn
              (string-match "\\.h$" file)
              (setq f (replace-match ".cc" t t file))
              ))
         (find-file f)
         (progn
           (goto-char (point-max))
           (insert insertion-string)
           (forward-char -3)
           (save-excursion
             (and (string-match ".*/" file)
                  (setq file (replace-match "" t nil file)))
             (or (re-search-backward
                  (concat "^#include *\"" file "\"$") nil t)
                 (progn
                   (goto-char (point-min))
                   (re-search-forward "^$" nil t)
                   (insert "\n#include \"" file "\"\n")))))))
  (fume-rescan-buffer)
)


(setq compilation-error-regexp-systems-list '(gnu of comma 4bsd)
      compilation-ask-about-save nil)


(defun c-guess-basic-syntax ()
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let* ((indent-point (point))
	     (case-fold-search nil)
	     (fullstate (c-parse-state))
	     (state fullstate)
	     literal containing-sexp char-before-ip char-after-ip lim
	     syntax placeholder c-in-literal-cache inswitch-p
	     tmpsymbol keyword injava-inher special-brace-list
	     ;; narrow out any enclosing class or extern "C" block
	     (inclass-p (c-narrow-out-enclosing-class state indent-point))
	     inenclosing-p)
	;; check for meta top-level enclosing constructs, possible
	;; extern language definitions, possibly (in C++) namespace
	;; definitions.
	(save-excursion
	  (save-restriction
	    (widen)
	    (if (and inclass-p
		     (progn
		       (goto-char (aref inclass-p 0))
		       (looking-at (concat c-extra-toplevel-key "[^_]"))))
		(let ((enclosing (match-string 1)))
		  (cond
		   ((string-equal enclosing "extern")
		    (setq inenclosing-p 'extern))
		   ((string-equal enclosing "namespace")
		    (setq inenclosing-p 'namespace))
		   )))))
	;; get the buffer position of the most nested opening brace,
	;; if there is one, and it hasn't been narrowed out
	(save-excursion
	  (goto-char indent-point)
	  (skip-chars-forward " \t}")
	  (skip-chars-backward " \t")
	  (while (and state
		      (not containing-sexp))
	    (setq containing-sexp (car state)
		  state (cdr state))
	    (if (consp containing-sexp)
		;; if cdr == point, then containing sexp is the brace
		;; that opens the sexp we close
		(if (= (cdr containing-sexp) (point))
		    (setq containing-sexp (car containing-sexp))
		  ;; otherwise, ignore this element
		  (setq containing-sexp nil))
	      ;; ignore the bufpos if its been narrowed out by the
	      ;; containing class or does not contain the indent point
	      (if (or (<= containing-sexp (point-min))
		      (>= containing-sexp indent-point))
		  (setq containing-sexp nil)))))

	;; set the limit on the farthest back we need to search
	(setq lim (or containing-sexp
		      (if (consp (car fullstate))
			  (cdr (car fullstate))
			nil)
		      (point-min)))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (char-after))
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (char-before))
	(goto-char indent-point)
	(skip-chars-forward " \t")

	;; are we in a literal?
	(setq literal (c-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((memq literal '(string))
	  (c-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((memq literal '(c c++))
	  (c-add-syntax literal (car (c-literal-limits lim))))
	 ;; CASE 3: in a cpp preprocessor macro continuation.
	 ((and (eq literal 'pound)
	       (/= (save-excursion
		     (c-beginning-of-macro lim)
		     (setq placeholder (point)))
		   (c-point 'boi)))
	  (c-add-syntax 'cpp-macro-cont placeholder))
	 ;; CASE 4: In-expression statement.
	 ((and (or c-inexpr-class-key c-inexpr-block-key c-lambda-key)
	       (setq placeholder (c-looking-at-inexpr-block)))
	  (setq tmpsymbol (assq (car placeholder)
				'((inexpr-class . class-open)
				  (inexpr-statement . block-open))))
	  (if tmpsymbol
	      ;; It's a statement block or an anonymous class.
	      (setq tmpsymbol (cdr tmpsymbol))
	    ;; It's a Pike lambda.  Check whether we are between the
	    ;; lambda keyword and the argument list or at the defun
	    ;; opener.
	    (setq tmpsymbol (if (eq char-after-ip ?{)
				'inline-open
			      'lambda-intro-cont)))
	  (goto-char (cdr placeholder))
	  (back-to-indentation)
	  (c-add-syntax tmpsymbol (point))
	  (unless (eq (point) (cdr placeholder))
	    (c-add-syntax (car placeholder))))
	 ;; CASE 5: Line is at top level.
	 ((null containing-sexp)
	  (cond
	   ;; CASE 5A: we are looking at a defun, brace list, class,
	   ;; or inline-inclass method opening brace
	   ((setq special-brace-list
		  (or (and c-special-brace-lists
			   (c-looking-at-special-brace-list))
		      (eq char-after-ip ?{)))
	    (cond
	     ;; CASE 5A.1: extern language or namespace construct
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t")
		(and (c-safe (progn (c-backward-sexp 2) t))
		     (looking-at (concat c-extra-toplevel-key "[^_]"))
		     (setq keyword (match-string 1)
			   placeholder (point))
		     (or (and (string-equal keyword "namespace")
			      (setq tmpsymbol 'namespace-open))
			 (and (string-equal keyword "extern")
			      (progn
				(c-forward-sexp 1)
				(c-forward-syntactic-ws)
				(eq (char-after) ?\"))
			      (setq tmpsymbol 'extern-lang-open)))
		     ))
	      (goto-char placeholder)
	      (c-add-syntax tmpsymbol (c-point 'boi)))
	     ;; CASE 5A.2: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		;; TBD: watch out! there could be a bogus
		;; c-state-cache in place when we get here.  we have
		;; to go through much chicanery to ignore the cache.
		;; But of course, there may not be!  BLECH!  BOGUS!
		(let ((decl
		       (let ((c-state-cache nil))
			 (c-search-uplist-for-classkey (c-parse-state))
			 )))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.3: brace list open
	     ((save-excursion
		(c-beginning-of-statement-1 lim)
		;; c-b-o-s could have left us at point-min
		(and (bobp)
		     (c-forward-syntactic-ws indent-point))
		(if (looking-at "typedef[^_]")
		    (progn (c-forward-sexp 1)
			   (c-forward-syntactic-ws indent-point)))
		(setq placeholder (c-point 'boi))
		(or (consp special-brace-list)
		    (and (or (save-excursion
			       (goto-char indent-point)
			       (setq tmpsymbol nil)
			       (while (and (> (point) placeholder)
					   (= (c-backward-token-1 1 t) 0)
					   (/= (char-after) ?=))
				 (if (and (not tmpsymbol)
					  (looking-at "new\\>[^_]"))
				     (setq tmpsymbol 'topmost-intro-cont)))
			       (eq (char-after) ?=))
			     (looking-at "enum[ \t\n]+"))
			 (save-excursion
			   (while (and (< (point) indent-point)
				       (= (c-forward-token-1 1 t) 0)
				       (not (memq (char-after) '(?\; ?\()))))
			   (not (memq (char-after) '(?\; ?\()))
			   ))))
	      (if (and (c-major-mode-is 'java-mode)
		       (eq tmpsymbol 'topmost-intro-cont))
		  ;; We're in Java and have found that the open brace
		  ;; belongs to a "new Foo[]" initialization list,
		  ;; which means the brace list is part of an
		  ;; expression and not a top level definition.  We
		  ;; therefore treat it as any topmost continuation
		  ;; even though the semantically correct symbol still
		  ;; is brace-list-open, on the same grounds as in
		  ;; case 10B.2.
		  (progn
		    (c-beginning-of-statement-1 lim)
		    (c-forward-syntactic-ws)
		    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
		(c-add-syntax 'brace-list-open placeholder)))
	     ;; CASE 5A.4: inline defun open
	     ((and inclass-p (not inenclosing-p))
	      (c-add-syntax 'inline-open)
	      (c-add-class-syntax 'inclass inclass-p))
	     ;; CASE 5A.5: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (if inclass-p
		  (c-add-syntax 'defun-open (c-point 'boi))
		(c-add-syntax 'defun-open (c-point 'bol)))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p)
	    (cond
	     ;; CASE 5B.1: a member init
	     ((or (eq char-before-ip ?:)
		  (eq char-after-ip ?:))
	      ;; this line should be indented relative to the beginning
	      ;; of indentation for the topmost-intro line that contains
	      ;; the prototype's open paren
	      ;; TBD: is the following redundant?
	      (if (eq char-before-ip ?:)
		  (forward-char -1))
	      (c-backward-syntactic-ws lim)
	      ;; TBD: is the preceding redundant?
	      (if (eq (char-before) ?:)
		  (progn (forward-char -1)
			 (c-backward-syntactic-ws lim)))
	      (if (eq (char-before) ?\))
		  (c-backward-sexp 1))
	      (setq placeholder (point))
	      (save-excursion
		(and (c-safe (c-backward-sexp 1) t)
		     (looking-at "throw[^_]")
		     (c-safe (c-backward-sexp 1) t)
		     (setq placeholder (point))))
	      (goto-char placeholder)
	      (c-add-syntax 'member-init-intro (c-point 'boi))
	      ;; we don't need to add any class offset since this
	      ;; should be relative to the ctor's indentation
	      )
	     ;; CASE 5B.2: K&R arg decl intro
	     (c-recognize-knr-p
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (if inclass-p (c-add-class-syntax 'inclass inclass-p)))
	     ;; CASE 5B.3: Inside a member init list.
	     ((c-beginning-of-member-init-list lim)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5B.4: Nether region after a C++ or Java func
	     ;; decl, which could include a `throws' declaration.
	     (t
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'func-decl-cont (c-point 'boi))
	      )))
	   ;; CASE 5C: inheritance line. could be first inheritance
	   ;; line, or continuation of a multiple inheritance
	   ((or (and c-baseclass-key
		     (progn
		       (when (eq char-after-ip ?,)
			 (skip-chars-forward " \t")
			 (forward-char))
		       (looking-at c-baseclass-key)))
		(and (or (eq char-before-ip ?:)
			 ;; watch out for scope operator
			 (save-excursion
			   (and (eq char-after-ip ?:)
				(c-safe (progn (forward-char 1) t))
				(not (eq (char-after) ?:))
				)))
		     (save-excursion
		       (c-backward-syntactic-ws lim)
		       (if (eq char-before-ip ?:)
			   (progn
			     (forward-char -1)
			     (c-backward-syntactic-ws lim)))
		       (back-to-indentation)
		       (looking-at c-class-key)))
		;; for Java
		(and (c-major-mode-is 'java-mode)
		     (let ((fence (save-excursion
				    (c-beginning-of-statement-1 lim)
				    (point)))
			   cont done)
		       (save-excursion
			 (while (not done)
			   (cond ((looking-at c-Java-special-key)
				  (setq injava-inher (cons cont (point))
					done t))
				 ((or (not (c-safe (c-forward-sexp -1) t))
				      (<= (point) fence))
				  (setq done t))
				 )
			   (setq cont t)))
		       injava-inher)
		     (not (c-crosses-statement-barrier-p (cdr injava-inher)
							 (point)))
		     ))
	    (cond
	     ;; CASE 5C.1: non-hanging colon on an inher intro
	     ((eq char-after-ip ?:)
	      (c-backward-syntactic-ws lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((eq char-before-ip ?:)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (if inclass-p (c-add-class-syntax 'inclass inclass-p)))
             ;; CASE agulbrahack.1:
             ((and inclass-p
                   c-access-key
                   (looking-at c-access-key))
              (c-add-syntax 'access-label (c-point 'bonl))
              )
	     ;; CASE 5C.3: in a Java implements/extends
	     (injava-inher
	      (let ((where (cdr injava-inher))
		    (cont (car injava-inher)))
		(goto-char where)
		(cond ((looking-at "throws[ \t\n]")
		       (c-add-syntax 'func-decl-cont
				     (progn (c-beginning-of-statement-1 lim)
					    (c-point 'boi))))
		      (cont (c-add-syntax 'inher-cont where))
		      (t (c-add-syntax 'inher-intro
				       (progn (goto-char (cdr injava-inher))
					      (c-beginning-of-statement-1 lim)
					      (point))))
		      )))
	     ;; CASE 5C.4: a continued inheritance line
	     (t
	      (c-beginning-of-inheritance-list lim)
	      (c-add-syntax 'inher-cont (point))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )))
	   ;; CASE 5D: this could be a top-level compound statement, a
	   ;; member init list continuation, or a template argument
	   ;; list continuation.
	   ((c-with-syntax-table (if (c-major-mode-is 'c++-mode)
				     c++-template-syntax-table
				   (syntax-table))
	      (save-excursion
		(while (and (= (c-backward-token-1 1 t lim) 0)
			    (not (looking-at "[;{<,]"))))
		(eq (char-after) ?,)))
	    (goto-char indent-point)
	    (c-beginning-of-member-init-list lim)
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and (save-excursion
		     (setq placeholder (point))
		     (c-backward-token-1 1 t lim)
		     (and (eq (char-after) ?:)
			  (not (eq (char-before) ?:))))
		   (save-excursion
		     (goto-char placeholder)
		     (back-to-indentation)
		     (or
		      (/= (car (save-excursion
				 (parse-partial-sexp (point) placeholder)))
			  0)
		      (and
		       (if c-access-key (not (looking-at c-access-key)) t)
		       (not (looking-at c-class-key))
		       (if c-bitfield-key (not (looking-at c-bitfield-key)) t))
		      )))
	      (goto-char placeholder)
	      (c-forward-syntactic-ws)
	      (c-add-syntax 'member-init-cont (point))
	      ;; we do not need to add class offset since relative
	      ;; point is the member init above us
	      )
	     ;; CASE 5D.2: non-hanging member init colon
	     ((progn
		(c-forward-syntactic-ws indent-point)
		(eq (char-after) ?:))
	      (skip-chars-forward " \t:")
	      (c-add-syntax 'member-init-cont (point)))
	     ;; CASE 5D.3: perhaps a multiple inheritance line?
	     ((save-excursion
		(c-beginning-of-statement-1 lim)
		(setq placeholder (point))
		(looking-at c-inher-key))
	      (goto-char placeholder)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a template list continuation?
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-backward "^<" lim)
		;; not sure if this is the right test, but it should
		;; be fast and mostly accurate.
		(setq placeholder (point))
		(and (eq (char-before) ?<)
		     (not (c-in-literal lim))))
	      ;; we can probably indent it just like an arglist-cont
	      (goto-char placeholder)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'template-args-cont (c-point 'boi)))
	     ;; CASE 5D.5: perhaps a top-level statement-cont
	     (t
	      (c-beginning-of-statement-1 lim)
	      ;; skip over any access-specifiers
	      (and inclass-p c-access-key
		   (while (looking-at c-access-key)
		     (forward-line 1)))
	      ;; skip over comments, whitespace
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'statement-cont (c-point 'boi)))
	     ))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-access-key
		 (looking-at c-access-key))
	    (c-add-syntax 'access-label (c-point 'bonl))
	    (c-add-class-syntax 'inclass inclass-p))
	   ;; CASE 5F: extern-lang-close or namespace-close?
	   ((and inenclosing-p
		 (eq char-after-ip ?}))
	    (setq tmpsymbol (if (eq inenclosing-p 'extern)
				'extern-lang-close
			      'namespace-close))
	    (c-add-syntax tmpsymbol (aref inclass-p 0)))
	   ;; CASE 5G: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (eq char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and (c-safe (progn (c-backward-sexp 1) t))
			  (= (point) (aref inclass-p 1))
			  ))))
	    (c-add-class-syntax 'class-close inclass-p))
	   ;; CASE 5H: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 ;; here we essentially use the hack that is used in
		 ;; Emacs' c-mode.el to limit how far back we should
		 ;; look.  The assumption is made that argdecls are
		 ;; indented at least one space and that function
		 ;; headers are not indented.
		 (let ((limit (save-excursion
				(re-search-backward "^[^ \^L\t\n#]" nil 'move)
				(point))))
		   (save-excursion
		     (c-backward-syntactic-ws limit)
		     (setq placeholder (point))
		     (while (and (memq (char-before) '(?\; ?,))
				 (> (point) limit))
		       (beginning-of-line)
		       (setq placeholder (point))
		       (c-backward-syntactic-ws limit))
		     (and (eq (char-before) ?\))
			  (or (not c-method-key)
			      (progn
				(c-forward-sexp -1)
				(forward-char -1)
				(c-backward-syntactic-ws)
				(not (or (memq (char-before) '(?- ?+))
					 ;; or a class category
					 (progn
					   (c-forward-sexp -2)
					   (looking-at c-class-key))
					 )))))
		     ))
		 (save-excursion
		   (c-beginning-of-statement-1)
		   (not (looking-at "typedef[ \t\n]+"))))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (c-point 'boi)))
	   ;; CASE 5I: ObjC method definition.
	   ((and c-method-key
		 (looking-at c-method-key))
	    (c-add-syntax 'objc-method-intro (c-point 'boi)))
	   ;; CASE 5J: we are at the topmost level, make sure we skip
	   ;; back past any access specifiers
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (while (and inclass-p
			  c-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (progn (c-backward-sexp 1) t))
			    ;; agulbrahack 2
                            (and (looking-at "slots:")
                                 (c-backward-sexp 1))
                            (looking-at c-access-key)))
		(c-backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
		  (memq (char-before) '(?\; ?\}))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol))
	      (if inclass-p
		  (progn
		    (goto-char (aref inclass-p 1))
		    (or (= (point) (c-point 'boi))
			(goto-char (aref inclass-p 0)))
		    (cond
		     ((eq inenclosing-p 'extern)
		      (c-add-syntax 'inextern-lang (c-point 'boi)))
		     ((eq inenclosing-p 'namespace)
		      (c-add-syntax 'innamespace (c-point 'boi)))
		     (t (c-add-class-syntax 'inclass inclass-p)))
		    ))
	      ))
	   ;; CASE 5K: we are at an ObjC or Java method definition
	   ;; continuation line.
	   ((and c-method-key
		 (progn
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (looking-at c-method-key)))
	    (c-add-syntax 'objc-method-args-cont (point)))
	   ;; CASE 5L: we are at the first argument of a template
	   ;; arglist that begins on the previous line.
	   ((eq (char-before) ?<)
	    (c-beginning-of-statement-1 lim)
	    (c-forward-syntactic-ws)
	    (c-add-syntax 'template-args-cont (c-point 'boi)))
	   ;; CASE 5M: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 lim)
	    (c-forward-syntactic-ws)
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))				; end CASE 5
	 ;; (CASE 6 has been removed.)
	 ;; CASE 7: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((not (or (and c-special-brace-lists
			(save-excursion
			  (goto-char containing-sexp)
			  (c-looking-at-special-brace-list)))
		   (eq (char-after containing-sexp) ?{)))
	  (c-backward-syntactic-ws containing-sexp)
	  (cond
	   ;; CASE 7A: we are looking at the arglist closing paren
	   ((and (or (c-major-mode-is 'pike-mode)
		     ;; Don't check this in Pike since it allows a
		     ;; comma after the last arg.
		     (not (eq char-before-ip ?,)))
		 (memq char-after-ip '(?\) ?\])))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-close placeholder))
	   ;; CASE 7B: Looking at the opening brace of an
	   ;; in-expression block or brace list.
	   ((eq char-after-ip ?{)
	    (goto-char indent-point)
	    (setq placeholder (c-point 'boi))
	    (goto-char containing-sexp)
	    (if (c-inside-bracelist-p placeholder
				      (cons containing-sexp state))
		(progn
		  (c-add-syntax 'brace-list-open (c-point 'boi))
		  (c-add-syntax 'inexpr-class))
	      (c-add-syntax 'block-open (c-point 'boi))
	      (c-add-syntax 'inexpr-statement)))
	   ;; CASE 7C: we are looking at the first argument in an empty
	   ;; argument list. Use arglist-close if we're actually
	   ;; looking at a close paren or bracket.
	   ((memq char-before-ip '(?\( ?\[))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-intro placeholder))
	   ;; CASE 7D: we are inside a conditional test clause. treat
	   ;; these things as statements
	   ((save-excursion
	      (goto-char containing-sexp)
	      (and (c-safe (progn (c-forward-sexp -1) t))
		   (looking-at "\\<for\\>[^_]")))
	    (goto-char (1+ containing-sexp))
	    (c-forward-syntactic-ws indent-point)
	    (c-beginning-of-statement-1 containing-sexp)
	    (if (eq char-before-ip ?\;)
		(c-add-syntax 'statement (point))
	      (c-add-syntax 'statement-cont (point))
	      ))
	   ;; CASE 7E: maybe a continued method call. This is the case
	   ;; when we are inside a [] bracketed exp, and what precede
	   ;; the opening bracket is not an identifier.
	   ((and c-method-key
		 (eq (char-after containing-sexp) ?\[)
		 (save-excursion
		   (goto-char (1- containing-sexp))
		   (c-backward-syntactic-ws (c-point 'bod))
		   (if (not (looking-at c-symbol-key))
		       (c-add-syntax 'objc-method-call-cont containing-sexp))
		   )))
	   ;; CASE 7F: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line
	   ((save-excursion
	      (goto-char (1+ containing-sexp))
	      (skip-chars-forward " \t")
	      (not (eolp)))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (when (and (c-safe (backward-up-list 1) t)
		       (> (point) placeholder))
	      (forward-char)
	      (skip-chars-forward " \t")
	      (setq placeholder (point)))
	    (c-add-syntax 'arglist-cont-nonempty placeholder))
	   ;; CASE 7G: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-beginning-of-statement-1 containing-sexp)
	      (forward-char 1)
	      (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 8: func-local multi-inheritance line
	 ((and c-baseclass-key
	       (save-excursion
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (looking-at c-baseclass-key)))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (cond
	   ;; CASE 8A: non-hanging colon on an inher intro
	   ((eq char-after-ip ?:)
	    (c-backward-syntactic-ws lim)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8B: hanging colon on an inher intro
	   ((eq char-before-ip ?:)
	    (c-add-syntax 'inher-intro (c-point 'boi)))
	   ;; CASE 8C: a continued inheritance line
	   (t
	    (c-beginning-of-inheritance-list lim)
	    (c-add-syntax 'inher-cont (point))
	    )))
	 ;; CASE 9: we are inside a brace-list
	 ((setq special-brace-list
		(or (and c-special-brace-lists
			 (save-excursion
			   (goto-char containing-sexp)
			   (c-looking-at-special-brace-list)))
		    (c-inside-bracelist-p containing-sexp state)))
	  (cond
	   ;; CASE 9A: In the middle of a special brace list opener.
	   ((and (consp special-brace-list)
		 (save-excursion
		   (goto-char containing-sexp)
		   (eq (char-after) ?\())
		 (eq char-after-ip (car (cdr special-brace-list))))
	    (goto-char (car (car special-brace-list)))
	    (skip-chars-backward " \t")
	    (if (and (bolp)
		     (assoc 'statement-cont
			    (setq placeholder (c-guess-basic-syntax))))
		(setq syntax placeholder)
	      (c-beginning-of-statement-1 lim)
	      (c-forward-token-1 0)
	      (if (looking-at "typedef\\>") (c-forward-token-1 1))
	      (c-add-syntax 'brace-list-open (c-point 'boi))))
	   ;; CASE 9B: brace-list-close brace
	   ((if (consp special-brace-list)
		;; Check special brace list closer.
		(progn
		  (goto-char (car (car special-brace-list)))
		  (save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (or
		     ;; We were between the special close char and the `)'.
		     (and (eq (char-after) ?\))
			  (eq (1+ (point)) (cdr (car special-brace-list))))
		     ;; We were before the special close char.
		     (and (eq (char-after) (cdr (cdr special-brace-list)))
			  (= (c-forward-token-1) 0)
			  (eq (1+ (point)) (cdr (car special-brace-list)))))))
	      ;; Normal brace list check.
	      (and (eq char-after-ip ?})
		   (c-safe (progn (forward-char 1)
				  (c-backward-sexp 1)
				  t))
		   (= (point) containing-sexp)))
	    (c-add-syntax 'brace-list-close (c-point 'boi)))
	   (t
	    ;; Prepare for the rest of the cases below by going to the
	    ;; token following the opening brace
	    (if (consp special-brace-list)
		(progn
		  (goto-char (car (car special-brace-list)))
		  (c-forward-token-1 1 nil indent-point))
	      (goto-char containing-sexp))
	    (forward-char)
	    (let ((start (point)))
	      (c-forward-syntactic-ws indent-point)
	      (goto-char (max start (c-point 'bol))))
	    (skip-chars-forward " \t\n\r" indent-point)
	    (cond
	     ;; CASE 9C: we're looking at the first line in a brace-list
	     ((= (point) indent-point)
	      (goto-char containing-sexp)
	      (c-add-syntax 'brace-list-intro (c-point 'boi))
	      )				; end CASE 9C
	     ;; CASE 9D: this is just a later brace-list-entry or
	     ;; brace-entry-open
	     (t (if (or (eq char-after-ip ?{)
			(and c-special-brace-lists
			     (save-excursion
			       (goto-char indent-point)
			       (c-forward-syntactic-ws (c-point 'eol))
			       (c-looking-at-special-brace-list (point)))))
		    (c-add-syntax 'brace-entry-open (point))
		  (c-add-syntax 'brace-list-entry (point))
		  ))			; end CASE 9D
	     ))))			; end CASE 9
	 ;; CASE 10: A continued statement
	 ((and (not (memq char-before-ip '(?\; ?:)))
	       (or (not (eq char-before-ip ?}))
		   (c-looking-at-inexpr-block-backward containing-sexp))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement-1 containing-sexp)
		    (c-forward-syntactic-ws)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  (goto-char indent-point)
	  (skip-chars-forward " \t")
	  (let ((after-cond-placeholder
		 (save-excursion
		   (goto-char placeholder)
		   (if (and c-conditional-key (looking-at c-conditional-key))
		       (progn
			 (c-safe (c-skip-conditional))
			 (c-forward-syntactic-ws)
			 (if (eq (char-after) ?\;)
			     (progn
			       (forward-char 1)
			       (c-forward-syntactic-ws)))
			 (point))
		     nil))))
	    (cond
	     ;; CASE 10A: substatement
	     ((and after-cond-placeholder
		   (>= after-cond-placeholder indent-point))
	      (goto-char placeholder)
	      (if (eq char-after-ip ?{)
		  (c-add-syntax 'substatement-open (c-point 'boi))
		(c-add-syntax 'substatement (c-point 'boi))))
	     ;; CASE 10B: open braces for class or brace-lists
	     ((setq special-brace-list
		    (or (and c-special-brace-lists
			     (c-looking-at-special-brace-list))
			(eq char-after-ip ?{)))
	      (cond
	       ;; CASE 10B.1: class-open
	       ((save-excursion
		  (goto-char indent-point)
		  (skip-chars-forward " \t{")
		  (let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		    (and decl
			 (setq placeholder (aref decl 0)))
		    ))
		(c-add-syntax 'class-open placeholder))
	       ;; CASE 10B.2: brace-list-open
	       ((or (consp special-brace-list)
		    (save-excursion
		      (goto-char placeholder)
		      (looking-at "\\<enum\\>"))
		    (save-excursion
		      (goto-char indent-point)
		      (while (and (> (point) placeholder)
				  (= (c-backward-token-1 1 t) 0)
				  (/= (char-after) ?=)))
		      (eq (char-after) ?=)))
		;; The most semantically accurate symbol here is
		;; brace-list-open, but we report it simply as a
		;; statement-cont.  The reason is that one normally
		;; adjusts brace-list-open for brace lists as
		;; top-level constructs, and brace lists inside
		;; statements is a completely different context.
		(goto-char indent-point)
		(c-beginning-of-closest-statement)
		(c-add-syntax 'statement-cont (c-point 'boi)))
	       ;; CASE 10B.3: The body of a function declared inside a
	       ;; normal block.  This can only occur in Pike.
	       ((and (c-major-mode-is 'pike-mode)
		     (progn
		       (goto-char indent-point)
		       (not (c-looking-at-bos))))
		(c-beginning-of-closest-statement)
		(c-add-syntax 'defun-open (c-point 'boi)))
	       ;; CASE 10B.4: catch-all for unknown construct.
	       (t
		;; Can and should I add an extensibility hook here?
		;; Something like c-recognize-hook so support for
		;; unknown constructs could be added.  It's probably a
		;; losing proposition, so I dunno.
		(goto-char placeholder)
		(c-add-syntax 'statement-cont (c-point 'boi))
		(c-add-syntax 'block-open))
	       ))
	     ;; CASE 10C: iostream insertion or extraction operator
	     ((looking-at "<<\\|>>")
	      (goto-char placeholder)
	      (and after-cond-placeholder
		   (goto-char after-cond-placeholder))
	      (while (and (re-search-forward "<<\\|>>" indent-point 'move)
			  (c-in-literal placeholder)))
	      ;; if we ended up at indent-point, then the first
	      ;; streamop is on a separate line. Indent the line like
	      ;; a statement-cont instead
	      (if (/= (point) indent-point)
		  (c-add-syntax 'stream-op (c-point 'boi))
		(c-backward-syntactic-ws lim)
		(c-add-syntax 'statement-cont (c-point 'boi))))
	     ;; CASE 10D: continued statement. find the accurate
	     ;; beginning of statement or substatement
	     (t
	      (c-beginning-of-statement-1 after-cond-placeholder)
	      ;; KLUDGE ALERT!  c-beginning-of-statement-1 can leave
	      ;; us before the lim we're passing in.  It should be
	      ;; fixed, but I'm worried about side-effects at this
	      ;; late date.  Fix for v5.
	      (goto-char (or (and after-cond-placeholder
				  (max after-cond-placeholder (point)))
			     (point)))
	      (c-add-syntax 'statement-cont (point)))
	     )))
	 ;; CASE 11: an else clause?
	 ((looking-at "\\<else\\>[^_]")
	  (c-backward-to-start-of-if containing-sexp)
	  (c-add-syntax 'else-clause (c-point 'boi)))
	 ;; CASE 12: Statement. But what kind?  Lets see if its a
	 ;; while closure of a do/while construct
	 ((progn
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (and (looking-at "while\\b[^_]")
		 (save-excursion
		   (c-backward-to-start-of-do containing-sexp)
		   (setq placeholder (point))
		   (looking-at "do\\b[^_]"))
		 ))
	  (goto-char placeholder)
	  (c-add-syntax 'do-while-closure (c-point 'boi)))
	 ;; CASE 13: A catch or finally clause?  This case is simpler
	 ;; than if-else and do-while, because a block is required
	 ;; after every try, catch and finally.
	 ((save-excursion
	    (and (cond ((c-major-mode-is 'c++-mode)
			(looking-at "\\<catch\\>[^_]"))
		       ((c-major-mode-is 'java-mode)
			(looking-at "\\<\\(catch\\|finally\\)\\>[^_]")))
		 (c-safe (c-backward-sexp) t)
		 (eq (char-after) ?{)
		 (c-safe (c-backward-sexp) t)
		 (if (eq (char-after) ?\()
		     (c-safe (c-backward-sexp) t)
		   t)
		 (looking-at "\\<\\(try\\|catch\\)\\>[^_]")
		 (setq placeholder (c-point 'boi))))
	  (c-add-syntax 'catch-clause placeholder))
	 ;; CASE 14: A case or default label
	 ((looking-at c-switch-label-key)
	  (goto-char containing-sexp)
	  ;; check for hanging braces
	  (if (/= (point) (c-point 'boi))
	      (c-forward-sexp -1))
	  (c-add-syntax 'case-label (c-point 'boi)))
	 ;; CASE 15: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  ;; check for hanging braces
	  (if (/= (point) (c-point 'boi))
	      (c-forward-sexp -1))
	  (c-add-syntax 'label (c-point 'boi)))
	 ;; CASE 16: block close brace, possibly closing the defun or
	 ;; the class
	 ((eq char-after-ip ?})
	  (let* ((lim (c-safe-position containing-sexp fullstate))
		 (relpos (save-excursion
			   (goto-char containing-sexp)
			   (if (/= (point) (c-point 'boi))
			       (c-beginning-of-statement-1 lim))
			   (c-point 'boi))))
	    (cond
	     ;; CASE 16A: closing a lambda defun or an in-expression
	     ;; block?
	     ((save-excursion
		(goto-char containing-sexp)
		(setq placeholder (c-looking-at-inexpr-block)))
	      (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				  'inline-close
				'block-close))
	      (goto-char containing-sexp)
	      (back-to-indentation)
	      (if (= containing-sexp (point))
		  (c-add-syntax tmpsymbol (point))
		(goto-char (cdr placeholder))
		(back-to-indentation)
		(c-add-syntax tmpsymbol (point))
		(if (/= (point) (cdr placeholder))
		    (c-add-syntax (car placeholder)))))
	     ;; CASE 16B: does this close an inline or a function in
	     ;; an extern block or namespace?
	     ((progn
		(goto-char containing-sexp)
		(setq placeholder (c-search-uplist-for-classkey state)))
	      (goto-char (aref placeholder 0))
	      (if (looking-at (concat c-extra-toplevel-key "[^_]"))
		  (c-add-syntax 'defun-close relpos)
		(c-add-syntax 'inline-close relpos)))
	     ;; CASE 16C: if there an enclosing brace that hasn't
	     ;; been narrowed out by a class, then this is a
	     ;; block-close
	     ((and (not inenclosing-p)
		   (c-most-enclosing-brace state)
		   (or (not (c-major-mode-is 'pike-mode))
		       ;; In Pike it can be a defun-close of a
		       ;; function declared in a statement block.  Let
		       ;; it through to be handled below.
		       (or (c-looking-at-bos)
			   (progn
			     (c-beginning-of-statement-1)
			     (looking-at c-conditional-key)))))
	      (c-add-syntax 'block-close relpos))
	     ;; CASE 16D: find out whether we're closing a top-level
	     ;; class or a defun
	     (t
	      (save-restriction
		(narrow-to-region (point-min) indent-point)
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (if decl
		      (c-add-class-syntax 'class-close decl)
		    (c-add-syntax 'defun-close relpos)))))
	     )))
	 ;; CASE 17: statement catchall
	 (t
	  ;; we know its a statement, but we need to find out if it is
	  ;; the first statement in a block
	  (goto-char containing-sexp)
	  (forward-char 1)
	  (c-forward-syntactic-ws indent-point)
	  ;; now skip forward past any case/default clauses we might find.
	  (while (or (c-skip-case-statement-forward fullstate indent-point)
		     (and (looking-at c-switch-label-key)
			  (not inswitch-p)))
	    (setq inswitch-p t))
	  ;; we want to ignore non-case labels when skipping forward
	  (while (and (looking-at c-label-key)
		      (goto-char (match-end 0)))
	    (c-forward-syntactic-ws indent-point))
	  (cond
	   ;; CASE 17A: we are inside a case/default clause inside a
	   ;; switch statement.  find out if we are at the statement
	   ;; just after the case/default label.
	   ((and inswitch-p
		 (progn
		   (goto-char indent-point)
		   (c-beginning-of-statement-1 containing-sexp)
		   (setq placeholder (point))
		   (beginning-of-line)
		   (when (re-search-forward c-switch-label-key
					    (max placeholder (c-point 'eol)) t)
		     (setq placeholder (match-beginning 0)))))
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (if (eq (char-after) ?{)
		(c-add-syntax 'statement-case-open placeholder)
	      (c-add-syntax 'statement-case-intro placeholder)))
	   ;; CASE 17B: continued statement
	   ((eq char-before-ip ?,)
	    (goto-char indent-point)
	    (c-beginning-of-closest-statement)
	    (c-add-syntax 'statement-cont (c-point 'boi)))
	   ;; CASE 17C: a question/colon construct?  But make sure
	   ;; what came before was not a label, and what comes after
	   ;; is not a globally scoped function call!
	   ((or (and (memq char-before-ip '(?: ??))
		     (save-excursion
		       (goto-char indent-point)
		       (c-backward-syntactic-ws lim)
		       (back-to-indentation)
		       (not (looking-at c-label-key))))
		(and (memq char-after-ip '(?: ??))
		     (save-excursion
		       (goto-char indent-point)
		       (skip-chars-forward " \t")
		       ;; watch out for scope operator
		       (not (looking-at "::")))))
	    (goto-char indent-point)
	    (c-beginning-of-closest-statement)
	    (c-add-syntax 'statement-cont (c-point 'boi)))
	   ;; CASE 17D: any old statement
	   ((< (point) indent-point)
	    (let ((safepos (c-most-enclosing-brace fullstate))
		  relpos done)
	      (goto-char indent-point)
	      (c-beginning-of-statement-1 safepos)
	      ;; It is possible we're on the brace that opens a nested
	      ;; function.
	      (if (and (eq (char-after) ?{)
		       (save-excursion
			 (c-backward-syntactic-ws safepos)
			 (not (eq (char-before) ?\;))))
		  (c-beginning-of-statement-1 safepos))
	      (if (and inswitch-p
		       (looking-at c-switch-label-key))
		  (progn
		    (goto-char (match-end 0))
		    (c-forward-syntactic-ws)))
	      (setq relpos (c-point 'boi))
	      (while (and (not done)
			  (<= safepos (point))
			  (/= relpos (point)))
		(c-beginning-of-statement-1 safepos)
		(if (= relpos (c-point 'boi))
		    (setq done t))
		(setq relpos (c-point 'boi)))
	      (c-add-syntax 'statement relpos)
	      (if (eq char-after-ip ?{)
		  (c-add-syntax 'block-open))))
	   ;; CASE 17E: first statement in an in-expression block
	   ((setq placeholder
		  (save-excursion
		    (goto-char containing-sexp)
		    (c-looking-at-inexpr-block)))
	    (goto-char containing-sexp)
	    (back-to-indentation)
	    (let ((block-intro (if (eq (car placeholder) 'inlambda)
				   'defun-block-intro
				 'statement-block-intro)))
	      (if (= containing-sexp (point))
		  (c-add-syntax block-intro (point))
		(goto-char (cdr placeholder))
		(back-to-indentation)
		(c-add-syntax block-intro (point))
		(if (/= (point) (cdr placeholder))
		    (c-add-syntax (car placeholder)))))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17F: first statement in an inline, or first
	   ;; statement in a top-level defun. we can tell this is it
	   ;; if there are no enclosing braces that haven't been
	   ;; narrowed out by a class (i.e. don't use bod here!)
	   ((save-excursion
	      (save-restriction
		(widen)
		(goto-char containing-sexp)
		(c-narrow-out-enclosing-class state containing-sexp)
		(not (c-most-enclosing-brace state))))
	    (goto-char containing-sexp)
	    ;; if not at boi, then defun-opening braces are hung on
	    ;; right side, so we need a different relpos
	    (if (/= (point) (c-point 'boi))
		(progn
		  (c-backward-syntactic-ws)
		  (c-safe (c-forward-sexp (if (eq (char-before) ?\))
					      -1 -2)))
		  ;; looking at a Java throws clause following a
		  ;; method's parameter list
		  (c-beginning-of-statement-1)
		  ))
	    (c-add-syntax 'defun-block-intro (c-point 'boi)))
	   ;; CASE 17G: First statement in a function declared inside
	   ;; a normal block.  This can only occur in Pike.
	   ((and (c-major-mode-is 'pike-mode)
		 (progn
		   (goto-char containing-sexp)
		   (and (not (c-looking-at-bos))
			(progn
			  (c-beginning-of-statement-1)
			  (not (looking-at c-conditional-key))))))
	    (c-add-syntax 'defun-block-intro (c-point 'boi)))
	   ;; CASE 17H: first statement in a block
	   (t (goto-char containing-sexp)
	      (if (/= (point) (c-point 'boi))
		  (c-beginning-of-statement-1
		   (if (= (point) lim)
		       (c-safe-position (point) state) lim)))
	      (c-add-syntax 'statement-block-intro (c-point 'boi))
	      (if (eq char-after-ip ?{)
		  (c-add-syntax 'block-open)))
	   ))
	 )
	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(cond
	 ;; are we looking at a comment only line?
	 ((and (looking-at c-comment-start-regexp)
	       (/= (c-forward-token-1 0 nil (c-point 'eol)) 0))
	  (c-add-syntax 'comment-intro))
	 ;; we might want to give additional offset to friends (in C++).
	 ((and (c-major-mode-is 'c++-mode)
	       (looking-at c-C++-friend-key))
	  (c-add-syntax 'friend))
	 ;; Start of a preprocessor directive?
	 ((and (eq literal 'pound)
	       (= (save-excursion
		    (c-beginning-of-macro lim)
		    (setq placeholder (point)))
		  (c-point 'boi))
	       (not (and (c-major-mode-is 'pike-mode)
			 (eq (char-after (1+ placeholder)) ?\"))))
	  (c-add-syntax 'cpp-macro)))
	;; return the syntax
	syntax))))


(defun agulbra-switch-cpp-h ()
  "Switch to the corresponding .cpp, .C, .cc or .h file."
  (interactive)
  (let ((n (buffer-file-name))
        (c nil))
    (cond ((and (string-match "\\.h$" n)
                (progn
                  (setq c (replace-match ".cpp" t t n))
                  (file-readable-p c)))
           (find-file c))
          ((and (string-match "\\.h$" n)
                (progn
                  (setq c (replace-match ".cc" t t n))
                  (file-readable-p c)))
           (find-file c))
          ((and (string-match "\\.h$" n)
                (progn
                  (setq c (replace-match ".C" t t n))
                  (file-readable-p c)))
           (find-file c))
          ((string-match "\\.h$" n)
           (find-file (replace-match ".cpp" t t n)))
          ((string-match "\\.h$" n)
           (find-file (replace-match ".cpp" t t n)))
          ;((string-match "_[a-z]+[0-9]*.cpp$" n)
          ; (find-file (replace-match ".h" t t n)))
          ((string-match "\\.cpp$" n)
           (find-file (replace-match ".h" t t n)))
          ((string-match "\\.cc$" n)
           (find-file (replace-match ".h" t t n)))
          ((string-match "\\.c$" n)
           (find-file (replace-match ".h" t t n)))
          (t
           (error "%s is neither .h, .cc, .C or .cpp" n)))))

;; ----- Second part, contrinuted by Klaralvdalens Datakonsult
(defvar kdab-qt-documentation
  "http://doc.trolltech.com/3.0/XXX.html"
  "URL for Qt documentation. XXX must be in the string. 
  Example: file:/packages/kde-src/qt-copy/doc/html/XXX.html")


;; special case for include files
;; Please notify blackie@klaralvdalens-datakonsult.se with any modification to this variable!
(defvar kdab-special-includes
  '( 
    (qlayout.h QHBoxLayout QVBoxLayout QGridLayout QBoxLayout)
    (qlistview.h QListViewItem QCheckListItem QListViewItemIterator)
    (qiconview.h QIconViewItem QIconDragItem QIconDrag)
    (qdragobject.h QTextDrag QStoredDrag QUriDag QColorDrag QImageDrag QDragManager)
    (qmime.h QMimeSource QMimeSourceFactory QWindowsMime)
    (qptrlist.h QPtrListIterator)
    (qevent.h QTimerEvent QMouseEvent QWheelEvent QTabletEvent QKeyEvent 
              QFocusEvent QPaintEvent QMoveEvent QResizeEvent QCloseEvent 
              QShowEvent QHideEvent QContextMenuEvent QIMEvent QDropEvent
              QDragMoveEvent QDragEnterEvent QDragResponseEvent QDragLeaveEvent
              QChildEvent QCustomEvent)
    (qdatetime.h QTime QDateTime QDate)
    
    ; Qt/Embedded
    (qcopchannel_qws.h QCopChannel)
    (qdirectpainter_qws.h QDirectPainter)
    (qfontfactorybdf_qws.h QFontFactoryBDF)
    (qfontfactoryttf_qws.h QFontFactoryFT)
    (qfontmanager_qws.h QGlyphMetrics QGlyph QRenderedFont QDiskFont QFontManager QFontFactory)
    (qgfx_qws.h QScreenCursor QPoolEntry QScreen QGfx)
    (qgfxlinuxfb_qws.h QLinuxFbScreen)
    (qgfxmatroxdefs_qws.h QQnxFbGfx QQnxScreen)
    (qgfxraster_qws.h QGfxRasterBase QGfxRaster)
    (qgfxvnc_qws.h QRfbRect QRfbPixelFormat QRfbServerInit QRfbSetEncodings 
                   QRfbFrameBufferUpdateRequest QRfbKeyEvent QRfbPointerEvent QRfbClientCutText QVNCServer)
    (qkeyboard_qws.h QWSKeyboardHandler)
    (qlock_qws.h QLock QLockHolder)
    (qmemorymanager_qws.h QMemoryManagerPixmap QMemoryManager)
    (qsoundqss_qws.h QWSSoundServer QWSSoundClient QWSSoundServerClient QWSSoundServerSocket)
    (qwindowsystem_qws.h QWSInternalWindowInfo QWSScreenSaver QWSWindow QWSSoundServer 
                         QWSServer QWSServer KeyboardFilter QWSClient)
    (qwsbeosdecoration_qws.h QWSBeOSDecoration)
    (qwscursor_qws.h QWSCursor)
    (qwsdecoration_qws.h QWSDecoration)
    (qwsdefaultdecoration_qws.h QWSDefaultDecoration)
    (qwsdisplay_qws.h QWSWindowInfo QWSDisplay)
    (qwshydrodecoration_qws.h QWSHydroDecoration)
    (qwskde2decoration_qws.h QWSKDE2Decoration)
    (qwskdedecoration_qws.h QWSKDEDecoration)
    (qwsmanager_qws.h QWSManager QWSButton)
    (qwsmouse_qws.h QWSPointerCalibrationData QWSMouseHandler QCalibratedMouseHandler 
                    QAutoMouseHandlerPrivate QWSMouseHandlerPrivate QVrTPanelHandlerPrivate 
                    QTPanelHandlerPrivate QYopyTPanelHandlerPrivate QCustomTPanelHandlerPrivate 
                    QVFbMouseHandlerPrivate)
    (qwsproperty_qws.h QWSPropertyManager)
    (qwsregionmanager_qws.h QWSRegionManager)
    (qwssocket_qws.h QWSSocket QWSServerSocket)
    (qwswindowsdecoration_qws.h QWSWindowsDecoration)

    ; KDE
    (kdebug.h kDebug kWarning kError kFatal kBacktrace)
    
    ) "List of special include files which do not follow the normal scheme")

;; Lookup class `cls' in kdab-special-includes and return the associate include file name
(defun kdab-map-special (cls)
  (let ((list kdab-special-includes)
        (found nil))
    (while (and list (not found))
      (let* ( (elm (car list))
              (include-file (car elm))
              (classes (cdr elm)))
        ( while (and classes (not found))
          (if (string= (downcase cls) (downcase (symbol-name (car classes))))
              (setq found include-file)
            (setq classes (cdr classes)))))
      (setq list (cdr list)))
    (if found
        (symbol-name found)
      nil)  ; return value
    ))
        

(defun kdab-word-under-point ()
  (save-excursion
    (let* ((start (if (= (preceding-char) ?\ )
                      (point)
                    (progn (backward-word 1) (point))))
           (end (progn (forward-word 1) (point))))
      (buffer-substring start end))))
    

;--------------------------------------------------------------------------------
; Insert include file.
; Place point anywhere on a class, and invoke this function. A result of
; this is that an include line is added (if it does not already exists) for
; the given class.
;--------------------------------------------------------------------------------
(defun kdab-insert-header ()
  (interactive "")
  (save-excursion
    (let* ((word (downcase (kdab-word-under-point)))
           (header (cond
                    ((kdab-map-special word) (kdab-map-special word))
                    ((string-match "^qdom" word) "qdom.h")
                    ((string-match "^qxml" word) "qxml.h")
                    (t (concat word ".h")))))
      (beginning-of-buffer)
      (if (not (re-search-forward (concat "#include *<" header ">") nil t))
          (progn
                                        ; No include existsed
            (goto-char (point-max)) ; Using end-of-buffer makes point move, dispete save-excursion
            (if (not (re-search-backward "^#include *[\"<][^\">]+\.h *[\">]" nil t))
                (beginning-of-buffer)
              (progn (end-of-line) (forward-char 1)))
            (if (file-exists-p header)
                (progn 
                  ; See this as a local file.
                  (insert "#include \"" header "\"\n")
                  (message (concat "inserted " "#include \"" header "\"")))
              (progn
                (insert "#include <" header ">\n")
                (message (concat "inserted " "#include <" header ">")))))
        (message (concat "header file \"" header "\" is already included"))))))




;--------------------------------------------------------------------------------
; Start konqueror with documentation for the class under point.
; set `kdab-qt-documentation' to specify the replacement for the documentation
;--------------------------------------------------------------------------------
(defun kdab-lookup-qt-documentation ()
  (interactive "")
  (save-excursion
    (let* ((word (downcase (kdab-word-under-point)))
          (url (if (not (string-match "XXX" kdab-qt-documentation))
                   (error "didn't find three X's in kdab-qt-documentation")
                 (replace-match word t t kdab-qt-documentation))))
      (start-process "qt documentation" nil "kfmclient" "openURL" url)
      (message (concat "Loading " url)))))


;; ----- Third part, contributed by various KDE developers

;;; func-menu is a package that scans your source file for function definitions
;;; and makes a menubar entry that lets you jump to any particular function
;;; definition by selecting it from the menu.  The following code turns this on
;;; for all of the recognized languages.  Scanning the buffer takes some time,
;;; but not much.

(if xemacs 
    (progn
      (require 'func-menu)
      (add-hook 'find-file-hooks 'fume-add-menubar-entry) )
  (progn
    (require 'imenu)))
    ;(add-hook 'find-file-hooks 'imenu)) )

;; Switch between the declaration of a class member in .cc/.cpp/.C, and its definition in the .h file
;; Written by David and Reggie after much hair tearing
(defun switch-to-function-def ()
  (interactive)
  (let ((n (buffer-file-name))
        (class "")
        (fn ""))
    (if (or (string-match "\\.cc$" n)
            (string-match "\\.cpp$" n)
            (string-match "\\.C$" n))
        (let ((a (fume-function-before-point)))
          (and (string-match "^\\(.*\\)::\\(.*\\)$" a)
               (progn
                 (setq class (match-string 1 a))
                 (setq fn (match-string 2 a))
                 (agulbra-switch-cpp-h)
                 (goto-char 0)
                 (re-search-forward class nil t)
                 (re-search-forward (concat "[ \t]+" fn "[ \t]*(") nil t)))))
    (if (string-match "\\.h$" n)
        (progn
          (save-excursion
            (forward-line 0)
            (re-search-forward "[ \t]+\\([^ \t(]+\\)[ \t]*(" nil t)
            (setq fn (match-string 1))
            (re-search-backward "^class \\([a-zA-Z0-9_]+\\)[ \t]*\\([a-zA-Z0-9_]*\\)" nil t)
            (setq class (match-string 1))
            (setq save (match-string 2))
            (and (string-match "Q_EXPORT" class)
                 (setq class save))
            (message (concat class "::" fn))
            )
          (agulbra-switch-cpp-h)
          (goto-char 0)
          (re-search-forward (concat "^[^()]*" class "::" fn "[ \t]*(") nil t)
          (message c-syntactic-context)
          )
)))

; Adds the current file to Makefile.am.
; Written by David.
(defun add-file-to-makefile-am ()
  "add the current file to the _SOURCES tag in the Makefile.am"
  (interactive)
  (let ((file (buffer-name))
        (makefile "Makefile.am"))
    (if (file-readable-p makefile )
	(message "")
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

; Inserts a kDebug statement showing the name of the current method.
; You need to create the empty line first.
(defun insert-kDebug ()
  (interactive)
  (insert "kDebug() << \"")
  (insert (fume-function-before-point))
  (insert "\" << endl;")
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
(define-key global-map [(button4)] 'scroll-me-down)
(define-key global-map [(button5)] 'scroll-me-up)
(define-key global-map [(shift button4)] 'scroll-me-down-a-bit)
(define-key global-map [(shift button5)] 'scroll-me-up-a-bit)

; Compilation
(defun makeclean () (interactive) (compile "make clean"))
(defun make () (interactive) (compile "make"))
(defun makeinstall () (interactive) (compile "make install"))
(defun makeinstallexec () (interactive) (compile "make install-exec"))
(defun makethisfile () (interactive)
    (let ((f (buffer-name)))
      (if (string-match "\.cpp$" f) (setq f (replace-match "\.lo" t t f)))
      (if (string-match "\.cc$" f) (setq f (replace-match "\.lo" t t f)))
      (compile (concat "make " f ))))

;; Indentation: 4 characters, no tabs.
(setq c-basic-offset 4)
(setq insert-tab-mode nil)
(setq-default require-final-newline t)
(setq-default next-line-add-newlines nil)

;; pc-like textmarking
(load "pc-select")
(if xemacs
    (pc-select-mode)
  (pc-selection-mode))

; Move in other window
(defun scroll-other-up () (interactive) (scroll-other-window-down 1)) ; hehe :)
(define-key global-map [(meta up)] 'scroll-other-up)
(defun scroll-other-down () (interactive) (scroll-other-window 1))
(define-key global-map [(meta down)] 'scroll-other-down)

;; Some example bindings, feel free to customize :)
(define-key global-map [(f2)] 'grep)
;; FIXME: remember to get these two working on Gnu/Emacs (Zack)
(define-key global-map [(f3)] 'fume-list-functions)
(define-key global-map [(shift f3)] 'fume-prompt-function-goto)
(define-key global-map [(shift button3)] 'mouse-function-menu)
(define-key global-map [(shift f4)] 'makeclean)
(define-key global-map [(f4)] 'make)
(define-key global-map [(f5)] 'makeinstall)
(define-key global-map [(shift f5)] 'makeinstallexec)
(define-key global-map [(shift f6)] 'makethisfile)
(define-key global-map [(f6)] 'agulbra-switch-cpp-h)
(define-key global-map [(f7)] 'switch-to-function-def)
;; imenu does that interactively
(if xemacs
    (define-key global-map [(f8)] 'function-menu))
;(define-key global-map [(f9)] 'agulbra-make-member) ;; uncomment this for a killer feature
(define-key global-map [(f10)] 'kdab-insert-header)
(define-key global-map [(shift f10)] 'kdab-lookup-qt-documentation)
(define-key global-map [(control meta d)] 'insert-kDebug)

; Standard Qt/KDE shortcuts: Ctrl+Backspace, Ctrl+Delete
(define-key global-map [(control backspace)] 'backward-kill-word)
(define-key global-map [(control delete)] 'kill-word)

; Standard Qt/KDE shortcuts: Control Pageup and Pagedown
(define-key global-map [(control prior)] 'beginning-of-buffer)
(define-key global-map [(control next)] 'end-of-buffer)

; currently no binding for header-protection and add-file-to-makefile-am,
; you need to call them from M-x

; -----------------------------------------------------------------
; The above list defines the following bindings:
;
; F2 : offer a grep command
;
; F3/Shift-F3/F8/Shift-RMB : different ways to see the list of methods in the current buffer
;
; F4 : make
; Shift-F4 : make clean
; F5 : make install
; Shift-F5 : make install-exec
;
; Shift-F6 : compile this file [assumes libtool is being used]
; F6 : Switch from .cpp/.cc to .h and vice-versa
; F7 : The same, but try to find the current method in the other file
; F9 (if enabled) : Create a member method in the .cpp, the cursor being on the definition in the .h
; F10: Place point on a class name, and the respective (Qt) include file will be inserted.
; This works with all Qt classes but can easily be extended to KDE classes.
; Shift-F10: Place point on a class name, and press Shift-F10, and konqueror will load
;            Qt documentation. Customize the location of the Qt documentation with the 
;            variable kdab-qt-documentation. XXX will be replace with the class name.
;            Example (setq kdab-qt-location "file:/packages/kde-src/qt-copy/doc/html/XXX.html")
;
; Ctrl+Meta+D : insert a kDebug statement with the name of the current method
; [the new hide-all-windows shortcut conflicts with that, you may have to
;  change it, or use Ctrl+Meta+Shift+D (!!)]
;
; Meta Up/Down : scroll the other window (when window is split)

; Other very useful keybindings to know about:
; C-x r m    to set a named bookmark in the buffer
; C-x r b    to jump to a named bookmark in the buffer

(setq-default initial-scratch-message
      "File kde-devel-emacs.el is deprecated! 
Please use KDE-Emacs from kdesdk/scripts/kde-emacs.")
