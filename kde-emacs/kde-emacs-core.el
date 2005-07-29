;; kde-emacs-core.el - core functionality,e.g. syntax & c++-mode-hook
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
;; Foundation, Inc., 51 Franklin Steet, Fifth Floor, Boston, MA
;; 02110-1301  USA

(require 'kde-emacs-vars)
;*---------------------------------------------------------------------*/
;*    Variables ...                                                    */
;*---------------------------------------------------------------------*/

(defcustom kde-tab-behavior 'default
  "Specifies the current tab behavior. default will expand try to complete
the symbol at point if at the end of something that looks like an indentifier else
it will indent the current line if the pointer is at the beginning of the line it will
be moved the the start of the indention. abbrev-indent behaves like default, but the 
cursor isn't moved to the beginning of the indention with tab is pressed when the cursor
is at the beginning of the line. indent simply indents the line without trying to 
complete the symbol"
  :group 'kde-devel
  :version "0.1"
  :type `(choice (const default) (const abbrev-indent) (const indent)))

;*---------------------------------------------------------------------*/
;*    Functions ...                                                    */
;*---------------------------------------------------------------------*/


;; -------  First part, from Arnt's "c++ stuff" - slightly modified for our needs :)

(defun agulbra-c++-tab (arg)
  "Do the right thing about tabs in c++ mode.
Try to finish the symbol, or indent the line."
  (interactive "*P")
  (cond
   ((and (not (looking-at "[A-Za-z0-9]"))
         (save-excursion 
           (forward-char -1)
           (looking-at "[A-Za-z0-9:>_\\-\\&\\.(){}\\*\\+/]")))
         (dabbrev-expand arg))
   (t
    (if (eq kde-tab-behavior 'default)
	(c-indent-command)
      (save-excursion
	(beginning-of-line)
	(c-indent-command))))))

(defun agulbra-clean-out-spaces ()
  "Remove spaces at ends of lines."
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
	   nil
           ))))

; the above used to contain (untabify (point-min) (point-max)) too

(defun agulbra-c++-clean-out-spaces ()
  "Remove spaces at ends of lines, only in c++ mode."
  (interactive)
  (if (eq major-mode 'c++-mode)
      (agulbra-clean-out-spaces)
    )
  )

(defun agulbra-delete-into-nomenclature (&optional arg)
  "Delete forward until the end of a nomenclature section or word.
With arg, do it arg times."
  (interactive "p")
  (save-excursion
    (let ((b (point-marker)))
      (c-forward-into-nomenclature arg)
      (delete-region b (point-marker)))))


(if (not (fboundp 'font-lock-add-keywords))
    (defun font-lock-add-keywords (mode keywords &optional append)
      "XEmacs doesn't have font-lock-add-keywords so we provide it."
      (font-lock-set-defaults)
      (if (eq append 'set)
	  (setq font-lock-keywords keywords)
	; NOTE: write this function for XEmacs - Zack
	;(font-lock-remove-keywords nil keywords) ;to avoid duplicates
	(let ((old (if (eq (car-safe font-lock-keywords) t)
		       (cdr font-lock-keywords)
		     font-lock-keywords)))
	  (setq font-lock-keywords (if append
				       (append old keywords)
				     (append keywords old))))))
  )

(c-add-style "kde-c" '("stroustrup"
		       (c-basic-offset . 4)
		       (c-offsets-alist
			(case-label . 4)
			(access-label . -)
			(label . 0)
			(statement-cont . c-lineup-math)
			)))

;  ( we use Backquote ( '`' ) instead of "'" because we want
;    kde-access-labels to be evaluated... )
(c-add-style "kde-c++" `("kde-c"
  ;;FIXME: 1) fume functions not available on GNU/Emacs
  ;;       2) insert-tab-mode no longer present (free variable)
  ;;       3) c-hangin-commment-under-p no longer present (free variable)
			 (if (not (eq kde-tab-behavior 'indent))
			     (c-tab-always-indent . nil))
					; (insert-tab-mode nil)
			 (indent-tabs-mode . nil)
			 (if (eq kde-emacs-type 'xemacs)
			     (fume-auto-rescan-buffer-p nil))
			 (c-access-key . ,kde-access-labels)
			 (c-opt-access-key . ,kde-access-labels)
					; (c-hanging-comment-under-p nil)
			 (c-offsets-alist . ((case-label . 0)
					     (inline-open . 0)))
			 ))
		    
;; KDE C++ mode
;; Not a "(setq c++-mode-hook ..." because this way we would
;; prune all other hooks!
(defun kde-c++-mode-hook ()
  (font-lock-mode)
  (c-set-style kde-c++-style)
  (define-key c++-mode-map "\C-m" 'c-context-line-break)
  (when (or
	 (eq kde-tab-behavior 'default)
	 (eq kde-tab-behavior 'abbrev-indent))
    (define-key c++-mode-map "\C-i" 'agulbra-c++-tab))
  (define-key c++-mode-map "\ef" 'c-forward-into-nomenclature)
  (define-key c++-mode-map "\ed" 'agulbra-delete-into-nomenclature)
  (define-key c++-mode-map "\eb" 'c-backward-into-nomenclature)
  ;; fontify "public|protected|private slots" with one and the same face :)
  ;; NOTE: write face-at-point function to fontify those just like other
  ;; access specifiers
  (font-lock-add-keywords nil '(("\\<\\(\\(public\\|protected\\|private\\) slots\\)\\>" 
				 . font-lock-reference-face)))
  ;; Add (setq magic-keys-mode nil) to your .emacs (before loading this file)
  ;; to disable the magic keys in C++ mode.
  (and (boundp 'magic-keys-mode) magic-keys-mode
       (progn
	 (define-key c++-mode-map "\(" 'insert-parens)
	 (define-key c++-mode-map "\)" 'insert-parens2)
	 (define-key c++-mode-map "\," 'insert-comma)
	 (define-key c++-mode-map "\{" 'insert-curly-brace)
	 ))
  )

(defun kde-c-mode-hook ()
  (font-lock-mode)
  (c-set-style kde-c-style))

;; NOTE : This is a completely new c-guess-basic-syntax, it's faster, 
;;        better, meaner, harder, covers more cases, more c++ syntax,
;;        and is in general cooler ;) You have to have the new cc-mode
;;        to use it ( 5.30 at least, check it with "M-x c-version")
;;        If you don't have 5.30 comment out the following c-guess-basic-syntax
;;        and uncomment the one underneath.
(cond
 ((string-match "^5\\.30\\." c-version)
  (defun c-guess-basic-syntax ()
  "Return the syntactic context of the current line.
This function does not do any hidden buffer changes."
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (c-save-buffer-state
	  ((indent-point (point))
	   (case-fold-search nil)
	   (paren-state (c-parse-state))
	   literal containing-sexp char-before-ip char-after-ip lim
	   c-syntactic-context placeholder c-in-literal-cache step-type
	   tmpsymbol keyword injava-inher special-brace-list
	   ;; narrow out any enclosing class or extern "C" block
	   (inclass-p (c-narrow-out-enclosing-class paren-state
						    indent-point))
	   ;; `c-state-cache' is shadowed here so that we don't
	   ;; throw it away due to the narrowing that might be done
	   ;; by the function above.  That means we must not do any
	   ;; changes during the execution of this function, since
	   ;; `c-invalidate-state-cache' then would change this local
	   ;; variable and leave a bogus value in the global one.
	   (c-state-cache (if inclass-p
			      (c-whack-state-before (point-min) paren-state)
			    paren-state))
	   (c-state-cache-start (point-min))
	   inenclosing-p macro-start in-macro-expr
	   ;; There's always at most one syntactic element which got
	   ;; a relpos.  It's stored in syntactic-relpos.
	   syntactic-relpos
	   (c-stmt-delim-chars c-stmt-delim-chars))
	;; Check for meta top-level enclosing constructs such as
	;; extern language definitions.
	(save-excursion
	  (save-restriction
	    (widen)
	    (when (and inclass-p
		       (progn
			 (goto-char (aref inclass-p 0))
			 (looking-at c-other-decl-block-key)))
	      (setq inenclosing-p (match-string 1))
	      (if (string-equal inenclosing-p "extern")
		  ;; Compatibility with legacy choice of name for the
		  ;; extern-lang syntactic symbols.
		  (setq inenclosing-p "extern-lang")))))

	;; Init some position variables:
	;;
	;; containing-sexp is the open paren of the closest
	;; surrounding sexp or nil if there is none that hasn't been
	;; narrowed out.
	;;
	;; lim is the position after the closest preceding brace sexp
	;; (nested sexps are ignored), or the position after
	;; containing-sexp if there is none, or (point-min) if
	;; containing-sexp is nil.
	;;
	;; c-state-cache is the state from c-parse-state at
	;; indent-point, without any parens outside the region
	;; narrowed by c-narrow-out-enclosing-class.
	;;
	;; paren-state is the state from c-parse-state outside
	;; containing-sexp, or at indent-point if containing-sexp is
	;; nil.  paren-state is not limited to the narrowed region, as
	;; opposed to c-state-cache.
	(if c-state-cache
	    (progn
	      (setq containing-sexp (car paren-state)
		    paren-state (cdr paren-state))
	      (if (consp containing-sexp)
		  (progn
		    (setq lim (cdr containing-sexp))
		    (if (cdr c-state-cache)
			;; Ignore balanced paren.  The next entry
			;; can't be another one.
			(setq containing-sexp (car (cdr c-state-cache))
			      paren-state (cdr paren-state))
		      ;; If there is no surrounding open paren then
		      ;; put the last balanced pair back on paren-state.
		      (setq paren-state (cons containing-sexp paren-state)
			    containing-sexp nil)))
		(setq lim (1+ containing-sexp))))
	  (setq lim (point-min)))

	;; If we're in a parenthesis list then ',' delimits the
	;; "statements" rather than being an operator (with the
	;; exception of the "for" clause).  This difference is
	;; typically only noticeable when statements are used in macro
	;; arglists.
	(when (and containing-sexp
		   (eq (char-after containing-sexp) ?\())
	  (setq c-stmt-delim-chars c-stmt-delim-chars-with-comma))

	;; cache char before and after indent point, and move point to
	;; the most likely position to perform the majority of tests
	(goto-char indent-point)
	(c-backward-syntactic-ws lim)
	(setq char-before-ip (char-before))
	(goto-char indent-point)
	(skip-chars-forward " \t")
	(setq char-after-ip (char-after))

	;; are we in a literal?
	(setq literal (c-in-literal lim))

	;; now figure out syntactic qualities of the current line
	(cond
	 ;; CASE 1: in a string.
	 ((eq literal 'string)
	  (c-add-syntax 'string (c-point 'bopl)))
	 ;; CASE 2: in a C or C++ style comment.
	 ((and (memq literal '(c c++))
	       ;; This is a kludge for XEmacs where we use
	       ;; `buffer-syntactic-context', which doesn't correctly
	       ;; recognize "\*/" to end a block comment.
	       ;; `parse-partial-sexp' which is used by
	       ;; `c-literal-limits' will however do that in most
	       ;; versions, which results in that we get nil from
	       ;; `c-literal-limits' even when `c-in-literal' claims
	       ;; we're inside a comment.
	       (setq placeholder (c-literal-limits lim)))
	  (c-add-syntax literal (car placeholder)))
	 ;; CASE 3: in a cpp preprocessor macro continuation.
	 ((and (save-excursion
		 (when (c-beginning-of-macro)
		   (setq macro-start (point))))
	       (/= macro-start (c-point 'boi))
	       (progn
		 (setq tmpsymbol 'cpp-macro-cont)
		 (or (not c-syntactic-indentation-in-macros)
		     (save-excursion
		       (goto-char macro-start)
		       ;; If at the beginning of the body of a #define
		       ;; directive then analyze as cpp-define-intro
		       ;; only.  Go on with the syntactic analysis
		       ;; otherwise.  in-macro-expr is set if we're in a
		       ;; cpp expression, i.e. before the #define body
		       ;; or anywhere in a non-#define directive.
		       (if (c-forward-to-cpp-define-body)
			   (let ((indent-boi (c-point 'boi indent-point)))
			     (setq in-macro-expr (> (point) indent-boi)
				   tmpsymbol 'cpp-define-intro)
			     (= (point) indent-boi))
			 (setq in-macro-expr t)
			 nil)))))
	  (c-add-syntax tmpsymbol macro-start)
	  (setq macro-start nil))
	 ;; CASE 11: an else clause?
	 ((looking-at "else\\>[^_]")
	  (c-beginning-of-statement-1 containing-sexp)
	  (c-add-stmt-syntax 'else-clause nil t nil
			     containing-sexp paren-state))
	 ;; CASE 12: while closure of a do/while construct?
	 ((and (looking-at "while\\>[^_]")
	       (save-excursion
		 (prog1 (eq (c-beginning-of-statement-1 containing-sexp)
			    'beginning)
		   (setq placeholder (point)))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'do-while-closure nil t nil
			     containing-sexp paren-state))
	 ;; CASE 13: A catch or finally clause?  This case is simpler
	 ;; than if-else and do-while, because a block is required
	 ;; after every try, catch and finally.
	 ((save-excursion
	    (and (cond ((c-major-mode-is 'c++-mode)
			(looking-at "catch\\>[^_]"))
		       ((c-major-mode-is 'java-mode)
			(looking-at "\\(catch\\|finally\\)\\>[^_]")))
		 (and (c-safe (c-backward-syntactic-ws)
			      (c-backward-sexp)
			      t)
		      (eq (char-after) ?{)
		      (c-safe (c-backward-syntactic-ws)
			      (c-backward-sexp)
			      t)
		      (if (eq (char-after) ?\()
			  (c-safe (c-backward-sexp) t)
			t))
		 (looking-at "\\(try\\|catch\\)\\>[^_]")
		 (setq placeholder (point))))
	  (goto-char placeholder)
	  (c-add-stmt-syntax 'catch-clause nil t nil
			     containing-sexp paren-state))
	 ;; CASE 18: A substatement we can recognize by keyword.
	 ((save-excursion
	    (and c-opt-block-stmt-key
		 (if (c-mode-is-new-awk-p)
                     (c-awk-prev-line-incomplete-p containing-sexp) ; ACM 2002/3/29
                   (not (eq char-before-ip ?\;)))
		 (not (memq char-after-ip '(?\) ?\] ?,)))
		 (or (not (eq char-before-ip ?}))
		     (c-looking-at-inexpr-block-backward c-state-cache))
		 (> (point)
		    (progn
		      ;; Ought to cache the result from the
		      ;; c-beginning-of-statement-1 calls here.
		      (setq placeholder (point))
		      (while (eq (setq step-type
				       (c-beginning-of-statement-1 lim))
				 'label))
		      (if (eq step-type 'previous)
			  (goto-char placeholder)
			(setq placeholder (point))
			(if (and (eq step-type 'same)
				 (not (looking-at c-opt-block-stmt-key)))
			    ;; Step up to the containing statement if we
			    ;; stayed in the same one.
			    (let (step)
			      (while (eq
				      (setq step
					    (c-beginning-of-statement-1 lim))
				      'label))
			      (if (eq step 'up)
				  (setq placeholder (point))
				;; There was no containing statement afterall.
				(goto-char placeholder)))))
		      placeholder))
		 (if (looking-at c-block-stmt-2-key)
		     ;; Require a parenthesis after these keywords.
		     ;; Necessary to catch e.g. synchronized in Java,
		     ;; which can be used both as statement and
		     ;; modifier.
		     (and (zerop (c-forward-token-2 1 nil))
			  (eq (char-after) ?\())
		   (looking-at c-opt-block-stmt-key))))
	  (if (eq step-type 'up)
	      ;; CASE 18A: Simple substatement.
	      (progn
		(goto-char placeholder)
		(cond
		 ((eq char-after-ip ?{)
		  (c-add-stmt-syntax 'substatement-open nil nil nil
				     containing-sexp paren-state))
		 ((save-excursion
		    (goto-char indent-point)
		    (back-to-indentation)
		    (looking-at c-label-key))
		  (c-add-stmt-syntax 'substatement-label nil nil nil
				     containing-sexp paren-state))
		 (t
		  (c-add-stmt-syntax 'substatement nil nil nil
				     containing-sexp paren-state))))
	    ;; CASE 18B: Some other substatement.  This is shared
	    ;; with case 10.
	    (c-guess-continued-construct indent-point
					 char-after-ip
					 placeholder
					 lim
					 paren-state)))
	 ;; CASE 4: In-expression statement.  C.f. cases 7B, 16A and
	 ;; 17E.
	 ((and (or c-opt-inexpr-class-key
		   c-opt-inexpr-block-key
		   c-opt-lambda-key)
	       (setq placeholder (c-looking-at-inexpr-block
				  (c-safe-position containing-sexp paren-state)
				  containing-sexp)))
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
	  (c-add-stmt-syntax tmpsymbol nil t nil
			     (c-most-enclosing-brace c-state-cache (point))
			     (c-whack-state-after (point) paren-state))
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
	     ;; CASE 5A.1: Non-class declaration block open.
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t")
		(and (c-safe (c-backward-sexp 2) t)
		     (looking-at c-other-decl-block-key)
		     (setq keyword (match-string 1)
			   placeholder (point))
		     (if (string-equal keyword "extern")
			 ;; Special case for extern-lang-open.  The
			 ;; check for a following string is disabled
			 ;; since it doesn't disambiguate anything.
			 (and ;;(progn
			      ;;  (c-forward-sexp 1)
			      ;;  (c-forward-syntactic-ws)
			      ;;  (eq (char-after) ?\"))
			      (setq tmpsymbol 'extern-lang-open))
		       (setq tmpsymbol (intern (concat keyword "-open"))))
		     ))
	      (goto-char placeholder)
	      (c-add-syntax tmpsymbol (c-point 'boi)))
	     ;; CASE 5A.2: we are looking at a class opening brace
	     ((save-excursion
		(goto-char indent-point)
		(skip-chars-forward " \t{")
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (and decl
		       (setq placeholder (aref decl 0)))
		  ))
	      (c-add-syntax 'class-open placeholder))
	     ;; CASE 5A.3: brace list open
	     ((save-excursion
		(c-beginning-of-decl-1 lim)
		(while (looking-at c-specifier-key)
		  (goto-char (match-end 1))
		  (c-forward-syntactic-ws indent-point))
		(setq placeholder (c-point 'boi))
		(or (consp special-brace-list)
		    (and (or (save-excursion
			       (goto-char indent-point)
			       (setq tmpsymbol nil)
			       (while (and (> (point) placeholder)
					   (zerop (c-backward-token-2 1 t))
					   (/= (char-after) ?=))
				 (and c-opt-inexpr-brace-list-key
				      (not tmpsymbol)
				      (looking-at c-opt-inexpr-brace-list-key)
				      (setq tmpsymbol 'topmost-intro-cont)))
			       (eq (char-after) ?=))
			     (looking-at c-brace-list-key))
			 (save-excursion
			   (while (and (< (point) indent-point)
				       (zerop (c-forward-token-2 1 t))
				       (not (memq (char-after) '(?\; ?\()))))
			   (not (memq (char-after) '(?\; ?\()))
			   ))))
	      (if (and (not c-auto-newline-analysis)
		       (c-major-mode-is 'java-mode)
		       (eq tmpsymbol 'topmost-intro-cont))
		  ;; We're in Java and have found that the open brace
		  ;; belongs to a "new Foo[]" initialization list,
		  ;; which means the brace list is part of an
		  ;; expression and not a top level definition.  We
		  ;; therefore treat it as any topmost continuation
		  ;; even though the semantically correct symbol still
		  ;; is brace-list-open, on the same grounds as in
		  ;; case B.2.
		  (progn
		    (c-beginning-of-statement-1 lim)
		    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
		(c-add-syntax 'brace-list-open placeholder)))
	     ;; CASE 5A.4: inline defun open
	     ((and inclass-p (not inenclosing-p))
	      (c-add-syntax 'inline-open)
	      (c-add-class-syntax 'inclass inclass-p paren-state))
	     ;; CASE 5A.5: ordinary defun open
	     (t
	      (goto-char placeholder)
	      (if (or inclass-p macro-start)
		  (c-add-syntax 'defun-open (c-point 'boi))
		;; Bogus to use bol here, but it's the legacy.
		(c-add-syntax 'defun-open (c-point 'bol)))
	      )))
	   ;; CASE 5B: first K&R arg decl or member init
	   ((c-just-after-func-arglist-p lim)
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
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
	      (if inclass-p
		  (c-add-class-syntax 'inclass inclass-p paren-state)))
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
	   ((or (and (c-major-mode-is 'c++-mode)
		     (progn
		       (when (eq char-after-ip ?,)
			 (skip-chars-forward " \t")
			 (forward-char))
		       (looking-at c-opt-postfix-decl-spec-key)))
		(and (or (eq char-before-ip ?:)
			 ;; watch out for scope operator
			 (save-excursion
			   (and (eq char-after-ip ?:)
				(c-safe (forward-char 1) t)
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
			   (cond ((looking-at c-opt-postfix-decl-spec-key)
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
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      ;; don't add inclass symbol since relative point already
	      ;; contains any class offset
	      )
	     ;; CASE 5C.2: hanging colon on an inher intro
	     ((eq char-before-ip ?:)
	      (c-beginning-of-statement-1 lim)
	      (c-add-syntax 'inher-intro (c-point 'boi))
	      (if inclass-p
		  (c-add-class-syntax 'inclass inclass-p paren-state)))
	     ;; CASE kde hack:
	     ((and inclass-p
		   c-access-key
		   (looking-at c-access-key))
	      (c-add-syntax 'access-label (c-point 'bonl))
	      (c-add-class-syntax 'inclass inclass-p paren-state)
	      )
	     ;; CASE 5C.3: in a Java implements/extends
	     (injava-inher
	      (let ((where (cdr injava-inher))
		    (cont (car injava-inher)))
		(goto-char where)
		(cond ((looking-at "throws\\>[^_]")
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
	   ;; CASE 5D: this could be a top-level initialization, a
	   ;; member init list continuation, or a template argument
	   ;; list continuation.
	   ((c-with-syntax-table (if (c-major-mode-is 'c++-mode)
				     c++-template-syntax-table
				   (syntax-table))
	      (save-excursion
		;; Note: We use the fact that lim is always after any
		;; preceding brace sexp.
		(while (and (zerop (c-backward-token-2 1 t lim))
			    (not (looking-at "[;<,=]"))))
		(or (memq (char-after) '(?, ?=))
		    (and (c-major-mode-is 'c++-mode)
			 (zerop (c-backward-token-2 1 nil lim))
			 (eq (char-after) ?<)))))
	    (goto-char indent-point)
	    (setq placeholder
		  (c-beginning-of-member-init-list lim))
	    (cond
	     ;; CASE 5D.1: hanging member init colon, but watch out
	     ;; for bogus matches on access specifiers inside classes.
	     ((and placeholder
		   (save-excursion
		     (setq placeholder (point))
		     (c-backward-token-2 1 t lim)
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
		       (if c-opt-access-key
			   (not (looking-at c-opt-access-key)) t)
		       (not (looking-at c-class-key))
		       (if c-opt-bitfield-key
			   (not (looking-at c-opt-bitfield-key)) t))
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
	     ;; CASE 5D.3: perhaps a template list continuation?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (save-restriction
		       (c-with-syntax-table c++-template-syntax-table
			 (goto-char indent-point)
			 (setq placeholder (c-up-list-backward (point)))
			 (and placeholder
			      (eq (char-after placeholder) ?<))))))
	      ;; we can probably indent it just like an arglist-cont
	      (goto-char placeholder)
	      (c-beginning-of-statement-1 lim t)
	      (c-add-syntax 'template-args-cont (c-point 'boi)))
	     ;; CASE 5D.4: perhaps a multiple inheritance line?
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (c-beginning-of-statement-1 lim)
		     (setq placeholder (point))
		     (if (looking-at "static\\>[^_]")
			 (c-forward-token-2 1 nil indent-point))
		     (and (looking-at c-class-key)
			  (zerop (c-forward-token-2 2 nil indent-point))
			  (if (eq (char-after) ?<)
			      (c-with-syntax-table c++-template-syntax-table
				(zerop (c-forward-token-2 1 t indent-point)))
			    t)
			  (eq (char-after) ?:))))
	      (goto-char placeholder)
	      (c-add-syntax 'inher-cont (c-point 'boi)))
	     ;; CASE 5D.5: Continuation of the "expression part" of a
	     ;; top level construct.
	     (t
	      (while (and (eq (car (c-beginning-of-decl-1 containing-sexp))
			      'same)
			  (save-excursion
			    (c-backward-syntactic-ws)
			    (eq (char-before) ?}))))
	      (c-add-stmt-syntax
	       (if (eq char-before-ip ?,)
		   ;; A preceding comma at the top level means that a
		   ;; new variable declaration starts here.  Use
		   ;; topmost-intro-cont for it, for consistency with
		   ;; the first variable declaration.  C.f. case 5N.
		   'topmost-intro-cont
		 'statement-cont)
	       nil nil nil containing-sexp paren-state))
	     ))
	   ;; CASE 5E: we are looking at a access specifier
	   ((and inclass-p
		 c-opt-access-key
		 (looking-at c-opt-access-key))
	    (setq placeholder (c-add-class-syntax 'inclass inclass-p
						  paren-state))
	    ;; Append access-label with the same anchor point as inclass gets.
	    (c-append-syntax 'access-label placeholder))
	   ;; CASE 5F: Close of a non-class declaration level block.
	   ((and inenclosing-p
		 (eq char-after-ip ?}))
	    (c-add-syntax (intern (concat inenclosing-p "-close"))
			  (aref inclass-p 0)))
	   ;; CASE 5G: we are looking at the brace which closes the
	   ;; enclosing nested class decl
	   ((and inclass-p
		 (eq char-after-ip ?})
		 (save-excursion
		   (save-restriction
		     (widen)
		     (forward-char 1)
		     (and (c-safe (c-backward-sexp 1) t)
			  (= (point) (aref inclass-p 1))
			  ))))
	    (c-add-class-syntax 'class-close inclass-p paren-state))
	   ;; CASE 5H: we could be looking at subsequent knr-argdecls
	   ((and c-recognize-knr-p
		 (not (eq char-before-ip ?}))
		 (save-excursion
		   (setq placeholder (cdr (c-beginning-of-decl-1 lim)))
		   (and placeholder
			;; Do an extra check to avoid tripping up on
			;; statements that occur in invalid contexts
			;; (e.g. in macro bodies where we don't really
			;; know the context of what we're looking at).
			(not (and c-opt-block-stmt-key
				  (looking-at c-opt-block-stmt-key)))))
		 (< placeholder indent-point))
	    (goto-char placeholder)
	    (c-add-syntax 'knr-argdecl (point)))
	   ;; CASE 5I: ObjC method definition.
	   ((and c-opt-method-key
		 (looking-at c-opt-method-key))
	    (c-beginning-of-statement-1 lim)
	    (c-add-syntax 'objc-method-intro (c-point 'boi)))
           ;; CASE 5P: AWK pattern or function or continuation
           ;; thereof.
           ((c-mode-is-new-awk-p)
            (setq placeholder (point))
            (c-add-stmt-syntax
             (if (and (eq (c-beginning-of-statement-1) 'same)
                      (/= (point) placeholder))
                 'topmost-intro-cont
               'topmost-intro)
             nil nil nil
             containing-sexp paren-state))
	   ;; CASE 5N: At a variable declaration that follows a class
	   ;; definition or some other block declaration that doesn't
	   ;; end at the closing '}'.  C.f. case 5D.5.
	   ((progn
	      (c-backward-syntactic-ws lim)
	      (and (eq (char-before) ?})
		   (save-excursion
		     (let ((start (point)))
		       (if paren-state
			   ;; Speed up the backward search a bit.
			   (goto-char (car (car paren-state))))
		       (c-beginning-of-decl-1 containing-sexp)
		       (setq placeholder (point))
		       (if (= start (point))
			   ;; The '}' is unbalanced.
			   nil
			 (c-end-of-decl-1)
			 (>= (point) indent-point))))))
	    (goto-char placeholder)
	    (c-add-stmt-syntax 'topmost-intro-cont nil nil nil
			       containing-sexp paren-state))
	   ;; CASE 5J: we are at the topmost level, make
	   ;; sure we skip back past any access specifiers
	   ((progn
	      (while (and inclass-p
			  c-opt-access-key
			  (not (bobp))
			  (save-excursion
			    (c-safe (progn (c-backward-sexp 1) t))
			    (and (looking-at "slots:")
				 (c-backward-sexp 1))
			    (looking-at c-opt-access-key)))
		(c-backward-sexp 1)
		(c-backward-syntactic-ws lim))
	      (or (bobp)
                  (if (c-mode-is-new-awk-p)
                      (not (c-awk-prev-line-incomplete-p))
                    (memq (char-before) '(?\; ?})))
		  (and (c-major-mode-is 'objc-mode)
		       (progn
			 (c-beginning-of-statement-1 lim)
			 (eq (char-after) ?@)))))
	    ;; real beginning-of-line could be narrowed out due to
	    ;; enclosure in a class block
	    (save-restriction
	      (widen)
	      (c-add-syntax 'topmost-intro (c-point 'bol))
	      ;; Using bol instead of boi above is highly bogus, and
	      ;; it makes our lives hard to remain compatible. :P
	      (if inclass-p
		  (progn
		    (goto-char (aref inclass-p 1))
		    (or (= (point) (c-point 'boi))
			(goto-char (aref inclass-p 0)))
		    (if inenclosing-p
			(c-add-syntax (intern (concat "in" inenclosing-p))
				      (c-point 'boi))
		      (c-add-class-syntax 'inclass inclass-p paren-state))
		    ))
	      (when (and c-syntactic-indentation-in-macros
			 macro-start
			 (/= macro-start (c-point 'boi indent-point)))
		(c-add-syntax 'cpp-define-intro)
		(setq macro-start nil))
	      ))
	   ;; CASE 5K: we are at an ObjC method definition
	   ;; continuation line.
	   ((and c-opt-method-key
		 (progn
		   (c-beginning-of-statement-1 lim)
		   (beginning-of-line)
		   (looking-at c-opt-method-key)))
	    (c-add-syntax 'objc-method-args-cont (point)))
	   ;; CASE 5L: we are at the first argument of a template
	   ;; arglist that begins on the previous line.
	   ((eq (char-before) ?<)
	    (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	    (c-add-syntax 'template-args-cont (c-point 'boi)))
	   ;; CASE 5M: we are at a topmost continuation line
	   (t
	    (c-beginning-of-statement-1 (c-safe-position (point) paren-state))
	    (c-add-syntax 'topmost-intro-cont (c-point 'boi)))
	   ))
	 ;; (CASE 6 has been removed.)
	 ;; CASE 7: line is an expression, not a statement.  Most
	 ;; likely we are either in a function prototype or a function
	 ;; call argument list
	 ((not (or (and c-special-brace-lists
			(save-excursion
			  (goto-char containing-sexp)
			  (c-looking-at-special-brace-list)))
		   (eq (char-after containing-sexp) ?{)))
	  (cond
	   ;; CASE 7A: we are looking at the arglist closing paren.
	   ;; C.f. case 7F.
	   ((memq char-after-ip '(?\) ?\]))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (if (and (c-safe (backward-up-list 1) t)
		     (> (point) placeholder))
		(progn
		  (forward-char)
		  (skip-chars-forward " \t"))
	      (goto-char placeholder))
	    (c-add-stmt-syntax 'arglist-close (list containing-sexp) t nil
			       (c-most-enclosing-brace paren-state (point))
			       (c-whack-state-after (point) paren-state)))
	   ;; CASE 7B: Looking at the opening brace of an
	   ;; in-expression block or brace list.  C.f. cases 4, 16A
	   ;; and 17E.
	   ((and (eq char-after-ip ?{)
		 (progn
		   (setq placeholder (c-inside-bracelist-p (point)
							   c-state-cache))
		   (if placeholder
		       (setq tmpsymbol '(brace-list-open . inexpr-class))
		     (setq tmpsymbol '(block-open . inexpr-statement)
			   placeholder
			   (cdr-safe (c-looking-at-inexpr-block
				      (c-safe-position containing-sexp
						       paren-state)
				      containing-sexp)))
		     ;; placeholder is nil if it's a block directly in
		     ;; a function arglist.  That makes us skip out of
		     ;; this case.
		     )))
	    (goto-char placeholder)
	    (back-to-indentation)
	    (c-add-stmt-syntax (car tmpsymbol) nil t nil
			       (c-most-enclosing-brace paren-state (point))
			       (c-whack-state-after (point) paren-state))
	    (if (/= (point) placeholder)
		(c-add-syntax (cdr tmpsymbol))))
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
	   ((progn
	      (goto-char containing-sexp)
	      (and (c-safe (c-forward-sexp -1) t)
		   (looking-at "\\<for\\>[^_]")))
	    (goto-char (1+ containing-sexp))
	    (c-forward-syntactic-ws indent-point)
	    (if (eq char-before-ip ?\;)
		(c-add-syntax 'statement (point))
	      (c-add-syntax 'statement-cont (point))
	      ))
	   ;; CASE 7E: maybe a continued ObjC method call. This is the
	   ;; case when we are inside a [] bracketed exp, and what
	   ;; precede the opening bracket is not an identifier.
	   ((and c-opt-method-key
		 (eq (char-after containing-sexp) ?\[)
		 (progn
		   (goto-char (1- containing-sexp))
		   (c-backward-syntactic-ws (c-point 'bod))
		   (if (not (looking-at c-symbol-key))
		       (c-add-syntax 'objc-method-call-cont containing-sexp))
		   )))
	   ;; CASE 7F: we are looking at an arglist continuation line,
	   ;; but the preceding argument is on the same line as the
	   ;; opening paren.  This case includes multi-line
	   ;; mathematical paren groupings, but we could be on a
	   ;; for-list continuation line.  C.f. case 7A.
	   ((progn
	      (goto-char (1+ containing-sexp))
	      (skip-chars-forward " \t")
	      (and (not (eolp))
		   (not (looking-at "\\\\$"))))
	    (goto-char containing-sexp)
	    (setq placeholder (c-point 'boi))
	    (if (and (c-safe (backward-up-list 1) t)
		     (> (point) placeholder))
		(progn
		  (forward-char)
		  (skip-chars-forward " \t"))
	      (goto-char placeholder))
	    (c-add-stmt-syntax 'arglist-cont-nonempty (list containing-sexp)
			       t nil
			       (c-most-enclosing-brace c-state-cache (point))
			       (c-whack-state-after (point) paren-state)))
	   ;; CASE 7G: we are looking at just a normal arglist
	   ;; continuation line
	   (t (c-forward-syntactic-ws indent-point)
	      (c-add-syntax 'arglist-cont (c-point 'boi)))
	   ))
	 ;; CASE 8: func-local multi-inheritance line
	 ((and (c-major-mode-is 'c++-mode)
	       (save-excursion
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (looking-at c-opt-postfix-decl-spec-key)))
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
	 ((and (not (c-mode-is-new-awk-p))  ; Maybe this isn't needed (ACM, 2002/3/29)
               (setq special-brace-list
                     (or (and c-special-brace-lists
                              (save-excursion
                                (goto-char containing-sexp)
                                (c-looking-at-special-brace-list)))
                         (c-inside-bracelist-p containing-sexp paren-state))))
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
		(setq c-syntactic-context placeholder)
	      (c-beginning-of-statement-1
	       (c-safe-position (1- containing-sexp) paren-state))
	      (c-forward-token-2 0)
	      (while (looking-at c-specifier-key)
		(goto-char (match-end 1))
		(c-forward-syntactic-ws))
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
			  (zerop (c-forward-token-2))
			  (eq (1+ (point)) (cdr (car special-brace-list)))))))
	      ;; Normal brace list check.
	      (and (eq char-after-ip ?})
		   (c-safe (goto-char (c-up-list-backward (point))) t)
		   (= (point) containing-sexp)))
	    (if (eq (point) (c-point 'boi))
		(c-add-syntax 'brace-list-close (point))
	      (setq lim (c-most-enclosing-brace c-state-cache (point)))
	      (c-beginning-of-statement-1 lim)
	      (c-add-stmt-syntax 'brace-list-close nil t t lim
				 (c-whack-state-after (point) paren-state))))
	   (t
	    ;; Prepare for the rest of the cases below by going to the
	    ;; token following the opening brace
	    (if (consp special-brace-list)
		(progn
		  (goto-char (car (car special-brace-list)))
		  (c-forward-token-2 1 nil indent-point))
	      (goto-char containing-sexp))
	    (forward-char)
	    (let ((start (point)))
	      (c-forward-syntactic-ws indent-point)
	      (goto-char (max start (c-point 'bol))))
	    (c-skip-ws-forward indent-point)
	    (cond
	     ;; CASE 9C: we're looking at the first line in a brace-list
	     ((= (point) indent-point)
	      (if (consp special-brace-list)
		  (goto-char (car (car special-brace-list)))
		(goto-char containing-sexp))
	      (if (eq (point) (c-point 'boi))
		  (c-add-syntax 'brace-list-intro (point))
		(setq lim (c-most-enclosing-brace c-state-cache (point)))
		(c-beginning-of-statement-1 lim)
		(c-add-stmt-syntax 'brace-list-intro nil t t lim
				   (c-whack-state-after (point) paren-state))))
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
		  ))
	     ))))
	 ;; CASE 10: A continued statement or top level construct.
	 ((and (if (c-mode-is-new-awk-p)
                   (c-awk-prev-line-incomplete-p containing-sexp) ; ACM 2002/3/29
                 (and (not (memq char-before-ip '(?\; ?:)))
                      (or (not (eq char-before-ip ?}))
                          (c-looking-at-inexpr-block-backward c-state-cache))))
	       (> (point)
		  (save-excursion
		    (c-beginning-of-statement-1 containing-sexp)
		    (setq placeholder (point))))
	       (/= placeholder containing-sexp))
	  ;; This is shared with case 18.
	  (c-guess-continued-construct indent-point
				       char-after-ip
				       placeholder
				       containing-sexp
				       paren-state))
	 ;; CASE 14: A case or default label
	 ((looking-at c-label-kwds-regexp)
	  (goto-char containing-sexp)
	  (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax 'case-label nil t nil
			     lim paren-state))
	 ;; CASE 15: any other label
	 ((looking-at c-label-key)
	  (goto-char containing-sexp)
	  (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	  (save-excursion
	    (setq tmpsymbol
		  (if (and (eq (c-beginning-of-statement-1 lim) 'up)
			   (looking-at "switch\\>[^_]"))
		      ;; If the surrounding statement is a switch then
		      ;; let's analyze all labels as switch labels, so
		      ;; that they get lined up consistently.
		      'case-label
		    'label)))
	  (c-backward-to-block-anchor lim)
	  (c-add-stmt-syntax tmpsymbol nil t nil
			     lim paren-state))
	 ;; CASE 16: block close brace, possibly closing the defun or
	 ;; the class
	 ((eq char-after-ip ?})
	  ;; From here on we have the next containing sexp in lim.
	  (setq lim (c-most-enclosing-brace paren-state))
	  (goto-char containing-sexp)
	    (cond
	     ;; CASE 16E: Closing a statement block?  This catches
	     ;; cases where it's preceded by a statement keyword,
	     ;; which works even when used in an "invalid" context,
	     ;; e.g. a macro argument.
	     ((c-after-conditional)
	      (c-backward-to-block-anchor lim)
	      (c-add-stmt-syntax 'block-close nil t nil
				 lim paren-state))
	     ;; CASE 16A: closing a lambda defun or an in-expression
	     ;; block?  C.f. cases 4, 7B and 17E.
	     ((setq placeholder (c-looking-at-inexpr-block
				 (c-safe-position containing-sexp paren-state)
				 nil))
	      (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				  'inline-close
				'block-close))
	      (goto-char containing-sexp)
	      (back-to-indentation)
	      (if (= containing-sexp (point))
		  (c-add-syntax tmpsymbol (point))
		(goto-char (cdr placeholder))
		(back-to-indentation)
		(c-add-stmt-syntax tmpsymbol nil t nil
				   (c-most-enclosing-brace paren-state (point))
				   (c-whack-state-after (point) paren-state))
		(if (/= (point) (cdr placeholder))
		    (c-add-syntax (car placeholder)))))
	     ;; CASE 16B: does this close an inline or a function in
	     ;; a non-class declaration level block?
	     ((setq placeholder (c-search-uplist-for-classkey paren-state))
	      (c-backward-to-decl-anchor lim)
	      (back-to-indentation)
	      (if (save-excursion
		    (goto-char (aref placeholder 0))
		    (looking-at c-other-decl-block-key))
		  (c-add-syntax 'defun-close (point))
		(c-add-syntax 'inline-close (point))))
	     ;; CASE 16F: Can be a defun-close of a function declared
	     ;; in a statement block, e.g. in Pike or when using gcc
	     ;; extensions.  Might also trigger it with some macros
	     ;; followed by blocks, and this gives sane indentation
	     ;; then too.  Let it through to be handled below.
	     ;; C.f. cases B.3 and 17G.
	     ((and (not inenclosing-p)
		   lim
		   (save-excursion
		     (and (not (c-looking-at-bos))
			  (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
			  (setq placeholder (point)))))
	      (back-to-indentation)
	      (if (/= (point) containing-sexp)
		  (goto-char placeholder))
	      (c-add-stmt-syntax 'defun-close nil t nil
				 lim paren-state))
	     ;; CASE 16C: if there an enclosing brace that hasn't
	     ;; been narrowed out by a class, then this is a
	     ;; block-close.  C.f. case 17H.
	     ((and (not inenclosing-p) lim)
	      ;; If the block is preceded by a case/switch label on
	      ;; the same line, we anchor at the first preceding label
	      ;; at boi.  The default handling in c-add-stmt-syntax is
	      ;; really fixes it better, but we do like this to keep
	      ;; the indentation compatible with version 5.28 and
	      ;; earlier.
	      (while (and (/= (setq placeholder (point)) (c-point 'boi))
			  (eq (c-beginning-of-statement-1 lim) 'label)))
	      (goto-char placeholder)
	      (if (looking-at c-label-kwds-regexp)
		  (c-add-syntax 'block-close (point))
		(goto-char containing-sexp)
		;; c-backward-to-block-anchor not necessary here; those
		;; situations are handled in case 16E above.
		(c-add-stmt-syntax 'block-close nil t nil
				   lim paren-state)))
	     ;; CASE 16D: find out whether we're closing a top-level
	     ;; class or a defun
	     (t
	      (save-restriction
		(narrow-to-region (point-min) indent-point)
		(let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		  (if decl
		      (c-add-class-syntax 'class-close decl paren-state)
		    (goto-char containing-sexp)
		    (c-backward-to-decl-anchor lim)
		    (back-to-indentation)
		    (c-add-syntax 'defun-close (point)))))
	      )))
	 ;; CASE 17: Statement or defun catchall.
	 (t
	  (goto-char indent-point)
	  ;; Back up statements until we find one that starts at boi.
	  (while (let* ((prev-point (point))
			(last-step-type (c-beginning-of-statement-1
					 containing-sexp)))
		   (if (= (point) prev-point)
		       (progn
			 (setq step-type (or step-type last-step-type))
			 nil)
		     (setq step-type last-step-type)
		     (/= (point) (c-point 'boi)))))
	  (cond
	   ;; CASE 17B: continued statement
	   ((and (eq step-type 'same)
		 (/= (point) indent-point))
	    (c-add-stmt-syntax 'statement-cont nil nil nil
			       containing-sexp paren-state))
	   ;; CASE 17A: After a case/default label?
	   ((progn
	      (while (and (eq step-type 'label)
			  (not (looking-at c-label-kwds-regexp)))
		(setq step-type
		      (c-beginning-of-statement-1 containing-sexp)))
	      (eq step-type 'label))
	    (c-add-stmt-syntax (if (eq char-after-ip ?{)
				   'statement-case-open
				 'statement-case-intro)
			       nil t nil containing-sexp paren-state))
	   ;; CASE 17D: any old statement
	   ((progn
	      (while (eq step-type 'label)
		(setq step-type
		      (c-beginning-of-statement-1 containing-sexp)))
	      (eq step-type 'previous))
	    (c-add-stmt-syntax 'statement nil t nil
			       containing-sexp paren-state)
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17I: Inside a substatement block.
	   ((progn
	      ;; The following tests are all based on containing-sexp.
	      (goto-char containing-sexp)
	      ;; From here on we have the next containing sexp in lim.
	      (setq lim (c-most-enclosing-brace paren-state containing-sexp))
	      (c-after-conditional))
	    (c-backward-to-block-anchor lim)
	    (c-add-stmt-syntax 'statement-block-intro nil t nil
			       lim paren-state)
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17E: first statement in an in-expression block.
	   ;; C.f. cases 4, 7B and 16A.
	   ((setq placeholder (c-looking-at-inexpr-block
			       (c-safe-position containing-sexp paren-state)
			       nil))
	    (setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				'defun-block-intro
			      'statement-block-intro))
	    (back-to-indentation)
	    (if (= containing-sexp (point))
		(c-add-syntax tmpsymbol (point))
	      (goto-char (cdr placeholder))
	      (back-to-indentation)
	      (c-add-stmt-syntax tmpsymbol nil t nil
				 (c-most-enclosing-brace c-state-cache (point))
				 (c-whack-state-after (point) paren-state))
	      (if (/= (point) (cdr placeholder))
		  (c-add-syntax (car placeholder))))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ;; CASE 17F: first statement in an inline, or first
	   ;; statement in a top-level defun. we can tell this is it
	   ;; if there are no enclosing braces that haven't been
	   ;; narrowed out by a class (i.e. don't use bod here).
	   ((save-excursion
	      (save-restriction
		(widen)
		(c-narrow-out-enclosing-class paren-state containing-sexp)
		(not (c-most-enclosing-brace paren-state))))
	    (c-backward-to-decl-anchor lim)
	    (back-to-indentation)
	    (c-add-syntax 'defun-block-intro (point)))
	   ;; CASE 17G: First statement in a function declared inside
	   ;; a normal block.  This can occur in Pike and with
	   ;; e.g. the gcc extensions.  Might also trigger it with
	   ;; some macros followed by blocks, and this gives sane
	   ;; indentation then too.  C.f. cases B.3 and 16F.
	   ((save-excursion
	      (and (not (c-looking-at-bos))
		   (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
		   (setq placeholder (point))))
	    (back-to-indentation)
	    (if (/= (point) containing-sexp)
		(goto-char placeholder))
	    (c-add-stmt-syntax 'defun-block-intro nil t nil
			       lim paren-state))
	   ;; CASE 17H: First statement in a block.  C.f. case 16C.
	   (t
	    ;; If the block is preceded by a case/switch label on the
	    ;; same line, we anchor at the first preceding label at
	    ;; boi.  The default handling in c-add-stmt-syntax is
	    ;; really fixes it better, but we do like this to keep the
	    ;; indentation compatible with version 5.28 and earlier.
	    (while (and (/= (setq placeholder (point)) (c-point 'boi))
			(eq (c-beginning-of-statement-1 lim) 'label)))
	    (goto-char placeholder)
	    (if (looking-at c-label-kwds-regexp)
		(c-add-syntax 'statement-block-intro (point))
	      (goto-char containing-sexp)
	      ;; c-backward-to-block-anchor not necessary here; those
	      ;; situations are handled in case 17I above.
	      (c-add-stmt-syntax 'statement-block-intro nil t nil
				 lim paren-state))
	    (if (eq char-after-ip ?{)
		(c-add-syntax 'block-open)))
	   ))
	 )
	;; now we need to look at any modifiers
	(goto-char indent-point)
	(skip-chars-forward " \t")
	;; are we looking at a comment only line?
	(when (and (looking-at c-comment-start-regexp)
		   (/= (c-forward-token-2 0 nil (c-point 'eol)) 0))
	  (c-append-syntax 'comment-intro))
	;; we might want to give additional offset to friends (in C++).
	(when (and c-opt-friend-key
		   (looking-at c-opt-friend-key))
	  (c-append-syntax 'friend))

	;; Set syntactic-relpos.
	(let ((p c-syntactic-context))
	  (while (and p
		      (if (integerp (car-safe (cdr-safe (car p))))
			  (progn
			    (setq syntactic-relpos (car (cdr (car p))))
			    nil)
			t))
	    (setq p (cdr p))))

	;; Start of or a continuation of a preprocessor directive?
	(if (and macro-start
		 (eq macro-start (c-point 'boi))
		 (not (and (c-major-mode-is 'pike-mode)
			   (eq (char-after (1+ macro-start)) ?\"))))
	    (c-append-syntax 'cpp-macro)
	  (when (and c-syntactic-indentation-in-macros macro-start)
	    (if in-macro-expr
		(when (or
		       (< syntactic-relpos macro-start)
		       (not (or
			     (assq 'arglist-intro c-syntactic-context)
			     (assq 'arglist-cont c-syntactic-context)
			     (assq 'arglist-cont-nonempty c-syntactic-context)
			     (assq 'arglist-close c-syntactic-context))))
		  ;; If inside a cpp expression, i.e. anywhere in a
		  ;; cpp directive except a #define body, we only let
		  ;; through the syntactic analysis that is internal
		  ;; in the expression.  That means the arglist
		  ;; elements, if they are anchored inside the cpp
		  ;; expression.
		  (setq c-syntactic-context nil)
		  (c-add-syntax 'cpp-macro-cont macro-start))
	      (when (and (eq macro-start syntactic-relpos)
			 (not (assq 'cpp-define-intro c-syntactic-context))
			 (save-excursion
			   (goto-char macro-start)
			   (or (not (c-forward-to-cpp-define-body))
			       (<= (point) (c-point 'boi indent-point)))))
		;; Inside a #define body and the syntactic analysis is
		;; anchored on the start of the #define.  In this case
		;; we add cpp-define-intro to get the extra
		;; indentation of the #define body.
		(c-add-syntax 'cpp-define-intro)))))
	;; return the syntax
	c-syntactic-context)))))
 ((>= (string-to-number c-version) 5.29)
  (defun c-guess-basic-syntax ()
    "Return the syntactic context of the current line."
    (save-excursion
	(save-restriction
	  (beginning-of-line)
	  (let* ((indent-point (point))
		 (case-fold-search nil)
		 (paren-state (c-parse-state))
		 literal containing-sexp char-before-ip char-after-ip lim
		 syntax placeholder c-in-literal-cache step-type
		 tmpsymbol keyword injava-inher special-brace-list
		 ;; narrow out any enclosing class or extern "C" block
		 (inclass-p (c-narrow-out-enclosing-class paren-state
							  indent-point))
		 ;; c-state-cache is shadowed here.  That means we must
		 ;; not do any changes during the execution of this
		 ;; function, since c-check-state-cache then would change
		 ;; this local variable and leave a bogus value in the
		 ;; global one.
		 (c-state-cache (if inclass-p
				    (c-whack-state-before (point-min) paren-state)
				  paren-state))
		 (c-state-cache-start (point-min))
		 inenclosing-p macro-start in-macro-expr
		 ;; There's always at most one syntactic element which got
		 ;; a relpos.  It's stored in syntactic-relpos.
		 syntactic-relpos
		 (c-stmt-delim-chars c-stmt-delim-chars))
	    ;; check for meta top-level enclosing constructs, possible
	    ;; extern language definitions, possibly (in C++) namespace
	    ;; definitions.
	    (save-excursion
	      (save-restriction
		(widen)
		(if (and inclass-p
			 (progn
			   (goto-char (aref inclass-p 0))
			   (looking-at c-other-decl-block-key)))
		    (let ((enclosing (match-string 1)))
		      (cond
		       ((string-equal enclosing "extern")
			(setq inenclosing-p 'extern))
		       ((string-equal enclosing "namespace")
			(setq inenclosing-p 'namespace))
		       )))))

	    ;; Init some position variables:
	    ;;
	    ;; containing-sexp is the open paren of the closest
	    ;; surrounding sexp or nil if there is none that hasn't been
	    ;; narrowed out.
	    ;;
	    ;; lim is the position after the closest preceding brace sexp
	    ;; (nested sexps are ignored), or the position after
	    ;; containing-sexp if there is none, or (point-min) if
	    ;; containing-sexp is nil.
	    ;;
	    ;; c-state-cache is the state from c-parse-state at
	    ;; indent-point, without any parens outside the region
	    ;; narrowed by c-narrow-out-enclosing-class.
	    ;;
	    ;; paren-state is the state from c-parse-state outside
	    ;; containing-sexp, or at indent-point if containing-sexp is
	    ;; nil.  paren-state is not limited to the narrowed region, as
	    ;; opposed to c-state-cache.
	    (if c-state-cache
		(progn
		  (setq containing-sexp (car paren-state)
			paren-state (cdr paren-state))
		  (if (consp containing-sexp)
		      (progn
			(setq lim (cdr containing-sexp))
			(if (cdr c-state-cache)
			    ;; Ignore balanced paren.  The next entry
			    ;; can't be another one.
			    (setq containing-sexp (car (cdr c-state-cache))
				  paren-state (cdr paren-state))
			  ;; If there is no surrounding open paren then
			  ;; put the last balanced pair back on paren-state.
			  (setq paren-state (cons containing-sexp paren-state)
				containing-sexp nil)))
		    (setq lim (1+ containing-sexp))))
	      (setq lim (point-min)))

	    ;; If we're in a parenthesis list then ',' delimits the
	    ;; "statements" rather than being an operator (with the
	    ;; exception of the "for" clause).  This difference is
	    ;; typically only noticeable when statements are used in macro
	    ;; arglists.
	    (when (and containing-sexp
		       (eq (char-after containing-sexp) ?\())
	      (setq c-stmt-delim-chars c-stmt-delim-chars-with-comma))

	    ;; cache char before and after indent point, and move point to
	    ;; the most likely position to perform the majority of tests
	    (goto-char indent-point)
	    (c-backward-syntactic-ws lim)
	    (setq char-before-ip (char-before))
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    (setq char-after-ip (char-after))

	    ;; are we in a literal?
	    (setq literal (c-in-literal lim))

	    ;; now figure out syntactic qualities of the current line
	    (cond
	     ;; CASE 1: in a string.
	     ((eq literal 'string)
	      (c-add-syntax 'string (c-point 'bopl)))
	     ;; CASE 2: in a C or C++ style comment.
	     ((memq literal '(c c++))
	      (c-add-syntax literal (car (c-literal-limits lim))))
	     ;; CASE 3: in a cpp preprocessor macro continuation.
	     ((and (save-excursion
		     (when (c-beginning-of-macro)
		       (setq macro-start (point))))
		   (/= macro-start (c-point 'boi))
		   (progn
		     (setq tmpsymbol 'cpp-macro-cont)
		     (or (not c-syntactic-indentation-in-macros)
			 (save-excursion
			   (goto-char macro-start)
			   ;; If at the beginning of the body of a #define
			   ;; directive then analyze as cpp-define-intro
			   ;; only.  Go on with the syntactic analysis
			   ;; otherwise.  in-macro-expr is set if we're in a
			   ;; cpp expression, i.e. before the #define body
			   ;; or anywhere in a non-#define directive.
			   (if (c-forward-to-cpp-define-body)
			       (let ((indent-boi (c-point 'boi indent-point)))
				 (setq in-macro-expr (> (point) indent-boi)
				       tmpsymbol 'cpp-define-intro)
				 (= (point) indent-boi))
			     (setq in-macro-expr t)
			     nil)))))
	      (c-add-syntax tmpsymbol macro-start)
	      (setq macro-start nil))
	     ;; CASE 11: an else clause?
	     ((looking-at "else\\>[^_]")
	      (c-beginning-of-statement-1 containing-sexp)
	      (c-add-stmt-syntax 'else-clause t containing-sexp paren-state))
	     ;; CASE 12: while closure of a do/while construct?
	     ((and (looking-at "while\\>[^_]")
		   (save-excursion
		     (prog1 (eq (c-beginning-of-statement-1 containing-sexp)
				'beginning)
		       (setq placeholder (point)))))
	      (goto-char placeholder)
	      (c-add-stmt-syntax 'do-while-closure t containing-sexp paren-state))
	     ;; CASE 13: A catch or finally clause?  This case is simpler
	     ;; than if-else and do-while, because a block is required
	     ;; after every try, catch and finally.
	     ((save-excursion
		(and (cond ((c-major-mode-is 'c++-mode)
			    (looking-at "catch\\>[^_]"))
			   ((c-major-mode-is 'java-mode)
			    (looking-at "\\(catch\\|finally\\)\\>[^_]")))
		     (and (c-safe (c-backward-syntactic-ws)
				  (c-backward-sexp)
				  t)
			  (eq (char-after) ?{)
			  (c-safe (c-backward-syntactic-ws)
				  (c-backward-sexp)
				  t)
			  (if (eq (char-after) ?\()
			      (c-safe (c-backward-sexp) t)
			    t))
		     (looking-at "\\(try\\|catch\\)\\>[^_]")
		     (setq placeholder (point))))
	      (goto-char placeholder)
	      (c-add-stmt-syntax 'catch-clause t containing-sexp paren-state))
	     ;; CASE 18: A substatement we can recognize by keyword.
	     ((save-excursion
		(and c-opt-block-stmt-key
		     (not (eq char-before-ip ?\;))
		     (not (memq char-after-ip '(?\) ?\] ?,)))
		     (or (not (eq char-before-ip ?}))
			 (c-looking-at-inexpr-block-backward c-state-cache))
		     (> (point)
			(progn
			  ;; Ought to cache the result from the
			  ;; c-beginning-of-statement-1 calls here.
			  (setq placeholder (point))
			  (while (eq (setq step-type
					   (c-beginning-of-statement-1 lim))
				     'label))
			  (if (eq step-type 'previous)
			      (goto-char placeholder)
			    (setq placeholder (point))
			    (if (and (eq step-type 'same)
				     (not (looking-at c-opt-block-stmt-key)))
				;; Step up to the containing statement if we
				;; stayed in the same one.
				(let (step)
				  (while (eq
					  (setq step
						(c-beginning-of-statement-1 lim))
					  'label))
				  (if (eq step 'up)
				      (setq placeholder (point))
				    ;; There was no containing statement afterall.
				    (goto-char placeholder)))))
			  placeholder))
		     (if (looking-at c-block-stmt-2-key)
			 ;; Require a parenthesis after these keywords.
			 ;; Necessary to catch e.g. synchronized in Java,
			 ;; which can be used both as statement and
			 ;; modifier.
			 (and (= (c-forward-token-1 1 nil) 0)
			      (eq (char-after) ?\())
		       (looking-at c-opt-block-stmt-key))))
	      (if (eq step-type 'up)
		  ;; CASE 18A: Simple substatement.
		  (progn
		    (goto-char placeholder)
		    (cond
		     ((eq char-after-ip ?{)
		      (c-add-stmt-syntax 'substatement-open nil
					 containing-sexp paren-state))
		     ((save-excursion
			(goto-char indent-point)
			(back-to-indentation)
			(looking-at c-label-key))
		      (c-add-stmt-syntax 'substatement-label nil
					 containing-sexp paren-state))
		     (t
		      (c-add-stmt-syntax 'substatement nil
					 containing-sexp paren-state))))
		;; CASE 18B: Some other substatement.  This is shared
		;; with case 10.
		(c-guess-continued-construct indent-point
					     char-after-ip
					     placeholder
					     lim
					     paren-state)))
	     ;; CASE 4: In-expression statement.  C.f. cases 7B, 16A and
	     ;; 17E.
	     ((and (or c-opt-inexpr-class-key
		       c-opt-inexpr-block-key
		       c-opt-lambda-key)
		   (setq placeholder (c-looking-at-inexpr-block
				      (c-safe-position containing-sexp paren-state)
				      containing-sexp)))
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
	      (c-add-stmt-syntax tmpsymbol t
				 (c-most-enclosing-brace c-state-cache (point))
				 (c-whack-state-after (point) paren-state))
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
			 (looking-at c-other-decl-block-key)
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
		    (let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		      (and decl
			   (setq placeholder (aref decl 0)))
		      ))
		  (c-add-syntax 'class-open placeholder))
		 ;; CASE 5A.3: brace list open
		 ((save-excursion
		    (c-beginning-of-decl-1 lim)
		    (if (looking-at "typedef\\>[^_]")
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
				 (looking-at "enum\\>[^_]"))
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
			(c-add-syntax 'topmost-intro-cont (c-point 'boi)))
		    (c-add-syntax 'brace-list-open placeholder)))
		 ;; CASE 5A.4: inline defun open
		 ((and inclass-p (not inenclosing-p))
		  (c-add-syntax 'inline-open)
		  (c-add-class-syntax 'inclass inclass-p paren-state))
		 ;; CASE 5A.5: ordinary defun open
		 (t
		  (goto-char placeholder)
		  (if (or inclass-p macro-start)
		      (c-add-syntax 'defun-open (c-point 'boi))
		    ;; Bogus to use bol here, but it's the legacy.
		    (c-add-syntax 'defun-open (c-point 'bol)))
		  )))
	       ;; CASE 5B: first K&R arg decl or member init
	       ((c-just-after-func-arglist-p nil lim)
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
		  (c-beginning-of-statement-1 lim)
		  (c-add-syntax 'knr-argdecl-intro (c-point 'boi))
		  (if inclass-p
		      (c-add-class-syntax 'inclass inclass-p paren-state)))
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
	       ((or (and (c-major-mode-is 'c++-mode)
			 (progn
			   (when (eq char-after-ip ?,)
			     (skip-chars-forward " \t")
			     (forward-char))
			   (looking-at c-opt-decl-spec-key)))
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
			       (cond ((looking-at c-opt-decl-spec-key)
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
		  (c-beginning-of-statement-1 lim)
		  (c-add-syntax 'inher-intro (c-point 'boi))
		  ;; don't add inclass symbol since relative point already
		  ;; contains any class offset
		  )
		 ;; CASE 5C.2: hanging colon on an inher intro
		 ((eq char-before-ip ?:)
		  (c-beginning-of-statement-1 lim)
		  (c-add-syntax 'inher-intro (c-point 'boi))
		  (if inclass-p
		      (c-add-class-syntax 'inclass inclass-p paren-state)))
		 ;; KDE Hack Start:
		 ((and inclass-p
		       c-access-key
		       (looking-at c-access-key))
		  (c-add-syntax 'access-label (c-point 'bonl))
		  (setq placeholder (c-add-class-syntax 'inclass inclass-p
							paren-state)))
		 ;;(nconc syntax (list (cons 'access-label placeholder))))
		 ;; KDE Hack End.
		 ;; CASE 5C.3: in a Java implements/extends
		 (injava-inher
		  (let ((where (cdr injava-inher))
			(cont (car injava-inher)))
		    (goto-char where)
		    (cond ((looking-at "throws\\>[^_]")
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
	       ;; CASE 5D: this could be a top-level initialization, a
	       ;; member init list continuation, or a template argument
	       ;; list continuation.
	       ((c-with-syntax-table (if (c-major-mode-is 'c++-mode)
					 c++-template-syntax-table
				       (syntax-table))
		  (save-excursion
		    ;; Note: We use the fact that lim is always after any
		    ;; preceding brace sexp.
		    (while (and (= (c-backward-token-1 1 t lim) 0)
				(not (looking-at "[;<,=]"))))
		    (or (memq (char-after) '(?, ?=))
			(and (c-major-mode-is 'c++-mode)
			     (= (c-backward-token-1 1 nil lim) 0)
			     (eq (char-after) ?<)))))
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
			   (if c-opt-access-key
			       (not (looking-at c-opt-access-key)) t)
			   (not (looking-at c-class-key))
			   (if c-opt-bitfield-key
			       (not (looking-at c-opt-bitfield-key)) t))
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
		 ;; CASE 5D.3: perhaps a template list continuation?
		 ((and (c-major-mode-is 'c++-mode)
		       (save-excursion
			 (save-restriction
			   (c-with-syntax-table c++-template-syntax-table
			     (goto-char indent-point)
			     (setq placeholder (c-up-list-backward (point)))
			     (and placeholder
				  (eq (char-after placeholder) ?<))))))
		  ;; we can probably indent it just like an arglist-cont
		  (goto-char placeholder)
		  (c-beginning-of-statement-1 lim t)
		  (c-add-syntax 'template-args-cont (c-point 'boi)))
		 ;; CASE 5D.4: perhaps a multiple inheritance line?
		 ((and (c-major-mode-is 'c++-mode)
		       (save-excursion
			 (c-beginning-of-statement-1 lim)
			 (setq placeholder (point))
			 (if (looking-at "static\\>[^_]")
			     (c-forward-token-1 1 nil indent-point))
			 (and (looking-at c-class-key)
			      (= (c-forward-token-1 2 nil indent-point) 0)
			      (if (eq (char-after) ?<)
				  (c-with-syntax-table c++-template-syntax-table
				    (= (c-forward-token-1 1 t indent-point) 0))
				t)
			      (eq (char-after) ?:))))
		  (goto-char placeholder)
		  (c-add-syntax 'inher-cont (c-point 'boi)))
		 ;; CASE 5D.5: Continuation of the "expression part" of a
		 ;; top level construct.
		 (t
		  (while (and (eq (car (c-beginning-of-decl-1 containing-sexp))
				  'same)
			      (save-excursion
				(c-backward-syntactic-ws)
				(eq (char-before) ?}))))
		  (c-add-stmt-syntax
		   (if (eq char-before-ip ?,)
		       ;; A preceding comma at the top level means that a
		       ;; new variable declaration starts here.  Use
		       ;; topmost-intro-cont for it, for consistency with
		       ;; the first variable declaration.  C.f. case 5N.
		       'topmost-intro-cont
		     'statement-cont)
		   nil containing-sexp paren-state))
		 ))
	       ;; CASE 5E: we are looking at a access specifier
	       ((and inclass-p
		     c-opt-access-key
		     (looking-at c-opt-access-key))
		(setq placeholder (c-add-class-syntax 'inclass inclass-p
						      paren-state))
		;; Append access-label with the same anchor point as inclass gets.
		(nconc syntax (list (cons 'access-label placeholder))))
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
		(c-add-class-syntax 'class-close inclass-p paren-state))
	       ;; CASE 5H: we could be looking at subsequent knr-argdecls
	       ((and c-recognize-knr-p
		     (not (eq char-before-ip ?}))
		     (save-excursion
		       (setq placeholder (cdr (c-beginning-of-decl-1 lim)))
		       (and placeholder
			    ;; Do an extra check to avoid tripping up on
			    ;; statements that occur in invalid contexts
			    ;; (e.g. in macro bodies where we don't really
			    ;; know the context of what we're looking at).
			    (not (and c-opt-block-stmt-key
				      (looking-at c-opt-block-stmt-key)))))
		     (< placeholder indent-point))
		(goto-char placeholder)
		(c-add-syntax 'knr-argdecl (point)))
	       ;; CASE 5I: ObjC method definition.
	       ((and c-opt-method-key
		     (looking-at c-opt-method-key))
		(c-beginning-of-statement-1 lim)
		(c-add-syntax 'objc-method-intro (c-point 'boi)))
	       ;; CASE 5N: At a variable declaration that follows a class
	       ;; definition or some other block declaration that doesn't
	       ;; end at the closing '}'.  C.f. case 5D.5.
	       ((progn
		  (c-backward-syntactic-ws lim)
		  (and (eq (char-before) ?})
		       (save-excursion
			 (let ((start (point)))
			   (if paren-state
			       ;; Speed up the backward search a bit.
			       (goto-char (car (car paren-state))))
			   (c-beginning-of-decl-1 containing-sexp)
			   (setq placeholder (point))
			   (if (= start (point))
			       ;; The '}' is unbalanced.
			       nil
			     (c-end-of-decl-1)
			     (> (point) indent-point))))))
		(goto-char placeholder)
		(c-add-stmt-syntax 'topmost-intro-cont nil
				   containing-sexp paren-state))
	       ;; CASE 5J: we are at the topmost level, make
	       ;; sure we skip back past any access specifiers
	       ((progn
		  (while (and inclass-p
			      c-opt-access-key
			      (not (bobp))
			      (save-excursion
				(c-safe (progn (c-backward-sexp 1) t))
				(and (looking-at "slots:")
				   (c-backward-sexp 1))
				(looking-at c-opt-access-key)))
		    (c-backward-sexp 1)
		    (c-backward-syntactic-ws lim))
		  (or (bobp)
		      (memq (char-before) '(?\; ?}))
		      (and (c-major-mode-is 'objc-mode)
			   (progn
			     (c-beginning-of-statement-1 lim)
			     (eq (char-after) ?@)))))
		;; real beginning-of-line could be narrowed out due to
		;; enclosure in a class block
		(save-restriction
		  (widen)
		  (c-add-syntax 'topmost-intro (c-point 'bol))
		  ;; Using bol instead of boi above is highly bogus, and
		  ;; it makes our lives hard to remain compatible. :P
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
			 (t (c-add-class-syntax 'inclass inclass-p paren-state)))
			))
		  (when (and c-syntactic-indentation-in-macros
			     macro-start
			     (/= macro-start (c-point 'boi indent-point)))
		    (c-add-syntax 'cpp-define-intro)
		    (setq macro-start nil))
		  ))
	       ;; CASE 5K: we are at an ObjC method definition
	       ;; continuation line.
	       ((and c-opt-method-key
		     (progn
		       (c-beginning-of-statement-1 lim)
		       (beginning-of-line)
		       (looking-at c-opt-method-key)))
		(c-add-syntax 'objc-method-args-cont (point)))
	       ;; CASE 5L: we are at the first argument of a template
	       ;; arglist that begins on the previous line.
	       ((eq (char-before) ?<)
		(c-beginning-of-statement-1 (c-safe-position (point) paren-state))
		(c-add-syntax 'template-args-cont (c-point 'boi)))
	       ;; CASE 5M: we are at a topmost continuation line
	       ;; KDE Hack 2
	       ;; NOTE: is there a way to detect these sooner ? 
	       (t
		(c-beginning-of-statement-1 (c-safe-position (point) paren-state))
		(if (re-search-forward c-access-key (point-at-eol) t)
		    (progn
		      (c-add-syntax 'topmost-intro (c-point 'bol))
		      (c-add-class-syntax 'inclass inclass-p paren-state)
		      )
		  (progn
		    (c-add-syntax 'topmost-intro-cont (c-point 'boi))
		    ))
		)
	       ))
	     ;; (CASE 6 has been removed.)
	     ;; CASE 7: line is an expression, not a statement.  Most
	     ;; likely we are either in a function prototype or a function
	     ;; call argument list
	     ((not (or (and c-special-brace-lists
			    (save-excursion
			      (goto-char containing-sexp)
			      (c-looking-at-special-brace-list)))
		       (eq (char-after containing-sexp) ?{)))
	      (cond
	       ;; CASE 7A: we are looking at the arglist closing paren
	       ((memq char-after-ip '(?\) ?\]))
		(goto-char containing-sexp)
		(setq placeholder (c-point 'boi))
		(when (and (c-safe (backward-up-list 1) t)
			   (> (point) placeholder))
		  (forward-char)
		  (skip-chars-forward " \t")
		  (setq placeholder (point)))
		(c-add-syntax 'arglist-close placeholder))
	       ;; CASE 7B: Looking at the opening brace of an
	       ;; in-expression block or brace list.  C.f. cases 4, 16A
	       ;; and 17E.
	       ((and (eq char-after-ip ?{)
		     (progn
		       (setq placeholder (c-inside-bracelist-p (point)
							       c-state-cache))
		       (if placeholder
			   (setq tmpsymbol '(brace-list-open . inexpr-class))
			 (setq tmpsymbol '(block-open . inexpr-statement)
			       placeholder
			       (cdr-safe (c-looking-at-inexpr-block
					  (c-safe-position containing-sexp
							   paren-state)
					  containing-sexp)))
			 ;; placeholder is nil if it's a block directly in
			 ;; a function arglist.  That makes us skip out of
			 ;; this case.
			 )))
		(goto-char placeholder)
		(back-to-indentation)
		(c-add-stmt-syntax (car tmpsymbol) t
				   (c-most-enclosing-brace paren-state (point))
				   (c-whack-state-after (point) paren-state))
		(if (/= (point) placeholder)
		    (c-add-syntax (cdr tmpsymbol))))
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
	       ((progn
		  (goto-char containing-sexp)
		  (and (c-safe (progn (c-forward-sexp -1) t))
		       (looking-at "\\<for\\>[^_]")))
		(goto-char (1+ containing-sexp))
		(c-forward-syntactic-ws indent-point)
		(if (eq char-before-ip ?\;)
		    (c-add-syntax 'statement (point))
		  (c-add-syntax 'statement-cont (point))
		  ))
	       ;; CASE 7E: maybe a continued ObjC method call. This is the
	       ;; case when we are inside a [] bracketed exp, and what
	       ;; precede the opening bracket is not an identifier.
	       ((and c-opt-method-key
		     (eq (char-after containing-sexp) ?\[)
		     (progn
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
	       ((progn
		  (goto-char (1+ containing-sexp))
		  (skip-chars-forward " \t")
		  (and (not (eolp))
		       (not (looking-at "\\\\$"))))
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
	       (t (c-forward-syntactic-ws indent-point)
		  (c-add-syntax 'arglist-cont (c-point 'boi)))
	       ))
	     ;; CASE 8: func-local multi-inheritance line
	     ((and (c-major-mode-is 'c++-mode)
		   (save-excursion
		     (goto-char indent-point)
		     (skip-chars-forward " \t")
		     (looking-at c-opt-decl-spec-key)))
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
			(c-inside-bracelist-p containing-sexp paren-state)))
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
		  (c-beginning-of-statement-1
		   (c-safe-position (1- containing-sexp) paren-state))
		  (c-forward-token-1 0)
		  (if (looking-at "typedef\\>[^_]") (c-forward-token-1 1))
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
		       (c-safe (progn (goto-char (c-up-list-backward (point)))
				      t))
		       (= (point) containing-sexp)))
		(if (eq (point) (c-point 'boi))
		    (c-add-syntax 'brace-list-close (point))
		  (setq lim (c-most-enclosing-brace c-state-cache (point)))
		  (c-beginning-of-statement-1 lim)
		  (c-add-stmt-syntax 'brace-list-close t lim
				     (c-whack-state-after (point) paren-state)
				     t)))
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
		(c-skip-ws-forward indent-point)
		(cond
		 ;; CASE 9C: we're looking at the first line in a brace-list
		 ((= (point) indent-point)
		  (if (consp special-brace-list)
		      (goto-char (car (car special-brace-list)))
		    (goto-char containing-sexp))
		  (if (eq (point) (c-point 'boi))
		      (c-add-syntax 'brace-list-intro (point))
		    (setq lim (c-most-enclosing-brace c-state-cache (point)))
		    (c-beginning-of-statement-1 lim)
		    (c-add-stmt-syntax 'brace-list-intro t lim
				       (c-whack-state-after (point) paren-state)
				       t)))
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
		      ))
		 ))))
	     ;; CASE 10: A continued statement or top level construct.
	     ((and (not (memq char-before-ip '(?\; ?:)))
		   (or (not (eq char-before-ip ?}))
		       (c-looking-at-inexpr-block-backward c-state-cache))
		   (> (point)
		      (save-excursion
			(c-beginning-of-statement-1 containing-sexp)
			(setq placeholder (point))))
		   (/= placeholder containing-sexp))
	      ;; This is shared with case 18.
	      (c-guess-continued-construct indent-point
					   char-after-ip
					   placeholder
					   containing-sexp
					   paren-state))
	     ;; CASE 14: A case or default label
	     ((looking-at c-label-kwds-regexp)
	      (goto-char containing-sexp)
	      (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	      (c-backward-to-block-anchor lim)
	      (c-add-stmt-syntax 'case-label t lim paren-state))
	     ;; CASE 15: any other label
	     ((looking-at c-label-key)
	      (goto-char containing-sexp)
	      (setq lim (c-most-enclosing-brace c-state-cache containing-sexp))
	      (save-excursion
		(setq tmpsymbol
		      (if (and (eq (c-beginning-of-statement-1 lim) 'up)
			       (looking-at "switch\\>[^_]"))
			  ;; If the surrounding statement is a switch then
			  ;; let's analyze all labels as switch labels, so
			  ;; that they get lined up consistently.
			  'case-label
			'label)))
	      (c-backward-to-block-anchor lim)
	      (c-add-stmt-syntax tmpsymbol t lim paren-state))
	     ;; CASE 16: block close brace, possibly closing the defun or
	     ;; the class
	     ((eq char-after-ip ?})
	      ;; From here on we have the next containing sexp in lim.
	      (setq lim (c-most-enclosing-brace paren-state))
	      (goto-char containing-sexp)
	      (cond
	       ;; CASE 16E: Closing a statement block?  This catches
	       ;; cases where it's preceded by a statement keyword,
	       ;; which works even when used in an "invalid" context,
	       ;; e.g. a macro argument.
	       ((c-after-conditional)
		(c-backward-to-block-anchor lim)
		(c-add-stmt-syntax 'block-close t lim paren-state))
	       ;; CASE 16A: closing a lambda defun or an in-expression
	       ;; block?  C.f. cases 4, 7B and 17E.
	       ((setq placeholder (c-looking-at-inexpr-block
				   (c-safe-position containing-sexp paren-state)
				   nil))
		(setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				    'inline-close
				  'block-close))
		(goto-char containing-sexp)
		(back-to-indentation)
		(if (= containing-sexp (point))
		    (c-add-syntax tmpsymbol (point))
		  (goto-char (cdr placeholder))
		  (back-to-indentation)
		  (c-add-stmt-syntax tmpsymbol t
				     (c-most-enclosing-brace paren-state (point))
				     (c-whack-state-after (point) paren-state))
		  (if (/= (point) (cdr placeholder))
		      (c-add-syntax (car placeholder)))))
	       ;; CASE 16B: does this close an inline or a function in
	       ;; an extern block or namespace?
	       ((setq placeholder (c-search-uplist-for-classkey paren-state))
		(c-backward-to-decl-anchor lim)
		(back-to-indentation)
		(if (save-excursion
		      (goto-char (aref placeholder 0))
		      (looking-at c-other-decl-block-key))
		    (c-add-syntax 'defun-close (point))
		  (c-add-syntax 'inline-close (point))))
	       ;; CASE 16F: Can be a defun-close of a function declared
	       ;; in a statement block, e.g. in Pike or when using gcc
	       ;; extensions.  Might also trigger it with some macros
	       ;; followed by blocks, and this gives sane indentation
	       ;; then too.  Let it through to be handled below.
	       ;; C.f. cases B.3 and 17G.
	       ((and (not inenclosing-p)
		     lim
		     (save-excursion
		       (and (not (c-looking-at-bos))
			    (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
			    (setq placeholder (point)))))
		(back-to-indentation)
		(if (/= (point) containing-sexp)
		    (goto-char placeholder))
		(c-add-stmt-syntax 'defun-close t lim paren-state))
	       ;; CASE 16C: if there an enclosing brace that hasn't
	       ;; been narrowed out by a class, then this is a
	       ;; block-close.  C.f. case 17H.
	       ((and (not inenclosing-p) lim)
		;; If the block is preceded by a case/switch label on
		;; the same line, we anchor at the first preceding label
		;; at boi.  The default handling in c-add-stmt-syntax is
		;; really fixes it better, but we do like this to keep
		;; the indentation compatible with version 5.28 and
		;; earlier.
		(while (and (/= (setq placeholder (point)) (c-point 'boi))
			    (eq (c-beginning-of-statement-1 lim) 'label)))
		(goto-char placeholder)
		(if (looking-at c-label-kwds-regexp)
		    (c-add-syntax 'block-close (point))
		  (goto-char containing-sexp)
		  ;; c-backward-to-block-anchor not necessary here; those
		  ;; situations are handled in case 16E above.
		  (c-add-stmt-syntax 'block-close t lim paren-state)))
	       ;; CASE 16D: find out whether we're closing a top-level
	       ;; class or a defun
	       (t
		(save-restriction
		  (narrow-to-region (point-min) indent-point)
		  (let ((decl (c-search-uplist-for-classkey (c-parse-state))))
		    (if decl
			(c-add-class-syntax 'class-close decl paren-state)
		      (goto-char containing-sexp)
		      (c-backward-to-decl-anchor lim)
		      (back-to-indentation)
		      (c-add-syntax 'defun-close (point)))))
		)))
	     ;; CASE 17: Statement or defun catchall.
	     (t
	      (goto-char indent-point)
	      ;; Back up statements until we find one that starts at boi.
	      (while (let* ((prev-point (point))
			    (last-step-type (c-beginning-of-statement-1
					     containing-sexp)))
		       (if (= (point) prev-point)
			   (progn
			     (setq step-type (or step-type last-step-type))
			     nil)
			 (setq step-type last-step-type)
			 (/= (point) (c-point 'boi)))))
	      (cond
	       ;; CASE 17B: continued statement
	       ((and (eq step-type 'same)
		     (/= (point) indent-point))
		(c-add-stmt-syntax 'statement-cont nil
				   containing-sexp paren-state))
	       ;; CASE 17A: After a case/default label?
	       ((progn
		  (while (and (eq step-type 'label)
			      (not (looking-at c-label-kwds-regexp)))
		    (setq step-type
			  (c-beginning-of-statement-1 containing-sexp)))
		  (eq step-type 'label))
		(c-add-stmt-syntax (if (eq char-after-ip ?{)
				       'statement-case-open
				     'statement-case-intro)
				   t containing-sexp paren-state))
	       ;; CASE 17D: any old statement
	       ((progn
		  (while (eq step-type 'label)
		    (setq step-type
			  (c-beginning-of-statement-1 containing-sexp)))
		  (eq step-type 'previous))
		(c-add-stmt-syntax 'statement t containing-sexp paren-state)
		(if (eq char-after-ip ?{)
		    (c-add-syntax 'block-open)))
	       ;; CASE 17I: Inside a substatement block.
	       ((progn
		  ;; The following tests are all based on containing-sexp.
		  (goto-char containing-sexp)
		  ;; From here on we have the next containing sexp in lim.
		  (setq lim (c-most-enclosing-brace paren-state containing-sexp))
		  (c-after-conditional))
		(c-backward-to-block-anchor lim)
		(c-add-stmt-syntax 'statement-block-intro t lim paren-state)
		(if (eq char-after-ip ?{)
		    (c-add-syntax 'block-open)))
	       ;; CASE 17E: first statement in an in-expression block.
	       ;; C.f. cases 4, 7B and 16A.
	       ((setq placeholder (c-looking-at-inexpr-block
				   (c-safe-position containing-sexp paren-state)
				   nil))
		(setq tmpsymbol (if (eq (car placeholder) 'inlambda)
				    'defun-block-intro
				  'statement-block-intro))
		(back-to-indentation)
		(if (= containing-sexp (point))
		    (c-add-syntax tmpsymbol (point))
		  (goto-char (cdr placeholder))
		  (back-to-indentation)
		  (c-add-stmt-syntax tmpsymbol t
				     (c-most-enclosing-brace c-state-cache (point))
				     (c-whack-state-after (point) paren-state))
		  (if (/= (point) (cdr placeholder))
		      (c-add-syntax (car placeholder))))
		(if (eq char-after-ip ?{)
		    (c-add-syntax 'block-open)))
	       ;; CASE 17F: first statement in an inline, or first
	       ;; statement in a top-level defun. we can tell this is it
	       ;; if there are no enclosing braces that haven't been
	       ;; narrowed out by a class (i.e. don't use bod here).
	       ;; However, we first check for statements that we can
	       ;; recognize by keywords.  That increases the robustness in
	       ;; cases where statements are used on the top level,
	       ;; e.g. in macro definitions.
	       ((save-excursion
		  (save-restriction
		    (widen)
		    (c-narrow-out-enclosing-class paren-state containing-sexp)
		    (not (c-most-enclosing-brace paren-state))))
		(c-backward-to-decl-anchor lim)
		(back-to-indentation)
		(c-add-syntax 'defun-block-intro (point)))
	       ;; CASE 17G: First statement in a function declared inside
	       ;; a normal block.  This can occur in Pike and with
	       ;; e.g. the gcc extensions.  Might also trigger it with
	       ;; some macros followed by blocks, and this gives sane
	       ;; indentation then too.  C.f. cases B.3 and 16F.
	       ((save-excursion
		  (and (not (c-looking-at-bos))
		       (eq (c-beginning-of-statement-1 lim nil nil t) 'same)
		       (setq placeholder (point))))
		(back-to-indentation)
		(if (/= (point) containing-sexp)
		    (goto-char placeholder))
		(c-add-stmt-syntax 'defun-block-intro t lim paren-state))
	       ;; CASE 17H: First statement in a block.  C.f. case 16C.
	       (t
		;; If the block is preceded by a case/switch label on the
		;; same line, we anchor at the first preceding label at
		;; boi.  The default handling in c-add-stmt-syntax is
		;; really fixes it better, but we do like this to keep the
		;; indentation compatible with version 5.28 and earlier.
		(while (and (/= (setq placeholder (point)) (c-point 'boi))
			    (eq (c-beginning-of-statement-1 lim) 'label)))
		(goto-char placeholder)
		(if (looking-at c-label-kwds-regexp)
		    (c-add-syntax 'statement-block-intro (point))
		  (goto-char containing-sexp)
		  ;; c-backward-to-block-anchor not necessary here; those
		  ;; situations are handled in case 17I above.
		  (c-add-stmt-syntax 'statement-block-intro t lim paren-state))
		(if (eq char-after-ip ?{)
		    (c-add-syntax 'block-open)))
	       ))
	     )
	    ;; now we need to look at any modifiers
	    (goto-char indent-point)
	    (skip-chars-forward " \t")
	    ;; are we looking at a comment only line?
	    (when (and (looking-at c-comment-start-regexp)
		       (/= (c-forward-token-1 0 nil (c-point 'eol)) 0))
	      (c-add-syntax 'comment-intro))
	    ;; we might want to give additional offset to friends (in C++).
	    (when (and c-opt-friend-key
		       (looking-at c-opt-friend-key))
	      (c-add-syntax 'friend))
	    ;; Start of or a continuation of a preprocessor directive?
	    (if (and macro-start
		     (eq macro-start (c-point 'boi))
		     (not (and (c-major-mode-is 'pike-mode)
			       (eq (char-after (1+ macro-start)) ?\"))))
		(c-add-syntax 'cpp-macro)
	      (when (and c-syntactic-indentation-in-macros macro-start)
		(if in-macro-expr
		    (when (or (< syntactic-relpos macro-start)
			      (not (or (assq 'arglist-intro syntax)
				       (assq 'arglist-cont syntax)
				       (assq 'arglist-cont-nonempty syntax)
				       (assq 'arglist-close syntax))))
		      ;; If inside a cpp expression, i.e. anywhere in a
		      ;; cpp directive except a #define body, we only let
		      ;; through the syntactic analysis that is internal
		      ;; in the expression.  That means the arglist
		      ;; elements, if they are anchored inside the cpp
		      ;; expression.
		      (setq syntax `((cpp-macro-cont . ,macro-start))))
		  (when (and (eq macro-start syntactic-relpos)
			     (not (assq 'cpp-define-intro syntax))
			     (save-excursion
			       (goto-char macro-start)
			       (or (not (c-forward-to-cpp-define-body))
				   (<= (point) (c-point 'boi indent-point)))))
		    ;; Inside a #define body and the syntactic analysis is
		    ;; anchored on the start of the #define.  In this case
		    ;; we add cpp-define-intro to get the extra
		    ;; indentation of the #define body.
		    (c-add-syntax 'cpp-define-intro)))))
	    ;; return the syntax
	    syntax)))))
  ((defun c-guess-basic-syntax ()
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
	  ;; (imenu "agulbra-c++-tab")
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
		(c-add-class-syntax 'inclass inclass-p)
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
		       (setq plaaceholder (point))
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
		)			; end CASE 9C
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
	  syntax))))))

(add-hook 'find-file-hooks 'agulbra-c++-clean-out-spaces)
(add-hook 'write-file-hooks 'agulbra-c++-clean-out-spaces)

(add-hook 'c++-mode-hook 'kde-c++-mode-hook)
(add-hook 'c-mode-hook 'kde-c-mode-hook)
; always end a file with a newline
(setq-default require-final-newline t)
; 'next-line won't be adding newlines
(setq-default next-line-add-newlines nil)
(setq compilation-error-regexp-systems-list '(gnu of comma 4bsd)
      compilation-ask-about-save nil)

(provide 'kde-emacs-core)
