;; kde-emacs-semantic.el
;;
;; Copyright (C)  2002  Zack Rusin <zack@kde.org>
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

;;; Commentary:
;; Package provides four interactive functions:
;; - kde-function-doc-insert - creates a skeleton doxygen
;;                           documentation for function at point.
;;                           Customize it with kde-func-doc variables.
;;
;; - kde-function-expanded-at-point - returns t if function at point
;;                           has already been expanded.
;;
;; - kde-function-expand-at-point - expand (creates a stub) for function
;;                           at point (as long as function is a prototype
;;                           and haven't been expanded).
;;
;; - kde-create-skeletons - creates stubs for all methods in the current 
;;                       header file.
;; 
;; Package is very flexible, look at defcustom variables for things 
;; you can customize.

;;; Problems:
;; Most problems relate to C++ syntax which isn't handled correctly
;; by the Semantic package. For now templates aren't supported, I 
;; have a temporary solution for other problems (e.g. const functions,
;; QT/KDE specific access specifiers)

;;; Code:
(require 'kde-emacs-vars)
(require 'kde-emacs-general)

;*---------------------------------------------------------------------*/
;*    User configuration ...                                           */
;*---------------------------------------------------------------------*/
;;Not yet, not yet
;(defcustom kde-summary-function 'semantic-uml-prototype-nonterminal
;  "*Function to use when showing info about the token"
;  :group 'kde-devel
;  :type semantic-token->text-custom-list
;  )

(defcustom kde-generate-docs-with-stubs nil
  "*Should function documentation be generated with the stubs."
  :group 'kde-devel
  :type 'boolean)

(defcustom kde-expand-arg-start "( "
  "*A string which specifies how the function arguments format should start.
e.g. \"( \" would start function arguments list like : \"func( int arg\".
and \" (\" will format the begining of the function argument list as 
follows : \"func (int arg\"."
  :group 'kde-devel
  :version "0.1"
  :type 'string)

(defcustom kde-expand-arg-end " )"
  "*Just like kde-expand-arg-start but specifies how the list should end."
  :group 'kde-devel
  :version "0.1"
  :type 'string)

(defcustom kde-expand-arg-break ", "
  "*Specifies how the arguments should be separated."
  :group 'kde-devel
  :version "0.1"
  :type 'string)

  
;*---------------------------------------------------------------------*/
;*    Functions  ...                                                   */
;*---------------------------------------------------------------------*/
;; FIXME : semantic doesn't handle QT access specifiers
;(setq-default global-semantic-show-unmatched-syntax-mode nil)
;(setq-default global-semantic-show-dirty-mode nil)

(defun kde-format-func-arg (arg)
  "Formats one argument (from token to string)."
  (let ((ret ""))
    (if (semantic-token-variable-extra-spec arg 'const)
      (setq ret "const "))
    (setq ret (concat ret (car (semantic-token-type arg))))
    (if (semantic-token-variable-extra-spec arg 'pointer)
	(dotimes (idx (semantic-token-variable-extra-spec arg 'pointer))
	  (setq ret (concat ret "*"))
	  )
      )
    (if (semantic-token-variable-extra-spec arg 'reference)
	(setq ret (concat ret "&"))
      )
    (setq ret (concat ret " " (semantic-token-name arg)))
    ret
    ))

(defun kde-format-args (token)
  "Formats all arguments from token to string.
Token has to be the function variable list e.g.
from semantic-token-function-args"
  (let ((res kde-expand-arg-start) (idx 1))
    (dolist (elt token res)
      (setq res (concat res (kde-format-func-arg elt)))
      (when (< idx (length token))
	(setq res (concat res kde-expand-arg-break)))
      (setq idx (1+ idx))
      )
    (setq res (concat res kde-expand-arg-end))
    ;; if it's something like "(   )" replace it with "()"
    (when (string= res (concat kde-expand-arg-start kde-expand-arg-end))
      (setq res (replace-regexp-in-string "([ \t]+)" "()" res)))
    res
    ))

(defun kde-function-in-tokens (FUNC TOKENS)
  "Search for function in tokens. FUNC has to be a function
token and TOKENS have to be a list of functions from buffer."
  (let ((ret)(elt))
    (while (and TOKENS (not ret))
      (setq elt (car TOKENS))
      (setq TOKENS (cdr TOKENS))
      (if (and (string= (semantic-token-name FUNC)
			(semantic-token-name elt))
	       (equal (semantic-token-type FUNC) 
		      (semantic-token-type elt))
	       ;; FIXME (semantic) : Functions in some classes don't have the
	       ;;                    'parent property set !!!
	       ;;(string= (semantic-token-function-parent FUNC1)    
	       ;;	    (semantic-token-function-parent FUNC2)) 
	       (string= (kde-format-args (semantic-token-function-args FUNC))
			(kde-format-args (semantic-token-function-args elt))))
	  (setq ret t))
      )
    ret
    ))

;; TODO support Q_SIGNALS too
(defmacro kde-label-signals (pt)
  "Returns none-nil if the current access label == \"signals\""
  `(save-excursion
     (goto-char ,pt)
     (if (looking-at ":")
	 (re-search-backward "signals" (point-at-bol) t)
       )
     ))

(defun kde-label-namespace (pt)
  "Return the namespace to which the variable/function at point PT belongs to."
  (save-excursion
    (goto-char pt)
    (if (looking-at "::")
	(let ((start) (end))
	  (re-search-backward "\\b\\w+" (point-at-bol) t)
	  (setq start (match-beginning 0))
	  (setq end   (match-end 0))
	  (buffer-substring-no-properties start end)
	  )
      )
    ))

(defmacro kde-label-slots (pt)
  "Return none-nil if at PT there's slots access specifier."
  `(save-excursion
    (goto-char ,pt)
    (if (looking-at ":")
	;; export this regex to a kde-emacs-vars defvar
	(re-search-backward "\\(public\\|protected\\|private\\)[ \t]+\\(slots\\|Q_SLOTS\\)" (point-at-bol) t))
    ))

(defmacro kde-is-constructor (function)
  "Returns t if the FUNCTION is a constructor."
  `(semantic-token-function-extra-spec ,function 'constructor)
  )

(defun kde-function-const (function)
  "Returns t if the FUNCTION has been declared as const, e.g.
if given a token representing \"int getInt() const\" this functions
would return t"
  (save-excursion
    (let ((start (semantic-token-start function)) 
	  (end (semantic-token-end function)))
      (goto-char end)
      (if (re-search-backward "const\b*;" start t)
	  t
	nil)
      )
    ))

(defun kde-is-prototype (function)
  "Returns t if the FUNCTION is only a prototype."
  (cond
   ((semantic-token-function-extra-spec function 'prototype)
    t)
   (t 
    (kde-function-const function))
   ))



(defun kde-function-at-point (pt)
  "Return function at pt as a token."
  (save-excursion
    (let ((token)
	  (what (semantic-find-nonterminal-by-position pt (current-buffer)))
	  (ctx))
      (goto-char pt)
      (if (eq (semantic-token-token what) 'function)
	  what
	(semantic-find-nonterminal-by-position pt (semantic-token-type-parts what)))
      )
    ))

(defun kde-function-construct (token pclass)
  "Constructs a function string from the TOKEN, with the parent class PCLASS."
  (let ((fname (semantic-token-name token)))
    (if (semantic-token-function-destructor token)
	(setq fname (concat "~" fname))
      )
    (if pclass
	(setq fname (concat pclass "::" fname))
      )
    (if (and
	 (not (kde-is-constructor token))
	 (not (semantic-token-function-destructor token)))
	(progn
	  (cond 
	   ((stringp (semantic-token-type token))
	    (setq fname (concat (semantic-token-type token) "\n" fname))
	    )
	   (t
	    (setq fname (concat (car (semantic-token-type token)) "\n" fname)))
	   )
	  (if (semantic-token-function-extra-spec token 'const)
	      (setq fname (concat "const " fname))
	    )
	  )
      )
    (setq fname (concat fname (kde-format-args (semantic-token-function-args token))))
    (if (kde-function-const token)
	(setq fname (concat fname " const" ))
      )
    (setq fname (concat fname "\n{" "\n}"))
    fname
    )
  )

(defun kde-class-expand (class-token)
  "Returns stubs for member functions as a string.
class-token has to be a token representing either a class or a struct."
  (let ((ret "")
	(name    (semantic-token-name class-token))
	(parents (semantic-token-type-parent class-token))
	(parts   (semantic-token-type-parts class-token))
	(cur-token)
	(cur-token-name)
	(asignal)
	(aslot)
	(namespace)
	)
    (dolist (elt parts ret)
      (setq cur-token (semantic-token-token elt))
      (setq cur-token-name (semantic-token-name elt))
      (cond
       ((and
	 (eq cur-token 'type)
	 (stringp cur-token-name))
	(cond
	 ((string= cur-token-name "class")
	  (kde-class-expand elt)
	  )
	 ((string= cur-token-name "enum")
	  ;;skip enums
	  )
	 ((string= cur-token-name "struct")
	  (kde-class-expand elt)
	  )
	 )
	)
       ((and
	 (eq cur-token 'function)
	 (stringp cur-token-name))
	;;FUNCTION - generate a skeleton for it
	(if (and (kde-is-prototype elt)
		 (not asignal))
	    (setq ret (concat ret (kde-function-construct elt name) "\n\n"))
	  )
	;(insert (kde-function-documentation elt) "\n")
	)
       ((and
	 (eq cur-token 'label)
	 (stringp cur-token-name))
	(setq aslot   nil
	      asignal nil)
	;;LABEL - unsets both signals and slots
	)
       ((and
	 (eq cur-token 'variable)
	 cur-token-name)
	;;VARIABLE - doesn't handle static variables correctly right now
	)
       ((not (stringp cur-token-name))
	(cond
	 ((kde-label-signals (car (semantic-token-extent elt)))
	  ;;SIGNALS - next prototypes belong to signals and we don't want to 
	  ;;          expand those
	  (setq asignal t
		aslot   nil)
	  )
	 ((kde-label-namespace (car (semantic-token-extent elt)))
	  ;;NAMESPACE - semantic doesn't handle things like Qt::ButtonState correctly
	  ;;            so we do ;)
	  (setq namespace (kde-label-namespace (car (semantic-token-extent elt))))
	  )
	 ((kde-label-slots (car (semantic-token-extent elt)))
	  ;;SLOTS - for now just unset signals
	  (setq aslot t
		asignal   nil)
	  )
	 (t
	  (insert "something else at " (number-to-string (car (semantic-token-extent elt))) "\n"))
	 ))
       (t
	(insert "Unknown type :: " (prin1-to-string elt) " >>" (prin1-to-string cur-token) "\n"))
       )
      )
    ret
    )
  )

(defun kde-expand-tokens (tokens)
  "Expands smenatic tokens to strings."
  (let ((ret ""))
    (dolist (elt tokens ret)
      (cond
       ((eq (semantic-token-token elt) 'type)
	(setq ret (concat ret (kde-class-expand elt)))
	)
       ((eq (semantic-token-token elt) 'function)
	(if (kde-is-prototype elt)
	    (setq ret (concat ret (kde-function-construct elt nil) "\n\n"))
	  )
	)
       ((eq (semantic-token-token elt) 'variable)
	;; skip
	;;(kde-extract-variable elt)
	)
       ((eq (semantic-token-token elt) 'include)
	;;ignore includes for now
	)
       (t (insert "Unknown type : " (prin1-to-string (semantic-token-type elt)) "\n"))
       )
      )
    )
  )


(defun kde-tokens-in-file (FILENAME)
  "Returns all tokens from a file with the FILENAME."
  (let ((exists (file-readable-p FILENAME))
	(buf (current-buffer))
	(tokens))
    (if exists
	(progn
	  (find-file FILENAME)
	  (setq tokens (semantic-bovinate-toplevel t))
	  (switch-to-buffer buf)
	  tokens)
      nil)
    ))

(defun kde-function-in-file (FUNC FILENAME)
  "Returns non-nil if FUNC is in a file named FILENAME"
  (let ((tokens (kde-tokens-in-file FILENAME)))
    (if tokens
	(kde-function-in-tokens FUNC tokens)
      nil
      )
    ))

(defun kde-function-is-expanded (FUNC)
  "Returns t if the function FUNC has been expanded."
  (let ((file (kde-file-get-cpp-h)))
    (if (cdr file)
	(if (kde-function-in-file FUNC (car file))
	    t
	  nil
	  )
      nil)
    ))

(defun kde-function-expanded-at-point (PT)
  "Returns non-nil if the function at point PT has already been expanded."
  (interactive "d")
  (let ((func (kde-function-at-point PT)))
    (kde-function-is-expanded func)
    )
  )

(defun kde-create-skeletons ()
  "Creates functions stubs in the source file, for all functions
in the current header file."
  (interactive)
  (let* ((all-tokens (semantic-bovinate-toplevel t))
	(filename (buffer-name))
	(cppfile (car (kde-file-get-cpp-h)))
	(funcs (kde-expand-tokens all-tokens)))
    (find-file cppfile)
    (save-excursion
      (insert "#include \"" filename "\"\n\n")
      (insert funcs)
      )
    )
  )

(defun kde-function-expand-at-point (PT)
  "Expand function at point PT."
  (interactive "d")
  (let ((object (semantic-find-nonterminal-by-position PT (current-buffer)))
	(func (kde-function-at-point PT))
	(file)
	(buf)
	(parent))
    (if (and object (equal (semantic-token-type object) "class"))
	(setq parent (semantic-token-name object)))
    (if (and (not (kde-function-expanded-at-point PT))
	     (kde-is-prototype func))
	(progn
	  (setq func (kde-function-construct func parent))
	  (setq file (car (kde-file-get-cpp-h)))
	  (setq buf (current-buffer))
	  (find-file file)
	  (save-excursion
	    (goto-char (point-max))
	    (insert "\n" func "\n")
	    )
	  (switch-to-buffer buf)
	  )
      (error "Function already expanded or defined!")
      )
    )
  )

(provide 'kde-emacs-semantic)
