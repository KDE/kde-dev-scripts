;; kde-emacs-doc.el
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
;;
;;
;;; Documentation :
;; Interactive functions:
;; kde-license-insert - inserts the chosen license header in the current
;;                      buffer,
;; kde-doc-function-insert - insert documentation skeleton for the function
;;                           at the current location
;; kde-doc-multiline-insert - inserts blank mutliline comment skeleton
;; kde-doc-oneliner-insert  - inserts blank one line comment skeleton
;;
;;; TODO :
;; - add interactive functions to insert file, class, brief,
;;   and group comments,
;; - change the way commenting works after license insertion,
;; - add syntax higlighting for doxygen/kdoc keywords
;; - add more license headers


(require 'kde-emacs-core)
(require 'kde-emacs-semantic)

;*---------------------------------------------------------------------*/
;*    Licenses   ...                                                   */
;*---------------------------------------------------------------------*/

(defvar LGPL "This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.
  
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
 
You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301  USA"
  "GNU LGPL license header.")

(defvar GPL "This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
  
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA."
  "GNU GPL license header.")

(defvar FDL "Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.1
or any later version published by the Free Software Foundation;
with the Invariant Sections being LIST THEIR TITLES, with the
Front-Cover Texts being LIST, and with the Back-Cover Texts being LIST.
A copy of the license is included in the section entitled \"GNU
Free Documentation License\"."
  "GNU FDL license header.")

(defvar BSD "Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
  "BSD license header.")


;;----------------
;; Variables     |
;;----------------
                 
(defconst kde-doc-styles
  '(
    (javadoc . 
	     ((start     . "/**")
	      (end       . "*/")
	      (separator . "\n*") 
	      (oneliner  . "///")
	      (element   . "/**< */")
	      (param     . "@param %s")
	      (return    . "@return")
	      (seealso   . "@see")
	      (class     . "")
	      (brief     . "@brief")
	      (file      . "@file %s")
	     ))
    (qt      .
	     ((start     . "/*!")
	      (end       . "*/")
	      (separator . "\n")
	      (oneliner  . "//!") 
	      (element   . "/*!< */")
	      (param     . "\\param %s")
	      (return    . "\\return")
	      (seealso   . "\\sa")
	      (class     . "\\class")
	      (brief     . "\\brief")
	      (file      . "\\file %s")
	     ))
    )
  "Documentation styles used by KDE.")

(defcustom kde-doc-style 'javadoc
  "Current documentation style. This variable is buffer local."
  :group 'kde-devel
  :version "0.1"
  :type (if (boundp 'kde-doc-styles)
	    `(choice ,@(mapcar (lambda (s) `(const ,(car s))) kde-doc-styles))
	  'symbol))
(make-variable-buffer-local 'kde-doc-style)

(defcustom kde-license-comment-style 'box
  "Style to be used for `kde-license-insert'.
See `comment-styles' for a list of available styles."
  :group 'kde-devel
  :version "0.1"
  :type (if (boundp 'comment-styles)
	    `(choice ,@(mapcar (lambda (s) `(const ,(car s))) comment-styles))
	  'symbol))

;*---------------------------------------------------------------------*/
;*    Functions  ...                                                   */
;*---------------------------------------------------------------------*/

(defun kde-license-header ()
  (let ((ret (file-name-nondirectory (buffer-file-name))))
    (setq ret (concat ret " \n\n"))
    (setq ret (concat ret "Copyright (C)  " (format-time-string "%Y  ")
		      kde-full-name " <"kde-email">\n\n"))
    ))

(defun kde-license-insert (license)
  "Inserts the chosen license header at the top of the current
buffer."
  (interactive (list (completing-read
		      "Which license do you want to use? "
		      '(("GNU GPL" 1) ("GNU LGPL" 2) ("GNU FDL" 3) ("BSD" 4))
		      nil t nil)))
  (save-excursion
    (let ((start (point-min))
	  (end)
	   )
      (setq comment-style kde-license-comment-style)
      (goto-char start)
      (if license
	  (progn
	    (cond ((string= license "GNU GPL")
		   (insert (kde-license-header))
		   (insert GPL)
		   )
		  ((string= license "GNU LGPL")
		   (insert (kde-license-header))
		   (insert LGPL)
		   )
		  ((string= license "GNU FDL")
		   (insert (kde-license-header))
		   (insert FDL)
		   )
		  ((string= license "BSD")
		   (insert (kde-license-header))
		   (insert BSD)
		   )
		  )
	    (insert "\n")
	    (setq end (point))
	    (comment-region start end)
	    )
	)
      )
    ))

(defmacro kde-doc-type-string (arg)
  "Maps doc element from kde-doc-style to string."
  `(cdr (assoc ,arg (assoc kde-doc-style kde-doc-styles)))
  )

(defun kde-doc-param-string (ARG)
  "Substitues %s in the param string with ARG."
  (let ((par (kde-doc-type-string 'param)))
    (if (string-match "\%s" par)
	(replace-match ARG t t par)
      par))
  )

(defun kde-function-documentation (function)
  (let ((ret "") (rettype (semantic-token-type function)))
    (setq ret (kde-doc-type-string 'start))
    (setq ret (concat ret (kde-doc-type-string 'separator)))
    (dolist (elt (semantic-token-function-args function) ret)
      (setq ret (concat ret (kde-doc-type-string 'separator) " "
			(kde-doc-param-string (semantic-token-name elt))))
      )
    (if (not (or
	      (kde-is-constructor function)
	      (semantic-token-function-destructor function)))
	(progn
	  (if (listp rettype)
	      (setq rettype (car rettype)))
	  (if (not (string= rettype "void"))
	      (setq ret (concat ret (kde-doc-type-string 'separator) " " (kde-doc-type-string 'return)))
	    )
	  )
      )
    (setq ret (concat ret "\n" (kde-doc-type-string 'end) ))
    ))

(defun kde-doc-function-insert ()
  "Inserts skeleton function documentation for a function
at the current location."
  (interactive)
  (save-excursion
    (let* ((pt (point))
	   (token (kde-function-at-point pt))
	   (ret "")
	   (start) (end)
	   )
      (if (not token)
	  (error "There's no function at %d." pt)
	(progn
	  (setq ret (kde-function-documentation token))
	  (goto-char (semantic-token-start token))
	  (previous-line)
	  (goto-char (point-at-eol))
	  (setq start (point))
	  (insert "\n " ret)
	  (setq end (semantic-token-end token))
	  (indent-region start end nil)
	  )
	)
      )))

(defun kde-doc-oneliner-insert ()
  "Insert oneliner comment at the current point. If the line is not empty newline is inserted."
  (interactive)
  (let ((thisblank)(pt))
  (save-excursion
      (beginning-of-line)
      (setq pt (point))
      (setq thisblank (looking-at "[ \t]*$"))
      (if (not thisblank)
	  (progn
	    (newline)
	    (goto-char pt)
	    ))
      (insert (kde-doc-type-string 'oneliner))
      (setq pt (point-at-eol))
      (end-of-line)
      )
  (goto-char pt)
  ))

(defun kde-doc-multiline-insert ()
  "Inserts blank multiline comment at point. If the current line isn't blank
the functions inserts a newline."
  (interactive)
  (let ((thisblank)(start) (end))
  (save-excursion
      (beginning-of-line)
      (setq start (point))
      (setq thisblank (looking-at "[ \t]*$"))
      (if (not thisblank)
	  (progn
	    (newline)
	    (goto-char start)
	    ))
      ;; The blank to fix sometimes occuring
      ;; weird behavior in indent-region
      (insert " " 
	      (kde-doc-type-string 'start) 
	      (kde-doc-type-string 'separator) "\n"
	      (kde-doc-type-string 'end)
	      )
      (setq end (point))
      (indent-region start end nil)
      )
  (goto-char start)
  (end-of-line)
  ))


(provide 'kde-emacs-doc)
