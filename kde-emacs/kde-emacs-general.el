;; kde-emacs-general.el
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

;;; Commentary:
;;

;;; Code:

(require 'kde-emacs-vars)

;*---------------------------------------------------------------------*/
;*    Functions  ...                                                   */
;*---------------------------------------------------------------------*/

;; Helper for kde-file-get-cpp-h
(defun kde-find-file (filename basedir)
  "Looks for \"filename\" under \"basedir\""
  (if basedir
      (let ((path (concat basedir "/" filename)))
	(if (file-readable-p path)
	    path))
    )
)

(defun kde-file-get-cpp-h ()
  "Function returns a corresponding source or header file. The returned
variable is a list of the form (FILENAME IS_READABLE) e.g. when being in
test.h file and having test.cpp file readable in the same directory it will
return (\"test.cpp\" t)."
  (interactive)
  (let* ((name (buffer-file-name))
	 (nname (file-name-sans-extension name))
	 (ext (file-name-extension name))
	 (path nil)
	 (ret nil)
	 (listit nil))
    (cond
     ((member ext kde-header-files)
      (setq listit kde-source-files)
      (while (and listit (not ret)) ; loop over the list but stop once ret is set
	(setq path (concat nname "." (car listit)))
	(if (file-readable-p path)
	    (setq ret (cons path t))
	  )
	(if (not ret)
	    (if (string-match "_p$" nname)
		(progn 
		  (setq path (concat (substring nname 0 (string-match "_p$" nname)) "." (car listit)))
		  (if (file-readable-p path)
		      (setq ret (cons path t))
		    )))
	  )
	(if (not ret)
	    (progn ; look in kde-source-directory
	      (setq path (kde-find-file (file-name-nondirectory path) kde-source-directory))
	      (if (and
		   path
		   (file-readable-p path))
		  (setq ret (cons path t))
		))
	  )
	(setq listit (cdr listit)) ; ++listit
	)
      ; not found, will create one
      (if (not ret)
	  (setq ret (cons (concat nname "." kde-prefered-source-extension) nil ))
	))

     ((member ext kde-source-files)
      (setq listit kde-header-files)
      (while (and listit (not ret)) ; loop over the list but stop once ret is set
	(setq path (concat nname "." (car listit)))
        ; look in current dir
	(if (file-readable-p path)
	    (setq ret (cons path t)))
	(if (not ret) ;check for header_p.h files
	    (progn (setq path (concat nname "_p." (car listit)))
		   (if (file-readable-p path)
		       (setq ret (cons path t)))))
	(if (not (file-readable-p path))
	    (progn ;  look in kde-include-directory
	      (setq path (kde-find-file (file-name-nondirectory path) kde-include-directory))
	      (if (and 
		   path
		   (file-readable-p path))
		  (setq ret (cons path t))
		))
	  )
	(setq listit (cdr listit)) ; ++listit
	)
      ; not found, will create one
      (if (not ret)
	  (setq ret (cons (concat nname "." (car kde-header-files)) nil ))
	))
     )
    ret
    ))

(defun kde-switch-cpp-h ()
  "Switches between the source and the header file 
(both directions)."
  (interactive)
  (let ((file (kde-file-get-cpp-h)))
    (if (car file)
	(find-file (car file))
      (error "Corresponding source file doesn't exist.")
    )
  ))

(defun kde-delete-backward-ws ()
  "Function deletes all preceding whitespace characters."
  (interactive)
  (let ((start (point))
	end)
    (save-excursion
      (setq end (re-search-backward "[^ \t]" (point-at-bol) t))
      (if (not end)
	  (setq end (point-at-bol))
	(setq end (1+ end))))
    (delete-backward-char (- start end))))

(defun kde-skip-blank-lines ()
  "Skips backwards past blank lines, stopping
at a first non-blank line"
  (let* ((start (point-at-bol))
	 (end (point-at-eol))
	 (mstring (buffer-substring start end))
	 (ret 0))
    (while (or 
	    (string-match "^[ \t\r\n]+$" mstring)
	    (and (string= mstring "")
		 (= ret 0)))
        (setq ret (forward-line -1))	; if ret != 0, we stop, since we're at the first line...
	(setq start (point-at-bol)
	      end   (point-at-eol))
	(setq mstring (buffer-substring start end))
	)
    ))

(defun kde-comments-begin ()
  "Skip back from current point past any preceding C-based comments at the beginning of lines.
Presumes no \"/*\" strings are nested within multi-line comments."
  (let ((opoint))
    (while (progn (setq opoint (point))
		  ;; To previous line
		  (if (zerop (forward-line -1))
		      (cond
		       ;; If begins with "//" or ends with "*/", then is a
		       ;; comment.
		       ((looking-at "[ \t]*\\(//\\|$\\)"))
		       ((looking-at ".*\\*/[ \t]*$")
			(progn (end-of-line)
			       ;; Avoid //*** single line comments here.
			       (if (re-search-backward "\\(^\\|[^/]\\)/\\*" nil t)
				   (progn (beginning-of-line)
					  (looking-at "[ \t]*/\\*")))))
		       (t nil)))))
    (goto-char opoint)
    ;; Skip past whitespace
    (skip-chars-forward " \t\n\r\f")
    (beginning-of-line)))

(provide 'kde-emacs-general)
