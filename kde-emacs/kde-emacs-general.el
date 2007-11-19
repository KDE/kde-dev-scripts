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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301  USA

;;; Commentary:
;;

;;; Code:

(require 'kde-emacs-vars)
(require 'sourcepair)

;*---------------------------------------------------------------------*/
;*    Functions  ...                                                   */
;*---------------------------------------------------------------------*/

;; remassoc exists as a built-in function in xemacs, but
;; not in GNU emacs
(if (not (functionp 'remassoc))
    (defun remassoc (key a)
      "remove an association pair from an alist"
      (if a
	  (let ((pair (car a)))
	    (if (equal (car pair) key)
		(cdr a)
		(cons pair (remassoc key (cdr a))))))))

;; Helper for kde-file-get-cpp-h
(defun kde-find-file (filename basedir)
  "Looks for \"filename\" under \"basedir\""
  (if basedir
      (let ((path (concat basedir "/" filename)))
	(if (file-readable-p path)
	    path))
    )
)

;; Helper for kde-file-get-cpp-h
(defun kde-file-or-buffer-exists (path)
  "Returns true if \"filename\" is an existing file, or an open buffer"
  (or (file-readable-p path)
      (get-file-buffer path))
)

(setq kde-file-lookup-cache '())

(defun kde-update-file-lookup-cache (file1 file2)
  (setq kde-file-lookup-cache (remassoc file1 kde-file-lookup-cache))
  (setq kde-file-lookup-cache (remassoc file2 kde-file-lookup-cache))
  (setq kde-file-lookup-cache 
        (cons (cons file1 file2)
              (cons (cons file2 file1) kde-file-lookup-cache))))
  
(defun kde-file-get-cpp-h ()
  "Function returns a corresponding source or header file. The returned
variable is a list of the form (FILENAME IS_READABLE) e.g. when being in
test.h file and having test.cpp file readable in the same directory it will
return (\"test.cpp\" t)."

  (save-excursion
    (let* ((current-file (buffer-file-name))
           (match (assoc current-file kde-file-lookup-cache))
           associated-file
           buffer)
      (if match
          (progn 
            (kde-update-file-lookup-cache current-file (cdr match))
            (cons (cdr match) 't)) ; return value

        (progn ;; else !match
          (setq buffer (sourcepair-load))
          (if (stringp buffer)
              (cons "" nil) ; return value
            (progn ;; Found a value
              (setq associated-file (buffer-file-name buffer))
              (kde-update-file-lookup-cache current-file associated-file)
              (cons (buffer-file-name buffer) 't))))))))

(defun kde-switch-cpp-h ()
  "Switches between the source and the header file (both directions)."
  (interactive)
  (let ((file (kde-file-get-cpp-h))
        (base-name-no-ext (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))
    (if (cdr file)
        (find-file (car file))
      (if (member (concat "." (file-name-extension (buffer-file-name))) sourcepair-header-extensions)
          (find-file (concat base-name-no-ext "." kde-prefered-source-extension))
        (find-file (concat base-name-no-ext ".h"))))))

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
