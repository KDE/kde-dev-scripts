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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307  USA

;;; Commentary:
;;

;;; Code:

(require 'kde-emacs-vars)

;*---------------------------------------------------------------------*/
;*    Functions  ...                                                   */
;*---------------------------------------------------------------------*/

(defun kde-file-get-cpp-h ()
  "Function returns a corresponding source or header file. The returned
variable is a list of the form (FILENAME IS_READABLE) e.g. when being in
test.h file and having test.cpp file readable in the same directory it will
return (\"test.cpp\" t)."
  (interactive)
  (let* ((name (buffer-name))
	 (nname (file-name-sans-extension name))
	 (ext (file-name-extension name))
	 (ret nil))
    (cond
     ((member ext kde-header-files)
      (dolist (elt kde-source-files nil)
	(if (file-readable-p (concat nname "." elt))
	    (setq ret (cons (concat nname "." elt) t)))
	)
      (if (not ret)
	  (setq ret (cons (concat nname "." (car kde-source-files)) nil ))
	))
     ((member ext kde-source-files)
      (dolist (elt kde-header-files nil)
	(if (file-readable-p (concat nname "." elt))
	    (setq ret (cons (concat nname "." elt) t)))
	)
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
      (error "Coresponding source file doesn't exist.")
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

(defun kde-word-under-point ()
  "Returns a word under the current position."
  (save-excursion
    (let* ((start (if (= (preceding-char) ?\ )
                      (point)
                    (progn (backward-word 1) (point))))
           (end (progn (forward-word 1) (point))))
      (buffer-substring start end))))

(defun kde-skip-blank-lines ()
  "Skips backwards past blank lines, stopping
at a first non-blank line"
  (let* ((start (point-at-bol))
	 (end (point-at-eol))
	 (mstring (buffer-substring start end)))
    (while (or 
	    (string-match "^[ \t\r\n]+$" mstring)
	    (string= mstring ""))
	(forward-line -1)
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
