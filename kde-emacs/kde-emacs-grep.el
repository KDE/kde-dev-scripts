;; kde-emacs-grep.el 
;; Copyright (C)  2002  KDE Development Team <www.kde.org>
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.




;;;; domi: the stuff in this file comes from the etc/sample.init.el
;;;;       file in the XEmacs 21.4.6 distribution, i couldn't put it
;;;;       in kde-emacs-utils.el, because it is GPL code, where
;;;;       kde-emacs-utils.el is LGPL

; domi: my addition, seems necessary to make it work..
(defcustom kde-grep-find-use-xargs t
"Whether kde-grep-* can use the xargs program..  Set this to nil for
no, t for yes, or to the symbol gnu if you have GNU xargs")



;; Copyright (C) 2000, 2001 Ben Wing.

;; Author: Mostly Ben Wing <ben@xemacs.org>
;; Maintainer: XEmacs Development Team
;; Keywords: sample, initialization

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


(defvar kde-grep-all-files-history nil)

(defvar kde-grep-all-files-omitted-expressions
  '("*~" "#*" ".#*" ",*" "*.elc" "*.obj" "*.o" "*.exe" "*.dll" "*.lib" "*.a"
    "*.dvi" "*.class" "*.bin")
  "List of expressions matching files to be omitted in `kde-grep-all-files-...'.
Each entry should be a simple name or a shell wildcard expression.")

(defvar kde-grep-all-files-omitted-directories '("CVS" "RCS" "SCCS")
  "List of directories not to recurse into in `kde-grep-all-files-...'.
Each entry should be a simple name or a shell wildcard expression.")

(defun kde-construct-grep-all-files-command (find-segment grep-segment)
  (let ((omit-annoying
	 (mapconcat #'(lambda (wildcard)
			(concat "-name '" wildcard "' -or "))
		    kde-grep-all-files-omitted-expressions
		    "")))
    (cond ((eq kde-grep-find-use-xargs 'gnu)
	   (format "find . %s %s -type f -print0 | xargs -0 -e %s"
		   find-segment omit-annoying grep-segment))
	  (kde-grep-find-use-xargs
	   (format "find . %s %s -type f -print | xargs %s"
		   find-segment omit-annoying grep-segment))
	  (t
	   (format "find . %s %s -type f -exec %s {} /dev/null \\;"
		   find-segment omit-annoying grep-segment)))))

(defun kde-grep-all-files-in-current-directory (command)
  "Run `grep' in all non-annoying files in the current directory.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `kde-grep-all-files-omitted-expressions'.

This function does not recurse into subdirectories.  If you want this,
use \\[kde-grep-all-files-in-current-directory-and-below]."
  (interactive
   (progn
     (require 'compile)
     (list (read-shell-command "Run grep (like this): "
			       grep-command 'kde-grep-all-files-history))))
  (require 'compile)
  (let ((grep-null-device ""))  ; domi: my addition, prevent grep from
				;       adding /dev/null at the end..
    (grep (kde-construct-grep-all-files-command
	   "-name . -or -type d -prune -or" command))))

(defun kde-grep-all-files-in-current-directory-and-below (command)
  "Run `grep' in all non-annoying files in the current directory and below.
`Non-annoying' excludes backup files, autosave files, CVS merge files, etc.
More specifically, this is controlled by `kde-grep-all-files-omitted-expressions'.

This function recurses into subdirectories.  If you do not want this,
use \\[kde-grep-all-files-in-current-directory]."
  (interactive
   (progn
     (require 'compile)
     (list (read-shell-command "Run grep (like this): "
			       grep-command 'kde-grep-all-files-history))))
  (require 'compile)
  (let ((grep-null-device ""))  ; domi: my addition, prevent grep from
				;       adding /dev/null at the end..
    (grep (kde-construct-grep-all-files-command
	   ;; prune all specified directories.
	   (mapconcat #'(lambda (wildcard)
			  (concat "-name '" wildcard "' -prune -or "))
		      kde-grep-all-files-omitted-directories
		      "")
	   command))))

(provide 'kde-emacs-grep)
