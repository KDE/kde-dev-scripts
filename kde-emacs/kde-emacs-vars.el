;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kde-emacs-vars.el 							    ;;
;; 									    ;;
;; Copyright (C)  2002  Zack Rusin <zack@kde.org>			    ;;
;;		                                                            ;;
;; This program is free software; you can redistribute it and/or	    ;;
;; modify it under the terms of the GNU General Public License		    ;;
;; as published by the Free Software Foundation; either version 2	    ;;
;; of the License, or (at your option) any later version.		    ;;
;;   									    ;;
;; This program is distributed in the hope that it will be useful,	    ;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of	    ;;
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the	    ;;
;; GNU General Public License for more details.				    ;;
;; 									    ;;
;; You should have received a copy of the GNU General Public License	    ;;
;; along with this program; if not, write to the Free Software		    ;;
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA		    ;;
;; 02111-1307, USA.							    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst kde-emacs-version "0.2"
  "KDE Emacs package version number.")
(defun kde-emacs-version ()
  "Returns the version of KDE Emacs package."
  (interactive)
  (message "KDE Emacs version : %s" kde-emacs-version))


(defvar kde-emacs-type
  (eval-when-compile
    (if (string-match "XEmacs" (emacs-version))
	'xemacs
      'emacs))
  "The type of Emacs we are running on.")

;*---------------------------------------------------------------------*/
;*    Constants ...                                                    */
;*---------------------------------------------------------------------*/

(defconst kde-access-labels
  "\\<\\(signals\\|k_dcop\\|\\(public\\|protected\\|private\\)\\([     ]+slots\\)?\\)\\>:"
  "KDE specific access labels regexp.")

(defconst kde-source-files '("cpp" "cc" "cxx" "CC" "C" "c")
  "List of source-file extensions.")

(defconst kde-header-files '("h" "H" "hh" "hxx" "hpp")
  "List of header-file extensions.")

;*---------------------------------------------------------------------*/
;*    Group ...                                                        */
;*---------------------------------------------------------------------*/
(defgroup kde-devel nil
  "Development utilities."
  :tag "KDE devel"
  :prefix "kdedevel-"
  :group 'programming)

(defcustom kde-full-name (or user-full-name
			     (getenv "USER")
			     "Your Name")
  "*Name used by kde-emacs."
  :group 'kde-devel
  :version "0.1"
  :type 'string)

(defcustom kde-email (or user-mail-address
			 (concat (getenv "LOGNAME") "@" (getenv "HOSTNAME"))
			 "Your Email")
  "*Email address used by kde-emacs."
  :group 'kde-devel
  :version "0.1"
  :type 'string)

(defcustom kde-cvs-root (concat (getenv "HOME") "/cvs/kde")
  "*Root Directory of KDE CVS Respiratory"
  :group 'kde-devel
  :type 'string)

(defcustom magic-keys-mode 't
  "Set this variable to true to have some special keybindings.  E.g. bind '(' to a function which inserts '( ' when appropriate..."
  :group 'kde-devel
  :type 'boolean)

;;Make styles a list of the format (radio (const kde-c++) (const kde-c) style)
;;and assign it to type.
(defcustom kde-c++-style "kde-c++"
  "Set this variable to the CC Mode style you would like loaded when you open a C++ KDE source code file..."
  :group 'kde-devel
  :type 'string)

(defcustom kde-c-style "kde-c"
  "Set this variable to the CC Mode style you would like loaded when you open a C KDE source code file..."
  :group 'kde-devel
  :type 'string)

(defcustom kde-use-pc-select 't
  "Set this to nil if you really hate PC Select Mode..."
  :group 'kde-devel
  :type 'boolean)

(defcustom kde-emacs-newline-semicolon nil
  "Set this to true to have typing \";\" automatically insert
a newline."
  :group 'kde-devel
  :type 'boolean)

(defcustom kde-header-protection-parts-to-show 1
  "Set this variable to the number of parts from the file name you want to be used for the defined word in the 
header-protection function..  E.g. setting this to 3 makes header-protection define KIG_MISC_NEWTYPE_H for a 
file named /home/domi/src/kdenonbeta/kig/misc/newtype.h"
  :group 'kde-devel
  :type 'integer)

(defcustom kde-emacs-after-parent-string " "
  "Set this to whatever you want to have inserted after the first parenthesis. Works only if
magic-keys-mode is set to true. "
  :group 'kde-devel
  :type 'string)



; a grep in the part of kde-source I have gives: 
; 5579 files uses .cpp, 1402 uses .cc, 10 uses .cxx, and 1 uses .C
(defconst kde-prefered-source-extension "cpp"
  "Source extension which kde-* functions should use for creating new files.")


(provide 'kde-emacs-vars)
