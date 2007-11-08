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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA		    ;;
;; 02110-1301, USA.							    ;;
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
  "\\<\\(signals\\|Q_SIGNALS\\|k_dcop\\|\\(public\\|protected\\|private\\)\\([     ]+\\(slots\\|Q_SLOTS\\)\\)?\\)\\>:"
  "KDE specific access labels regexp.")

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
  "Set this variable to true to have some special keybindings. E.g. bind ',' to a function which inserts ', ' when appropriate..."
  :group 'kde-devel
  :type 'boolean)

(defcustom magic-parens-mode 't
  "Set this variable to true to bind '(' and ')' to functions which insert spaces when appropriate. Depends on magic-keys-mode being set."
  :group 'kde-devel
  :type 'boolean)

(defcustom kde-emacs-make "make"
  "Specifies the make command which KDE Emacs will use"
  :group 'kde-devel
  :type 'string)

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

(defcustom kde-include-directory nil
  "Set this to the directory holding the includes for the current module/project/whatever."
  :group 'kde-devel
  :type 'string)

(defcustom kde-source-directory nil
  "Set this to the directory holding the sources for the current module/project/whatever."
  :group 'kde-devel
  :type 'string)

(defcustom kde-make-member-default-impl "    \n"
  "Default implementation added by agulbra-make-member. FUNCTION gets replaced by the full signature of the function/method."
  :group 'kde-devel
  :type 'string)

; a grep in the part of kde-source I have gives: 
; 5579 files uses .cpp, 1402 uses .cc, 10 uses .cxx, and 1 uses .C
(defconst kde-prefered-source-extension "cpp"
  "Source extension which kde-* functions should use for creating new files.")

(provide 'kde-emacs-vars)
