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

(defconst kde-emacs-version "0.1"
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

(defvar magic-keys-mode nil
  "Set this variable to true to have some special keybindings.  E.g. bind '(' to a function which inserts '( ' when appropriate...")

(provide 'kde-emacs-vars)
