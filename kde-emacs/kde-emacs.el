;; kde-emacs.el
;; Time-stamp: <2002-06-26 00:49:48 zack>
;;
;; Copyright (C)  2002  Zack Rusin <zackrat@att.net>
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

;;; Installation:
;;
;; Put the following lines in your ".emacs":
;; (add-to-list 'load-path "~/path-to-kde-emacs")
;; (require 'kde-emacs)
;; 
;; I also strongly recommend to add the following two lines to 
;; .emacs file:
;; (setq kde-full-name "Your Name")
;; (setq kde-email "Your Email")
;;
;; You may want to byte-compile the package to speed it up
;; a bit. To do it in the *scratch* buffer type in the following
;; line:
;; (byte-recompile-directory "~/kde-emacs" t)
;; place the cursor after the closing paren and hit "Ctrl-x Ctrl-e",
;; that's it.
;;
;; All keybindings are in kde-emacs-bindings.el, look at/customize 
;; this file before byte-compiling the package!
;; If you want to see things you can customize type:
;; M-x customize-group
;; and type in "kde-devel" group.
;;
;; TODO: 
;; - in (if kde-emacs-type... change direct function calls
;;   to funcall's
;;

(require 'cc-mode) ;; needed by kde-emacs-core's test on c-version

(require 'kde-emacs-compat)
(require 'kde-emacs-core)
(require 'kde-emacs-general)
(require 'klaralv)
(require 'kde-emacs-utils)
(require 'dirvars)

;; load this only if semantic package is present
(when (featurep 'semantic)
  (require 'kde-emacs-semantic)
  (require 'kde-emacs-doc))

(require 'kde-emacs-bindings)

(provide 'kde-emacs)
