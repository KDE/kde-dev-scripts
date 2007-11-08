;; kde-emacs-compat.el - contains compatibility functions
;;
;; Copyright (C)  2003  Zack Rusin <zack@kde.org>
;;                2003  KDE Developlment team
;;                2003  XEmacs developers
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
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

(require 'kde-emacs-vars)

;;GNU/Emacs does not have this one
(if (not (fboundp 'replace-in-string))
    (defun replace-in-string (str regexp newtext &optional literal)
      "Replace all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat `\\' in NEWTEXT as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\u' means upcase the next character.
  `\\l' means downcase the next character.
  `\\U' means begin upcasing all following characters.
  `\\L' means begin downcasing all following characters.
  `\\E' means terminate the effect of any `\\U' or `\\L'."
      (if (> (length str) 50)
	  (with-temp-buffer
	    (insert str)
	    (goto-char 1)
	    (while (re-search-forward regexp nil t)
	      (replace-match newtext t literal))
	    (buffer-string))
	(let ((start 0) newstr)
	  (while (string-match regexp str start)
	    (setq newstr (replace-match newtext t literal str)
		  start (+ (match-end 0) (- (length newstr) (length str)))
		  str newstr))
	  str)))
  
  )

(if (not (fboundp 'read-shell-command))
    (progn
      (defvar read-shell-command-map
	(let ((map (make-sparse-keymap 'read-shell-command-map)))
	  (if (eq kde-emacs-type 'xemacs)
	      (set-keymap-parents map (list minibuffer-local-map))
	    (set-keymap-parent map minibuffer-local-map))
	  (define-key map "\t" 'comint-dynamic-complete)
	  (define-key map "\M-\t" 'comint-dynamic-complete)
	  (define-key map "\M-?" 'comint-dynamic-list-completions)
	  map)
	"Minibuffer keymap used by `shell-command' and related commands.")
      (defun read-shell-command (prompt &optional initial-input history default-value)
	"Just like read-string, but uses read-shell-command-map:
\\{read-shell-command-map}"
	(let ((minibuffer-completion-table nil))
	  (read-from-minibuffer prompt initial-input read-shell-command-map
				nil (or history 'shell-command-history)
				nil default-value)))
      ))

(provide 'kde-emacs-compat)
