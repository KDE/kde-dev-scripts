;; kde-emacs-core.el - core functionality,e.g. syntax & c++-mode-hook
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

(require 'kde-emacs-vars)
;*---------------------------------------------------------------------*/
;*    Variables ...                                                    */
;*---------------------------------------------------------------------*/

(defcustom kde-tab-behavior 'default
  "Specifies the current tab behavior. default will expand try to complete
the symbol at point if at the end of something that looks like an indentifier else
it will indent the current line if the pointer is at the beginning of the line it will
be moved to the start of the indention. abbrev-indent behaves like default, but the 
cursor isn't moved to the beginning of the indention with tab is pressed when the cursor
is at the beginning of the line. indent simply indents the line without trying to 
complete the symbol"
  :group 'kde-devel
  :version "0.1"
  :type `(choice (const default) (const abbrev-indent) (const indent)))

;*---------------------------------------------------------------------*/
;*    Functions ...                                                    */
;*---------------------------------------------------------------------*/


;; -------  First part, from Arnt's "c++ stuff" - slightly modified for our needs :)

(defun agulbra-c++-tab (arg)
  "Do the right thing about tabs in c++ mode.
Try to finish the symbol, or indent the line."
  (interactive "*P")
  (cond
   ((and (not (looking-at "[A-Za-z0-9]"))
         (save-excursion 
           (forward-char -1)
           (looking-at "[A-Za-z0-9:>_\\-\\&\\.(){}\\*\\+/]")))
         (dabbrev-expand arg))
   (t
    (if (eq kde-tab-behavior 'default)
	(c-indent-command)
      (save-excursion
	(beginning-of-line)
	(c-indent-command))))))

(defun agulbra-clean-out-spaces ()
  "Remove spaces at ends of lines."
  (interactive)
  (and (not buffer-read-only)
       (save-excursion
         (goto-char (point-min))
         (let ((count 0)
               (bmp (buffer-modified-p)))
           (while (re-search-forward "[ \t]+$" nil t)
             (setq count (1+ count))
             (replace-match "" t t))
           (set-buffer-modified-p bmp)
	   nil
           ))))

; the above used to contain (untabify (point-min) (point-max)) too

(defun agulbra-c++-clean-out-spaces ()
  "Remove spaces at ends of lines, only in c++ mode."
  (interactive)
  (if (eq major-mode 'c++-mode)
      (agulbra-clean-out-spaces)
    )
  )

(defun agulbra-delete-into-nomenclature (&optional arg)
  "Delete forward until the end of a nomenclature section or word.
With arg, do it arg times."
  (interactive "p")
  (save-excursion
    (let ((b (point-marker)))
      (c-forward-into-nomenclature arg)
      (delete-region b (point-marker)))))


(if (not (fboundp 'font-lock-add-keywords))
    (defun font-lock-add-keywords (mode keywords &optional append)
      "XEmacs doesn't have font-lock-add-keywords so we provide it."
      (font-lock-set-defaults)
      (if (eq append 'set)
	  (setq font-lock-keywords keywords)
	; NOTE: write this function for XEmacs - Zack
	;(font-lock-remove-keywords nil keywords) ;to avoid duplicates
	(let ((old (if (eq (car-safe font-lock-keywords) t)
		       (cdr font-lock-keywords)
		     font-lock-keywords)))
	  (setq font-lock-keywords (if append
				       (append old keywords)
				     (append keywords old))))))
  )

(c-add-style "kde-c" '("stroustrup"
		       (c-basic-offset . 4)
		       (c-offsets-alist
			(case-label . 4)
			(access-label . -)
			(label . 0)
			(statement-cont . c-lineup-math)
			)))

;  ( we use Backquote ( '`' ) instead of "'" because we want
;    kde-access-labels to be evaluated... )
(c-add-style "kde-c++" `("kde-c"
  ;;FIXME: 1) fume functions not available on GNU/Emacs
  ;;       2) insert-tab-mode no longer present (free variable)
  ;;       3) c-hangin-commment-under-p no longer present (free variable)
			 (if (not (eq kde-tab-behavior 'indent))
			     (c-tab-always-indent . nil))
					; (insert-tab-mode nil)
			 (indent-tabs-mode . nil)
			 (if (eq kde-emacs-type 'xemacs)
			     (fume-auto-rescan-buffer-p nil))
			 (c-access-key . ,kde-access-labels)
			 (c-opt-access-key . ,kde-access-labels)
					; (c-hanging-comment-under-p nil)
			 (c-offsets-alist . ((case-label . 0)
					     (inline-open . 0)))
			 ))

;; KDE C++ mode
;; Not a "(setq c++-mode-hook ..." because this way we would
;; prune all other hooks!
(defun kde-c++-mode-hook ()
  (font-lock-mode)
  (c-set-style kde-c++-style)
  (define-key c++-mode-map "\C-m" 'c-context-line-break)
  (when (or
	 (eq kde-tab-behavior 'default)
	 (eq kde-tab-behavior 'abbrev-indent))
    (define-key c++-mode-map "\C-i" 'agulbra-c++-tab))
  (define-key c++-mode-map "\ef" 'c-forward-into-nomenclature)
  (define-key c++-mode-map "\ed" 'agulbra-delete-into-nomenclature)
  (define-key c++-mode-map "\eb" 'c-backward-into-nomenclature)
  ;; fontify "public|protected|private slots" with one and the same face :)
  ;; NOTE: write face-at-point function to fontify those just like other
  ;; access specifiers
  ;; This breaks in the font-lock-fontify engine in xemacs-21.5.28... no solution known yet.
  ;; TODO use the const variable kde-access-labels here. Couldn't figure out the syntax.
  ;(font-lock-add-keywords nil '(("\\<\\(signals\\|Q_SIGNALS\\|k_dcop\\|\\(public\\|protected\\|private\\)\\([     ]+\\(slots\\|Q_SLOTS\\)\\)?\\)\\>:" . font-lock-reference-face)))

  ;; Add (setq magic-keys-mode nil) to your .emacs (before loading this file)
  ;; to disable the magic keys in C++ mode.
  (and (boundp 'magic-keys-mode) magic-keys-mode
       (progn
	 (define-key c++-mode-map "\," 'insert-comma)
	 (define-key c++-mode-map "\{" 'insert-curly-brace)
	 (define-key c++-mode-map "\(" 'insert-parens)
	 (define-key c++-mode-map "\)" 'insert-parens2)
	 ))
  )

(defun kde-c-mode-hook ()
  (font-lock-mode)
  (c-set-style kde-c-style))

(and (user-variable-p 'kde-emacs-delete-trailing-whitespace)
     kde-emacs-delete-trailing-whitespace
     (progn
       (add-hook 'find-file-hooks 'agulbra-c++-clean-out-spaces)
       (add-hook 'write-file-hooks 'agulbra-c++-clean-out-spaces)
       ))

(add-hook 'c++-mode-hook 'kde-c++-mode-hook)
(add-hook 'c-mode-hook 'kde-c-mode-hook)
; always end a file with a newline
(setq-default require-final-newline t)
; 'next-line won't be adding newlines
(setq-default next-line-add-newlines nil)
(setq compilation-error-regexp-systems-list '(gnu of comma 4bsd)
      compilation-ask-about-save nil)

(provide 'kde-emacs-core)
