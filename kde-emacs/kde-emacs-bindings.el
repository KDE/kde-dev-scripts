;; kde-emacs-bindings.el
;;
;; Copyright (C)  2002  KDE Development Team
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

; currently no binding for header-protection and add-file-to-makefile-am,
; you need to call them from M-x

; -----------------------------------------------------------------
; The list below defines the following bindings:
;
; F2 : offer a grep command  (use C-u F2 if you need to specify options, like -i or -w)
; Shift-F2 : offer a grep command to search in directories below the current too..
;
; F3/Shift-F3/F8/Shift-RMB : different ways to see the list of methods in the current buffer
;
; F4 : make
; Shift-F4 : make clean
; F5 : make install
; Shift-F5 : make install-exec
;
; Shift-F6 : compile this file [assumes libtool is being used]
; F6 : Switch from .cpp/.cc to .h and vice-versa
; F7 : The same, but try to find the current method in the other file
; F9 : Create a member method in the .cpp, the cursor being on the definition in the .h
; F10: Place point on a class name, and the respective (Qt) include file will be inserted.
; This works with all Qt classes but can easily be extended to KDE classes.
; Shift-F10: Place point on a class name, and "class Blah" will be inserted near the top.
; Meta-F10: Place point on a class name, and press Meta-F10, and konqueror will load
;            Qt documentation. Customize the location of the Qt documentation with the
;            variable kdab-qt-documentation. XXX will be replace with the class name.
;            Example (setq kdab-qt-location "file:/packages/kde-src/qt-copy/doc/html/XXX.html")
;
; M-n: jump to the next error (after compiling) or grep matches
;
; Ctrl+Meta+D : insert a kDebug statement with the name of the current method
; [the new hide-all-windows shortcut conflicts with that, you may have to
;  change it, or use Ctrl+Meta+Shift+D (!!)]
;
; Meta Up/Down : scroll the other window (when window is split)

; Other very useful keybindings to know about:
; C-x r m    to set a named bookmark in the buffer
; C-x r b    to jump to a named bookmark in the buffer
; To save bookmarks to a file type:
; M-x bookmark-write 
; and to load bookmarks from a file write:
; M-x bookmark-load

(require 'kde-emacs-core)
(require 'kde-emacs-general)
(require 'kde-emacs-utils)
(require 'klaralv)
(require 'kde-emacs-utils)
(when (featurep 'semantic)
  (require 'kde-emacs-semantic)
  (require 'kde-emacs-doc))

;; Wheelmouse support
(define-key global-map [(button4)] 'scroll-me-down)
(define-key global-map [(button5)] 'scroll-me-up)
(define-key global-map [(shift button4)] 'scroll-me-down-a-bit)
(define-key global-map [(shift button5)] 'scroll-me-up-a-bit)

;; Some example bindings, feel free to customize :)
(define-key global-map [(meta up)] 'scroll-other-up)
(define-key global-map [(meta down)] 'scroll-other-down)
(global-set-key [(control j)] 'goto-line)
(global-set-key [(control %)] 'match-paren) ;;for all buffers :)

(if (featurep 'igrep)
    (progn
      (setq igrep-find-prune-clause
	    (format "-type d %s -name CVS -o -name .libs -o -name .deps -o -name .svn %s"
		    (shell-quote-argument "(")
		    (shell-quote-argument ")")))
      (setq igrep-find-file-clause
	    (format "-type f %s -name %s %s -name %s %s -name %s %s -name %s" ; -type l
		    (shell-quote-argument "!")
		    (shell-quote-argument "*~")	; Emacs backup
		    (shell-quote-argument "!")
		    (shell-quote-argument "*,v") ; RCS file
		    (shell-quote-argument "!")
		    (shell-quote-argument "s.*") ; SCCS file
		    (shell-quote-argument "!")
		    (shell-quote-argument "*.o") ; compiled object
		    (shell-quote-argument "!")
		    (shell-quote-argument ".#*") ; Emacs temp file
		    )
	    )
      (define-key global-map [(f2)] 'igrep)
      (define-key global-map [(shift f2)] 'igrep-find)
      (define-key global-map [(f12)] 'igrep-find)  ; on the console, shift f2 gives f12 for some reason..
      ;(setq igrep-files-default 'ignore) ; too hard to use *.cc *.h with it, because of the full path
      )
  (define-key global-map [(f2)] 'grep))
(define-key global-map [(shift backspace)] 'kde-delete-backward-ws)

;; FIXME: remember to get these working on Gnu/Emacs (Zack)
(when (eq kde-emacs-type 'xemacs)
  (define-key c++-mode-map [(f8)] 'function-menu)
  (define-key c++-mode-map [(f3)] 'fume-prompt-function-goto)
  (define-key c++-mode-map [(shift f3)] 'fume-list-functions)
  )

(define-key global-map [(shift button3)] 'mouse-function-menu)
(if kde-emacs-newline-semicolon
    (define-key c++-mode-map "\;" 'insert-semicolon))

(if kde-emacs-use-qtcreator-shortcuts
  (progn
    ;(define-key global-map [(shift f4)] 'makeclean)
    (define-key global-map [(control b)] 'make)
    (define-key global-map [(control B)] 'makeinstall)
    ;(define-key global-map [(shift f5)] 'makeinstallexec) ; TODO replace with run-current-program
    (define-key global-map [(shift f6)] 'makethisfile)
    (define-key c++-mode-map [(f4)] 'kde-switch-cpp-h)
    (define-key c-mode-map [(f4)] 'kde-switch-cpp-h)
    (define-key c++-mode-map [(shift f2)] 'switch-to-function-def)
    (define-key c-mode-map [(shift f2)] 'switch-to-function-def)
    (define-key c++-mode-map [(control shift f2)] 'agulbra-make-member)
    (define-key c-mode-map [(control shift f2)] 'agulbra-make-member)
    (define-key global-map [(f6)] 'next-error)
  )
; else
  (progn ; Historic default keybindings
    (define-key global-map [(shift f4)] 'makeclean)
    (define-key global-map [(f4)] 'make)
    (define-key global-map [(f5)] 'makeinstall)
    (define-key global-map [(shift f5)] 'makeinstallexec) ; TODO replace with run-current-program
    (define-key c++-mode-map [(f6)] 'kde-switch-cpp-h)
    (define-key c-mode-map [(f6)] 'kde-switch-cpp-h)
    (define-key c++-mode-map [(f7)] 'switch-to-function-def)
    (define-key c-mode-map [(f7)] 'switch-to-function-def)
    (define-key c++-mode-map [(f9)] 'agulbra-make-member)
    (define-key c-mode-map [(f9)] 'agulbra-make-member)
    (define-key global-map [(meta n)] 'next-error)
  )
)

; kde-emacs-headers:
(define-key c++-mode-map [(f10)] 'kdab-insert-header)
(define-key c++-mode-map [(shift f10)] 'kdab-insert-forward-decl)
(define-key c++-mode-map [(meta f10)] 'kdab-lookup-qt-documentation)
(define-key c++-mode-map [(control meta d)] 'insert-kDebug)

; Standard Qt/KDE shortcuts: Ctrl+Backspace, Ctrl+Delete
(define-key global-map [(control backspace)] 'backward-kill-word)
(define-key global-map [(control delete)] 'kill-word)

; Standard Qt/KDE shortcuts: Control Pageup and Pagedown
(define-key global-map [(control prior)] 'beginning-of-buffer)
(define-key global-map [(control next)] 'end-of-buffer)

; kde-emacs-semantic :
; no binding for kde-license-insert; call it via M-x
(when (featurep 'semantic)
  (define-key c++-mode-map [(control c)(control k)(d)] 'kde-doc-function-insert)
  (define-key c++-mode-map [(control c)(control k)(m)] 'kde-doc-multiline-insert)
  (define-key c++-mode-map [(control c)(control k)(o)] 'kde-doc-oneliner-insert)
  (define-key c++-mode-map [(control c)(control k)(e)] 'kde-function-expand-at-point)
  (define-key c++-mode-map [(control c)(control k)(s)] 'kde-create-skeletons))

(modify-frame-parameters (selected-frame) '((menu-bar-lines . 2)))
(define-key c++-mode-map [menu-bar KDE]
  (cons "KDE" c++-mode-map))
(when (featurep 'semantic)
  (define-key c++-mode-map [menu-bar KDE kde-doc-function-insert]
    '("kde-doc-function-insert" . kde-doc-function-insert))
  (define-key c++-mode-map [menu-bar KDE kde-function-expand-at-point]
    '("kde-function-expand-at-point" . kde-function-expand-at-point))
  (define-key c++-mode-map [menu-bar KDE kde-create-skeletons]
    '("kde-create-skeletons" . kde-create-skeletons))
  (define-key c++-mode-map [menu-bar KDE kde-doc-multiline-insert]
    '("kde-doc-multiline-insert" . kde-doc-multiline-insert)))
(define-key c++-mode-map [menu-bar KDE makeclean]
  '("make clean" . makeclean))
(define-key c++-mode-map [menu-bar KDE make]
  '("make" . make))
(define-key c++-mode-map [menu-bar KDE makeinstall]
  '("make install" . makeinstall))
(define-key c++-mode-map [menu-bar KDE makethisfile]
  '("make this file" . makethisfile))
(define-key c++-mode-map [menu-bar KDE kdeswitchcpph]
  '("Switch to .h/.cpp file" . kde-switch-cpp-h))
(define-key c++-mode-map [menu-bar KDE insert-kDebug]
  '("Insert kDebug" . insert-kDebug))


(provide 'kde-emacs-bindings)

