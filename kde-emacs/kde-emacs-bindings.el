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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307  USA

; currently no binding for header-protection and add-file-to-makefile-am,
; you need to call them from M-x

; -----------------------------------------------------------------
; The list below defines the following bindings:
;
; F2 : offer a grep command
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
; Shift-F10: Place point on a class name, and press Shift-F10, and konqueror will load
;            Qt documentation. Customize the location of the Qt documentation with the 
;            variable kdab-qt-documentation. XXX will be replace with the class name.
;            Example (setq kdab-qt-location "file:/packages/kde-src/qt-copy/doc/html/XXX.html")
;
; Ctrl+Meta+D : insert a kdDebug statement with the name of the current method
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
;

(require 'kde-emacs-core)
(require 'kde-emacs-general)
(require 'kde-emacs-utils)
(require 'klaralv)
(require 'kde-emacs-utils)
(when (featurep 'semantic)
  (require 'kde-emacs-semantic)
  (require 'kde-emacs-doc))

;; Some example bindings, feel free to customize :)

(define-key global-map [(button4)] 'scroll-me-down)
(define-key global-map [(button5)] 'scroll-me-up)
(define-key global-map [(shift button4)] 'scroll-me-down-a-bit)
(define-key global-map [(shift button5)] 'scroll-me-up-a-bit)
(define-key global-map [(meta up)] 'scroll-other-up)
(define-key global-map [(meta down)] 'scroll-other-down)
(define-key global-map [(control j)] 'goto-line)
(define-key global-map [(f2)] 'grep)
(define-key global-map [(shift backspace)] 'kde-delete-backward-ws)

;; FIXME: remember to get these working on Gnu/Emacs (Zack)
(when (eq kde-emacs-type 'xemacs)
  (define-key global-map [(f8)] 'function-menu)
  (define-key global-map [(f3)] 'fume-list-functions)
  (define-key global-map [(shift f3)] 'fume-prompt-function-goto)
  )

(define-key global-map [(shift button3)] 'mouse-function-menu)
(define-key global-map [(shift f4)] 'makeclean)
(define-key global-map [(f4)] 'make)
(define-key global-map [(f5)] 'makeinstall)
(define-key global-map [(shift f5)] 'makeinstallexec)
(define-key global-map [(shift f6)] 'makethisfile)
(define-key global-map [(f6)] 'kde-switch-cpp-h)
(define-key global-map [(f7)] 'switch-to-function-def)
(define-key global-map [(f9)] 'agulbra-make-member)

; kde-emacs-headers:
(define-key global-map [(f10)] 'kdab-insert-header)
(define-key global-map [(shift f10)] 'kdab-lookup-qt-documentation)
(define-key global-map [(control meta d)] 'insert-kdDebug)

; Standard Qt/KDE shortcuts: Ctrl+Backspace, Ctrl+Delete
(define-key global-map [(control backspace)] 'backward-kill-word)
(define-key global-map [(control delete)] 'kill-word)

; Standard Qt/KDE shortcuts: Control Pageup and Pagedown
(define-key global-map [(control prior)] 'beginning-of-buffer)
(define-key global-map [(control next)] 'end-of-buffer)

; kde-emacs-semantic :
; no binding for kde-license-insert; call it via M-x
(when (featurep 'semantic)
  (define-key global-map [(control c)(control k)(d)] 'kde-doc-function-insert)
  (define-key global-map [(control c)(control k)(m)] 'kde-doc-multiline-insert)
  (define-key global-map [(control c)(control k)(o)] 'kde-doc-oneliner-insert)
  (define-key global-map [(control c)(control k)(e)] 'kde-function-expand-at-point)
  (define-key global-map [(control c)(control k)(s)] 'kde-create-skeletons))

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
(define-key c++-mode-map [menu-bar KDE insert-kdDebug]
  '("Insert kdDebug" . insert-kdDebug))


(provide 'kde-emacs-bindings)

