;; klaralv.el
;;
;; Copyright (C)  2002  KDE Development team
;; Authors : Klaralvdalens Datakonsult
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


(require 'kde-emacs-core)
(require 'kde-emacs-general)

;*---------------------------------------------------------------------*/
;*    Variables ...                                                    */
;*---------------------------------------------------------------------*/

(defcustom kdab-qt-documentation
  "http://doc.trolltech.com/3.0/XXX.html"
  "*URL for Qt documentation. XXX must be in the string. 
  Example: file:/packages/kde-src/qt-copy/doc/html/XXX.html"
  :group 'kde-devel
  :version "0.1"
  :type 'string)


;; special case for include files
;; Please notify blackie@klaralvdalens-datakonsult.se with any modification to this variable!
(defvar kdab-special-includes
  '( 
    (qlayout.h QHBoxLayout QVBoxLayout QGridLayout QBoxLayout)
    (qlistview.h QListViewItem QCheckListItem QListViewItemIterator)
    (qiconview.h QIconViewItem QIconDragItem QIconDrag)
    (qdragobject.h QTextDrag QStoredDrag QUriDag QColorDrag QImageDrag QDragManager)
    (qmime.h QMimeSource QMimeSourceFactory QWindowsMime)
    (qptrlist.h QPtrListIterator)
    (qevent.h QTimerEvent QMouseEvent QWheelEvent QTabletEvent QKeyEvent 
              QFocusEvent QPaintEvent QMoveEvent QResizeEvent QCloseEvent 
              QShowEvent QHideEvent QContextMenuEvent QIMEvent QDropEvent
              QDragMoveEvent QDragEnterEvent QDragResponseEvent QDragLeaveEvent
              QChildEvent QCustomEvent)
    (qdatetime.h QTime QDateTime QDate)
    (qpalette.h QColorGroup)
    
    ; Qt/Embedded
    (qcopchannel_qws.h QCopChannel)
    (qdirectpainter_qws.h QDirectPainter)
    (qfontfactorybdf_qws.h QFontFactoryBDF)
    (qfontfactoryttf_qws.h QFontFactoryFT)
    (qfontmanager_qws.h QGlyphMetrics QGlyph QRenderedFont QDiskFont QFontManager QFontFactory)
    (qgfx_qws.h QScreenCursor QPoolEntry QScreen QGfx)
    (qgfxlinuxfb_qws.h QLinuxFbScreen)
    (qgfxmatroxdefs_qws.h QQnxFbGfx QQnxScreen)
    (qgfxraster_qws.h QGfxRasterBase QGfxRaster)
    (qgfxvnc_qws.h QRfbRect QRfbPixelFormat QRfbServerInit QRfbSetEncodings 
                   QRfbFrameBufferUpdateRequest QRfbKeyEvent QRfbPointerEvent QRfbClientCutText QVNCServer)
    (qkeyboard_qws.h QWSKeyboardHandler)
    (qlock_qws.h QLock QLockHolder)
    (qmemorymanager_qws.h QMemoryManagerPixmap QMemoryManager)
    (qsoundqss_qws.h QWSSoundServer QWSSoundClient QWSSoundServerClient QWSSoundServerSocket)
    (qwindowsystem_qws.h QWSInternalWindowInfo QWSScreenSaver QWSWindow QWSSoundServer 
                         QWSServer QWSServer KeyboardFilter QWSClient)
    (qwsbeosdecoration_qws.h QWSBeOSDecoration)
    (qwscursor_qws.h QWSCursor)
    (qwsdecoration_qws.h QWSDecoration)
    (qwsdefaultdecoration_qws.h QWSDefaultDecoration)
    (qwsdisplay_qws.h QWSWindowInfo QWSDisplay)
    (qwshydrodecoration_qws.h QWSHydroDecoration)
    (qwskde2decoration_qws.h QWSKDE2Decoration)
    (qwskdedecoration_qws.h QWSKDEDecoration)
    (qwsmanager_qws.h QWSManager QWSButton)
    (qwsmouse_qws.h QWSPointerCalibrationData QWSMouseHandler QCalibratedMouseHandler 
                    QAutoMouseHandlerPrivate QWSMouseHandlerPrivate QVrTPanelHandlerPrivate 
                    QTPanelHandlerPrivate QYopyTPanelHandlerPrivate QCustomTPanelHandlerPrivate 
                    QVFbMouseHandlerPrivate)
    (qwsproperty_qws.h QWSPropertyManager)
    (qwsregionmanager_qws.h QWSRegionManager)
    (qwssocket_qws.h QWSSocket QWSServerSocket)
    (qwswindowsdecoration_qws.h QWSWindowsDecoration)

    ; KDE
    (kdebug.h kdDebug kdWarning kdError kdFatal kdBacktrace)
    
    ) "List of special include files which do not follow the normal scheme")


;*---------------------------------------------------------------------*/
;*    Functions ...                                                    */
;*---------------------------------------------------------------------*/

;; Lookup class `cls' in kdab-special-includes and return the associate include file name
(defun kdab-map-special (cls)
  (let ((list kdab-special-includes)
        (found nil))
    (while (and list (not found))
      (let* ( (elm (car list))
              (include-file (car elm))
              (classes (cdr elm)))
        ( while (and classes (not found))
          (if (string= (downcase cls) (downcase (symbol-name (car classes))))
              (setq found include-file)
            (setq classes (cdr classes)))))
      (setq list (cdr list)))
    (if found
        (symbol-name found)
      nil)  ; return value
    ))
            

;--------------------------------------------------------------------------------
; Insert include file.
; Place point anywhere on a class, and invoke this function. A result of
; this is that an include line is added (if it does not already exists) for
; the given class.
;--------------------------------------------------------------------------------
(defun kdab-insert-header ()
  (interactive "")
  (save-excursion
    (let* ((word (downcase (kde-word-under-point)))
           (header (cond
                    ((kdab-map-special word) (kdab-map-special word))
                    ((string-match "^qdom" word) "qdom.h")
                    ((string-match "^qxml" word) "qxml.h")
                    (t (concat word ".h")))))
      (beginning-of-buffer)
      (if (not (re-search-forward (concat "#include *<" header ">") nil t))
          (progn
                                        ; No include existsed
            (goto-char (point-max)) ; Using end-of-buffer makes point move, dispete save-excursion
            (if (not (re-search-backward "^#include *[\"<][^\">]+\.h *[\">]" nil t))
                (beginning-of-buffer)
              (progn (end-of-line) (forward-char 1)))
            (if (file-exists-p header)
                (progn 
                  ; See this as a local file.
                  (insert "#include \"" header "\"\n")
                  (message (concat "inserted " "#include \"" header "\"")))
              (progn
                (insert "#include <" header ">\n")
                (message (concat "inserted " "#include <" header ">")))))
        (message (concat "header file \"" header "\" is already included"))))))




;-----------------------------------------------------------------------------
; Start konqueror with documentation for the class under point.
; set `kdab-qt-documentation' to specify the replacement for the documentation
;-----------------------------------------------------------------------------
(defun kdab-lookup-qt-documentation ()
  (interactive "")
  (save-excursion
    (let* ((word (downcase (kde-word-under-point)))
          (url (if (not (string-match "XXX" kdab-qt-documentation))
                   (error "didn't find three X's in kdab-qt-documentation")
                 (replace-match word t t kdab-qt-documentation))))
      (start-process "qt documentation" nil "kfmclient" "openURL" url)
      (message (concat "Loading " url)))))

(provide 'klaralv)
