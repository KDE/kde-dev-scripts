;; ------------------------------ COPYRIGHT NOTICE ------------------------------
;; klaralv.el version 1.3
;; Copyright Klaralvdalens Datakonsult AB.
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.


;; ------------------------------ INSTALLATION ------------------------------
;; To use this file, add the current directory to your load path.
;; you do this by inserting something like the following to your .emacs:
;; (setq load-path (cons "/home/blackie/Emacs/" load-path))
;;
;; Next insert the following line into your .emacs
;; (require 'klaralv)
;; (global-set-key [(f5)] 'kdab-insert-header)
;; (global-set-key [(shift f5)] 'kdab-insert-forward-decl)
;; (setq kdab-qt-documentation "file://usr/local/qt/html/doc/XXX.html")
;; (global-set-key [(control f5)] 'kdab-lookup-qt-documentation)
;; 
;; If you use QTopia, and do not want include files to be prefixed with qpe/,
;; as in qpe/qpeapplication, then insert the following code in your setup
;; (setq kdab-prefix-qpe nil)

;; ------------------------------ CONFIGURATION ------------------------------
(defvar kdab-qt-documentation
  "http://doc.trolltech.com/3.0/XXX.html"
  "URL for Qt documentation. XXX must be in the string. 
  Example: file://packages/kde-src/qt-copy/doc/html/XXX.html")

(defvar kdab-qpe-documentation
  "file://opt/qtopia/doc/XXX.html"
  "URL for QTopia documentatin. XXX must be in the string. 
  Example: file:/opt/qtopia/doc/XXX.html")


(defvar kdab-prefix-qpe 't
  "set this to nil if you do not want QPE header files prefixed with qpe/")

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
    (qdatetimeedit.h QTimeEdit QDateTimeEditBase QDateEdit QDateTimeEdit)
    (qcstring.h QByteArray)
    (qwidgetlist.h QWidgetListIt)
    (qtabbar.h QTab)
    (qpalette.h QColorGroup)
    (qaction.h QActionGroup)
    (qvalidator.h QIntValidator QDoubleValidator QRegExpValidator)
    (qlistbox.h QListBoxItem QListBoxText QListBoxPixmap)
    (qstring.h QChar QCharRef QConstString)
    (qcanvas.h QCanvasSprite QCanvasPolygonalItem QCanvasRectangle
               QCanvasPolygon QCanvasEllipse QCanvasText QCanvasLine
               QCanvasChunk QCanvas QCanvasItem QCanvasView QCanvasPixmap)
    (qgl.h QGLFormat QGL QGLContext QGLWidget QGLColormap)
    (qtable.h QTableSelection QTableItem QComboTableItem QCheckTableItem) 

    
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

    ; KDGear - http://www.klaralvdalens-datakonsult.se
    (KDCheckableGroupBox.h KDCheckableGroupBox)
    (KDCheckableHGroupBox.h KDCheckableHGroupBox)
    (KDCheckableVGroupBox.h KDCheckableVGroupBox)
    (KDCloseableWidget.h KDCloseableWidget)
    (KDConfigDialog.h KDConfigDialog)
    (KDConfigWidget.h KDConfigWidget)
    (KDDateWidget.h KDDateWidget KDDateTimeWidget)
    (KDDirMonitor.h KDDirMonitor)
    (KDGridWidget.h KDGridWidget)
    (KDListBoxPair.h KDListBoxPair)
    (KDMinimizeSplitter.h KDMinimizeSplitter)
    (KDSearchableListBox.h KDSearchableListBox)
    (KDSemiSizingControl.h KDSemiSizingControl)
    (KDShowHideTableControl.h KDShowHideTableControl)
    (KDSimpleSizingControl.h KDSimpleSizingControl)
    (KDSizingControl.h KDSizingControl)
    (KDStream.h KDStream)
    (KDTimeWidget.h KDTimeWidget)

    ; Fake entries, which is usefull
    (qapplication.h qApp)
    (kapplication.h kapp)
    (klocale.h i18n I18N_NOOP)
    (kstandarddirs.h locate locateLocal)
    (stdlib.h getenv)
    (unistd.h unlink)
    (iostream.h cout cerr)
    (ctype.h isalnum isalpha isascii isblank iscntrl isdigit isgraph islower isprint ispunct isspace isupper isxdigit)


    )
    "List of special include files which do not follow the normal scheme")

(defvar kdab-qpe-includes 
  '(
    (alarmserver.h AlarmServer)
    (applnk.h AppLnk DocLnk AppLnkSet DocLnkSet)
    (calendar.h Calendar)
    (categories.h CategoryGroup CategoryGroup Categories CheckedListView)
    (categorymenu.h CategoryMenu)
    (categoryselect.h CategoryCombo CategorySelect CategoryEdit CategoryWidget)
    (config.h Config)
    (contact.h Contact)
    (database.h QWSDatabase DatabaseDefaultView Database DatabaseView DatabaseDefaultView)
    (datebookdb.h DateBookDB)
    (datebookmonth.h DateBookMonthHeader DayItemMonth DateBookMonthTable DateBookMonth DateButton)
    (event.h Event EffectiveEvent EffectiveEventSizeSorter EffectiveEventTimeSorter)
    (filemanager.h FileManager)
    (fileselector.h FileSelectorItem FileSelector)
    (finddialog.h FindDialog)
    (fontdatabase.h FontDatabase)
    (fontmanager.h FontManager)
    (global.h Global)
    (imageedit.h ImageEdit)
    (inputmethodinterface.h InputMethodInterface)
    (ir.h Ir)
    (lightstyle.h LightStyle)
    (lnkproperties.h LnkProperties)
    (mediaplayerplugininterface.h MediaPlayerDecoder)
    (menubutton.h MenuButton)
    (mimetype.h MimeType)
    (network.h Network)
    (palmtoprecord.h Record)
    (palmtopuidgen.h UidGen)
    (password.h Password)
    (power.h PowerStatus PowerStatusManager )
    (process.h Process)
    (qcopenvelope_qws.h QCopEnvelope)
    (qdawg.h QDawg)
    (qlibrary.h QLibrary)
    (qpeapplication.h QPEApplication)
    (qpedecoration_qws.h QPEDecoration QPEManager)
    (qpedialog.h QPEDialogListener)
    (qpemenubar.h  QPEMenuToolFocusManager QPEMenuBar)
    (qpemessagebox.h QPEMessageBox)
    (qpestyle.h QPEStyle : public QWindowsStyle)
    (qpetoolbar.h QPEToolBar)
    (record.h Record)
    (resource.h Resource)
    (sound.h Sound)
    (storage.h StorageInfo FileSystem)
    (task.h Task)
    (timeconversion.h TimeConversion)
    (timestring.h DateFormat TimeString)
    (tzselect.h TZCombo TimeZoneSelector)
    ))

;; ------------------------------ SOURCE CODE ------------------------------

;; Merge in qpe classes
(defun kdab-get-special-include-list ()
  (let (elm header classes (list kdab-qpe-includes) filename (result kdab-special-includes))
    (while list
      (setq elm (car list))
      (setq list (cdr list))
      (setq filename (concat (if kdab-prefix-qpe "qpe/" "") (symbol-name (car elm))))
      (setq result (cons (cons (intern filename) (cdr elm)) result)))
    result))

;; Lookup class `cls' in kdab-special-includes and return the associate include file name
(defun kdab-map-special (cls)
  (let ((list (kdab-get-special-include-list))
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
; Insert include file for Qt program.
; Place point anywhere on a Qt class, and invoke this function. A result of
; this is that an include line is added (if it does not already exists) for
; the given class.
;--------------------------------------------------------------------------------
(defun kdab-insert-header ( prefix )
  "Insert include file for class at point"
  (interactive "P")
  (save-excursion
    (let* ((word-at-point (if prefix
                              (read-from-minibuffer "Class: ")
                            (current-word))))
      (kdab-insert-header-non-interactive word-at-point))))

;--------------------------------------------------------------------------------
; insert include file for `word-with-case' non-interactively.
; for an interactive version see kdab-insert-header
;--------------------------------------------------------------------------------
(defun kdab-insert-header-non-interactive (word-with-case)
  (save-excursion
    (let* ((word (downcase word-with-case))
           (special-header (cond
                    ((kdab-map-special word) (kdab-map-special word))
                    ((string-match "^qdom" word) "qdom.h")
                    ((string-match "^qxml" word) "qxml.h")
                    (t (concat word ".h"))))
           header is-local)

      
      ;; decide on the header file.
      (if (file-exists-p (concat word-with-case ".h"))
          (progn ; file exists in given case in pwd.
            (setq header (concat word-with-case ".h"))
            (setq is-local 't))
        (if  (file-exists-p (concat word ".h")) ; file exists in lowercase in pwd
            (progn
              (setq header (concat word ".h"))
              (setq is-local 't))
          (progn ; header in <..> path
            (setq header special-header)
            (setq is-local nil))))

      (kdab-insert-include-file header is-local t))))

;--------------------------------------------------------------------------------
; Insert header file for header. If is-local insert it with "" 
; otherwise insert it with <>
;--------------------------------------------------------------------------------
(defun kdab-insert-include-file (header is-local show-message)
  (let ((include-file (if is-local
                          (concat "#include \"" header "\"")
                        (concat "#include <" header ">"))))

    (beginning-of-buffer)
    (if (re-search-forward (concat "^ *// *\\(#include *[<\"][ \t]*" header "[ \t]*[>\"]\\)") nil t)
        (progn
          (replace-match "\\1")
          (when show-message
            (message (concat "commented in #include for " header))))
      
      (if (not (re-search-forward (concat "#include *[\"<][ \t]*" header "[ \t]*[\">]") nil t))
          (progn
                                        ; No include existed
            (goto-char (point-max)) ; Using end-of-buffer makes point move, despite save-excursion
            (if (not (re-search-backward "^#include *[\"<][^\">]+\.h *[\">]" nil t))
                (beginning-of-buffer)
              (progn (end-of-line) (forward-char 1)))
            
            ;; Now insert the header
            (insert (concat include-file "\n"))
            (when show-message
              (message (concat "inserted " include-file))))
        (when show-message
              (message (concat "header file \"" header "\" is already included")))))))



;----------------------------------------------------------------------------
; Insert a forward declaration for a Qt class.
; Place point anywhere on a Qt class, and invoke this function. A
; result of this is that a forward declaration line is added (if it does
; not already exist) for the given class.
;----------------------------------------------------------------------------
(defun kdab-insert-forward-decl ( prefix )
  (interactive "P")
  (save-excursion
    (let* ((word (if prefix (read-from-minibuffer "Class: ")
                   (current-word))))
      (beginning-of-buffer)
      (if (re-search-forward (concat "^ *// *\\(class *" word ";\\)") nil t)
          (progn
            (replace-match "\\1")
            (message (concat "commented in forward declaration for " word)))

        (if (not (re-search-forward (concat "class *" word ";") nil t))
            (progn
                                        ; No forward decl existed
              (goto-char (point-max)) ; Using end-of-buffer makes point move, despite save-excursion
              (if (re-search-backward "^[ \t]*class .*;" nil t)
                  (progn (end-of-line) (forward-char 1))
                                        ; No forward declarations found, lets search for include lines.
                (if (re-search-backward "#include" nil t)
                    (progn (end-of-line) (forward-char 1))
                  (beginning-of-buffer)))
              
              (progn
                (insert "class " word ";\n")
                (message (concat "inserted class " word ";"))))
          (message (concat "forward decl for \"" word "\" already exists")))))))
  

(defun is-qpe-class (class)
  (let ((list kdab-qpe-includes) classes (found nil))
    (while (and (not found) list)
      (setq classes (cdr (car list)))
      (while classes
        (if (string= (downcase (symbol-name (car classes))) (downcase class))
            (setq found 't))
        (setq classes (cdr classes)))
      (setq list (cdr list)))
    found))
        
;--------------------------------------------------------------------------------
; Start konqueror with documentation for the class under point.
; set `kdab-qt-documentation' and `kdab-qpe-documentation'
; to specify the replacement for the documentation
;--------------------------------------------------------------------------------
(defun kdab-lookup-qt-documentation ()
  (interactive "")
  (save-excursion
    (let* ((word (downcase (current-word)))
           (doc (if (is-qpe-class word) kdab-qpe-documentation kdab-qt-documentation))
           (url (if (not (string-match "XXX" doc))
                   (error "didn't find three X's in kdab-qt-documentation or kdab-qpe-documentation")
                 (replace-match word t t doc))))
      (start-process "qt documentation" nil "kfmclient" "openURL" url)
      (message (concat "Loading " url)))))

(provide 'klaralv)
