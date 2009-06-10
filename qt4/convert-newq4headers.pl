#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2005 GPL
# This function allows to adapt file to new Qt4 includes

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;
my $warning;
while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $modified;
    my @necessaryIncludes = ();
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
    my $orig = $_;

    


    s!#include <qaccessible.h>!#include <QAccessible>!;
    s!#include <qapplication.h>!#include <QApplication>!;
#     s!#include <qarray.h>!#include <QArray>!;
    s!#include <qasciidict.h>!#include <Q3AsciiDict>!;
    s!#include <qbitarray.h>!#include <QBitArray>!;
    s!#include <qbitmap.h>!#include <QBitmap>!;
    s!#include <qboxlayout.h>!#include <QBoxLayout>!;
    s!#include <qbrush.h>!#include <QBrush>!;
    s!#include <qbuffer.h>!#include <QBuffer>!;
    s!#include <qbuttongroup.h>!#include <QButtonGroup>!;
    s!#include <qbytearray.h>!#include <QByteArray>!;
    s!#include <qcheckbox.h>!#include <QCheckBox>!;
    s!#include <qclipboard.h>!#include <QClipboard>!;
    s!#include <qcolordialog.h>!#include <QColorDialog>!;
    s!#include <qcombobox.h>!#include <QComboBox>!;
    s!#include <qcoreapplication.h>!#include <QCoreApplication>!;
    s!#include <qcstring.h>!#include <Q3CString>!;
    s!#include <qcursor.h>!#include <QCursor>!;
    s!#include <qdatetime.h>!#include <QDateTime>!;
    s!#include <qdialog.h>!#include <QDialog>!;
    s!#include <qdir.h>!#include <QDir>!;
#     s!#include <qdom.h>!#include <QDom>!;
#     s!#include <qdrawutil.h>!#include <QDrawUtil>!;
    s!#include <qevent.h>!#include <QEvent>!;
    s!#include <qeventloop.h>!#include <QEventLoop>!;
    s!#include <qfile.h>!#include <QFile>!;
    s!#include <qfiledialog.h>!#include <QFileDialog>!;
    s!#include <qfileinfo.h>!#include <QFileInfo>!;
    s!#include <qfontdatabase.h>!#include <QFontDatabase>!;
    s!#include <qfontmetrics.h>!#include <QFontMetrics>!;
#     s!#include <qglobal.h>!#include <QtGlobal>!;
    s!#include <qgridlayout.h>!#include <QGridLayout>!;
    s!#include <qgroupbox.h>!#include <QGroupBox>!;
    s!#include <qhash.h>!#include <QHash>!;
    s!#include <qhbox.h>!#include <Q3HBox>!;
    s!#include <qhbuttongroup.h>!#include <Q3HButtonGroup>!;
    s!#include <qheader.h>!#include <Q3Header>!;
    s!#include <qicon.h>!#include <QIcon>!;
    s!#include <qimage.h>!#include <QImage>!;
    s!#include <qiodevice.h>!#include <QIODevice>!;
    s!#include <qlabel.h>!#include <QLabel>!;
    s!#include <qlayout.h>!#include <QLayout>!;
    s!#include <qlineedit.h>!#include <QLineEdit>!;
    s!#include <qlist.h>!#include <QList>!;
    s!#include <qlistview.h>!#include <QListView>!;
    s!#include <qmatrix.h>!#include <QMatrix>!;
    s!#include <qmenu.h>!#include <QMenu>!;
    s!#include <qmessagebox.h>!#include <QMessageBox>!;
    s!#include <qmetaobject.h>!#include <QMetaObject>!;
    s!#include <qmovie.h>!#include <QMovie>!;
    s!#include <qmutex.h>!#include <QMutex>!;
#     s!#include <qnamespace.h>!#include <QNameSpace>!;
    s!#include <qobject.h>!#include <QObject>!;
    s!#include <qpaintdevicemetrics.h>!#include <Q3PaintDeviceMetrics>!;
    s!#include <qpainter.h>!#include <QPainter>!;
    s!#include <qpair.h>!#include <QPair>!;
    s!#include <qpalette.h>!#include <QPalette>!;
    s!#include <qpen.h>!#include <QPen>!;
    s!#include <qpixmap.h>!#include <QPixmap>!;
#     s!#include <qplugin.h>!#include <QPlugin>!;
    s!#include <qpointer.h>!#include <QPointer>!;
    s!#include <qprinter.h>!#include <QPrinter>!;
    s!#include <qprocess.h>!#include <QProcess>!;
    s!#include <qptrlist.h>!#include <Q3PtrList>!;
    s!#include <qpushbutton.h>!#include <QPushButton>!;
    s!#include <qradiobutton.h>!#include <QRadioButton>!;
    s!#include <qrect.h>!#include <QRect>!;
    s!#include <qregexp.h>!#include <QRegExp>!;
    s!#include <qsignalmapper.h>!#include <QSignalMapper>!;
    s!#include <qsize.h>!#include <QSize>!;
    s!#include <qsizepolicy.h>!#include <QSizePolicy>!;
    s!#include <qslider.h>!#include <QSlider>!;
    s!#include <qsocketnotifier.h>!#include <QSocketNotifier>!;
    s!#include <qsplitter.h>!#include <QSplitter>!;
    s!#include <qstring.h>!#include <QString>!;
    s!#include <qstringlist.h>!#include <QStringList>!;
    s!#include <qstyle.h>!#include <QStyle>!;
    s!#include <qsyntaxhighlighter.h>!#include <QSyntaxHighlighter>!;
    s!#include <qtable.h>!#include <Q3Table>!;
    s!#include <qtabwidget.h>!#include <QTabWidget>!;
    s!#include <qtextcodec.h>!#include <QTextCodec>!;
    s!#include <qtextcodecplugin.h>!#include <QTextCodecPlugin>!;
    s!#include <qtextdocument.h>!#include <QTextDocument>!;
    s!#include <qtextedit.h>!#include <QTextEdit>!;
    s!#include <qtextstream.h>!#include <QTextStream>!;
    s!#include <qthread.h>!#include <QThread>!;
    s!#include <qtimer.h>!#include <QTimer>!;
    s!#include <qtoolbutton.h>!#include <QToolButton>!;
    s!#include <qtooltip.h>!#include <QToolTip>!;
    s!#include <qurl.h>!#include <QUrl>!;
#     s!#include <qutf7codec.h>!#include <Qutf7codec>!;
    s!#include <qvalidator.h>!#include <QValidator>!;
    s!#include <qvaluelist.h>!#include <Q3ValueList>!;
    s!#include <qvaluestack.h>!#include <Q3ValueStack>!;
    s!#include <qvariant.h>!#include <QVariant>!;
    s!#include <qvector.h>!#include <QVector>!;
    s!#include <qwhatsthis.h>!#include <QWhatsThis>!;
    s!#include <qwidgetstack.h>!#include <Q3WidgetStack>!;
    s!#include <qxml.h>!#include <QtXml>!;
    s!#include <qtranslator.h>!#include <QTranslator>!;
    s!#include <qstatusbar.h>!#include <QStatusBar>!;
    s!#include <qaction.h>!#include <QAction>!;
    s!#include <qcolor.h>!#include <QColor>!;
    s!#include <qmenubar.h>!#include <QMenuBar>!;

    $modified ||= $orig ne $_;
    $_;
    } <$FILE>;

    if ($modified) {
      open (my $OUT, ">$file");
      print $OUT @l;
    }

    my %alreadyadded = {};
    foreach my $inc (@necessaryIncludes) {
      next if (defined $alreadyadded{$inc});
      $alreadyadded{$inc} = 1;

      functionUtilkde::addIncludeInFile( $file, $inc );
    }
}
functionUtilkde::diffFile( <$F> );
warn "Warning: $warning\n";
