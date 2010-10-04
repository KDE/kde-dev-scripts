#!/usr/bin/perl

# Laurent Montel <montel@kde.org> 2005 GPL
# David Faure <faure@kde.org>
# This script ports everything in the current directory (and recursively) to the Qt4 API,
# for things that can be done automatically.
# Note that there are many other scripts for things that require manual tweaking afterwards,
# especially for things that would otherwise compile with QT3_SUPPORT so they don't need
# porting initially.

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

sub addQStringElement
{
    my $result = $_[0];
    if ( $result =~ /^\"/ ) {
	$result = "QString(" . $result . ")";
    }
    return $result;
}

open(my $F, q(find -name "*" |));
my $file;
my $warning;
my @files = ();
while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);
    my @necessaryIncludes = ();

    my $is_ui_file = 0;
    $is_ui_file = 1 if ( $file =~ /\.ui$/ );

    if (functionUtilkde::substInFile {

    if ( $_ =~ /Q3StyleSheet::escape/ ) {
    	s!Q3StyleSheet::escape!Qt::escape!g;
    	push(@necessaryIncludes, "QTextDocument");
    }
    if ( $_ =~ /Q3StyleSheet::convertFromPlainText/ ) {
        s!Q3StyleSheet::convertFromPlainText!Qt::convertFromPlainText!g;
        s!Q3StyleSheetItem::!Qt::!;
        push(@necessaryIncludes, "QTextDocument");
    }

    s!Qt::WordBreak!Qt::TextWordWrap!;
    s!Qt::SingleLine!Qt::TextSingleLine!;
    s!Qt::DontClip!Qt::TextDontClip!;
    s!Qt::ExpandTabs!Qt::TextExpandTabs!;
    s!Qt::ShowPrefix!Qt::TextShowMnemonic!;
    s!Qt::BreakAnywhere!Qt::TextWrapAnywhere!;
    s!Qt::DontPrint!Qt::TextDontPrint!;
    s!Qt::IncludeTrailingSpaces!Qt::TextIncludeTrailingSpaces!;
    s!Qt::NoAccel!Qt::TextHideMnemonic!;
    s!Qt::Key_BackSpace!Qt::Key_Backspace!;
    s!Qt::Key_BackTab!Qt::Key_Backtab!;
    s!Qt::Key_Prior!Qt::Key_PageUp!;
    s!Qt::Key_Next!Qt::Key_PageDown!;
    s!Qt::Key_MediaPrev([\s*|,])!Qt::Key_MediaPrevious\1!;

    s!Qt::arrowCursor!Qt::ArrowCursor!;
    s!Qt::upArrowCursor!Qt::UpArrowCursor!;
    s!Qt::crossCursor!Qt::CrossCursor!;
    s!Qt::waitCursor!Qt::WaitCursor!;
    s!Qt::ibeamCursor!Qt::IBeamCursor!;
    s!Qt::sizeVerCursor!Qt::SizeVerCursor!;
    s!Qt::sizeHorCursor!Qt::SizeHorCursor!;
    s!Qt::sizeBDiagCursor!Qt::SizeBDiagCursor!;
    s!Qt::sizeFDiagCursor!Qt::SizeFDiagCursor!;
    s!Qt::sizeAllCursor!Qt::SizeAllCursor!;
    s!Qt::blankCursor!Qt::BlankCursor!;
    s!Qt::splitVCursor!Qt::SplitVCursor!;
    s!Qt::splitHCursor!Qt::SplitHCursor!;
    s!Qt::pointingHandCursor!Qt::PointingHandCursor!;
    s!Qt::forbiddenCursor!Qt::ForbiddenCursor!;
    s!Qt::whatsThisCursor!Qt::WhatsThisCursor!;
    s!QScrollBar::Horizontal!Qt::Horizontal!g;
    s!QSlider::Horizontal!Qt::Horizontal!g; 
    s!QScrollBar::Vertical!Qt::Vertical!g;
    s!QSlider::Vertical!Qt::Vertical!g;


    s!\bQPen::PenStyle\b!Qt::PenStyle!g;
    s!QSlider::Below!QSlider::TicksBelow!;
    s!QSlider::Above!QSlider::TicksAbove!;

    # Qt3 name class
    #s!QIconSet!QIcon!g;
    s!QWMatrix!QMatrix!g;
    s!QGuardedPtr!QPointer!g;

    # Qt2-compat classes :)
    s!QArray!Q3MemArray!g;

    s!IO_ReadOnly!QIODevice::ReadOnly!;
    s!IO_WriteOnly!QIODevice::WriteOnly!;
    s!IO_ReadWrite!QIODevice::ReadWrite!;
    s!IO_Append!QIODevice::Append!;
    s!IO_Truncate!QIODevice::Truncate!;
    s!IO_Translate!QIODevice::Text!;

    s!Q_INT8!qint8!g;
    s!Q_UINT8!quint8!g;
    s!Q_INT16!qint16!g;
    s!Q_UINT16!quint16!g;
    s!Q_INT32!qint32!g;
    s!Q_UINT32!quint32!g;
    s!Q_INT64!qint64!g;
    s!Q_UINT64!quint64!g;
    s!Q_LLONG!qint64!g;
    s!Q_ULLONG!quint64!g;
    s!\bQMAX\b!qMax!g;
    s!\bQMIN\b!qMin!g;
    s!\bQABS\b!qAbs!g;

    s!QWidget::PaletteBase!Qt::PaletteBase!;
    s!Qt::ShiftButton!Qt::ShiftModifier!;
    s!ShiftButton!Qt::ShiftModifier!;
    s!Qt::ControlButton!Qt::ControlModifier!;
    s!ControlButton!Qt::ControlModifier!;
    s!Qt::AltButton!Qt::AltModifier!;
    s!AltButton!Qt::AltModifier!;
    s!Qt::MetaButton!Qt::MetaModifier!;
    s!MetaButton!Qt::MetaModifier!;
    s!Qt::Keypad!Qt::KeypadModifier!;
    s!\bKeypad\b!Qt::KeypadModifier!;
    s!Qt::KeyButtonMask!Qt::KeyboardModifierMask!;
    s!KeyButtonMask!Qt::KeyboardModifierMask!;

    s!QMouseEvent::LeftButton!Qt::LeftButton!;
    s!QMouseEvent::RightButton!Qt::RightButton!;
    s!QMouseEvent::MidButton!Qt::MidButton!;

    s!QWidget::NoFocus!Qt::NoFocus!g;

    s!convertToAbs!makeAbsolute!;
    s!currentDirPath!currentPath!;
    s!homeDirPath!homePath!;
    s!rootDirPath!rootPath!;
    s!cleanDirPath!cleanPath!;
    s!absFilePath!absoluteFilePath!;
    s!QDir::SortSpec!QDir::SortFlags!;
    s!QDir::All!QDir::TypeMask!;
    s!QDir::DefaultFilter!QDir::NoFilter!;
    s!QDir::DefaultSort!QDir::NoSort!;
    s!simplifyWhiteSpace!simplified!g;
    s!stripWhiteSpace!trimmed!g;
    s!\bucs2\b!utf16!g;
    s!leftJustify!leftJustified!g;
    s!rightJustify!rightJustified!g;
    s!fromUcs2!fromUtf16!g;
    s!constref!at!g;
    #s!changeInterval!start!g;

    s!Q3TextEdit::PlainText!Qt::PlainText!;
    s!\bQImage::ScaleMin\b!Qt::ScaleMin!;

    # When using qt3to4 with -strict (to avoid some nonsensical replacements of col.red() or enum { Top, Bottom })
    # we are then missing the following safer replacements:
    s!QWidget::WFlags!Qt::WFlags!;
    s!\bWFlags\b!Qt::WFlags! unless (/Qt::WFlags/);
    s!\bWStyle_!Qt::WStyle_!g unless (/Qt::WStyle_/);
    s!\bWType_!Qt::WType_!g unless (/Qt::WType_/);
    s!\bKey_!Qt::Key_!g unless (/Qt::Key_/);
    s!\bCTRL\b!Qt::CTRL!g unless (/Qt::CTRL/ || /_CTRL/ );
    s!\bSHIFT\b!Qt::SHIFT!g unless (/Qt::SHIFT/);
    s!\bLeftButton\b!Qt::LeftButton!g unless (/Qt::LeftButton/);
    s!\bRightButton\b!Qt::RightButton!g unless (/Qt::RightButton/);
    s!\bMidButton\b!Qt::MidButton!g unless (/Qt::MidButton/);
    s!QWidget::TabFocus!Qt::TabFocus!;
    s!\bTabFocus\b!Qt::TabFocus!g unless (/Qt::TabFocus/);
   

    s!QStyle::Style_Enabled!QStyle::State_Enabled!g;
    s!QStyle::Style_On!QStyle::State_On!g;
    s!QStyle::Style_Off!QStyle::State_Off!g; 
    s!QStyle::Style_NoChange!QStyle::State_NoChange!g;

    s!QWidget::StrongFocus!Qt::StrongFocus!g;
    s!QPushButton::StrongFocus!Qt::StrongFocus!g;
    s!StrongFocus!Qt::StrongFocus!g unless (/Qt::StrongFocus/ );

    s!QLabel::AlignLeft!Qt::AlignLeft!g;
    s!QLabel::AlignRight!Qt::AlignRight!g;
    s!QLabel::AlignCenter!Qt::AlignCenter!g;
    s!QLabel::AlignHCenter!Qt::AlignHCenter!g;
    s!QLabel::AlignVCenter!Qt::AlignVCenter!g;
    s!QLabel::AlignTop!Qt::AlignTop!g;
    s!QLabel::AlignBottom!Qt::AlignBottom!g;
    s!\bAlignmentFlags\b!Qt::Alignment!g unless (/Qt::Alignment/);

    if (!$is_ui_file) { # uic/cpp/cppwriteinitialization.cpp prepends Qt:: automatically
        s!\bSolidPattern\b!Qt::SolidPattern!g unless ( /Qt::SolidPattern/ );
        s!Dense1Pattern!Qt::Dense1Pattern!g unless ( /Qt::Dense1Pattern/ );
        s!Dense2Pattern!Qt::Dense2Pattern!g unless ( /Qt::Dense2Pattern/ );
        s!Dense3Pattern!Qt::Dense3Pattern!g unless ( /Qt::Dense3Pattern/ );
        s!Dense4Pattern!Qt::Dense4Pattern!g unless ( /Qt::Dense4Pattern/ );
        s!Dense5Pattern!Qt::Dense5Pattern!g unless ( /Qt::Dense5Pattern/ );
        s!Dense6Pattern!Qt::Dense6Pattern!g unless ( /Qt::Dense6Pattern/ );
        s!Dense7Pattern!Qt::Dense7Pattern!g unless ( /Qt::Dense7Pattern/ );
        s!\bHorPattern\b!Qt::HorPattern!g unless ( /Qt::HorPattern/ );
        s!\bVerPattern\b!Qt::VerPattern!g unless ( /Qt::VerPattern/ );
        s!\bCrossPattern\b!Qt::CrossPattern!g unless ( /Qt::CrossPattern/ );
        s!\bBDiagPattern\b!Qt::BDiagPattern!g unless ( /Qt::BDiagPattern/ );
        s!\bFDiagPattern\b!Qt::FDiagPattern!g unless ( /Qt::FDiagPattern/ );
        s!\bDiagCrossPattern\b!Qt::DiagCrossPattern!g unless ( /Qt::DiagCrossPattern/ );
        s!\bNoBrush\b!Qt::NoBrush!g unless (/Qt::NoBrush/);
    }

    s!\bSolidLine\b!Qt::SolidLine!g unless ( /Qt::SolidLine/ );

    s!QBrush::NoBrush!Qt::NoBrush!g;
    s!QButton::NoChange!QCheckBox::NoChange!g;

    s!QObject::AlignLeft!Qt::AlignLeft!g;
    s!QObject::AlignRight!Qt::AlignRight!g;
    s!QObject::AlignCenter!Qt::AlignCenter!g;
    s!QObject::AlignHCenter!Qt::AlignHCenter!g;
    s!QObject::AlignVCenter!Qt::AlignVCenter!g;
    s!QObject::AlignTop!Qt::AlignTop!g;
    s!QObject::AlignBottom!Qt::AlignBottom!g;

    s!Q3ListViewItem::AlignLeft!Qt::AlignLeft!g;
    s!Q3ListViewItem::AlignRight!Qt::AlignRight!g;
    s!Q3ListViewItem::AlignCenter!Qt::AlignCenter!g;
    s!Q3ListViewItem::AlignHCenter!Qt::AlignHCenter!g;
    s!Q3ListViewItem::AlignVCenter!Qt::AlignVCenter!g;
    s!Q3ListViewItem::AlignTop!Qt::AlignTop!g;
    s!Q3ListViewItem::AlignBottom!Qt::AlignBottom!g;


    s!AlignLeft!Qt::AlignLeft!g unless (/Qt::AlignLeft/ || /"Align/ || /AlignLeft\(.*\)/);
    s!AlignRight!Qt::AlignRight!g unless (/Qt::AlignRight/ || /"Align/ || /AlignRight\(.*\)/);
    s!AlignCenter!Qt::AlignCenter!g unless (/Qt::AlignCenter/ || /"Align/ || /AlignCenter\(.*\)/);
    s!AlignHCenter!Qt::AlignHCenter!g unless (/Qt::AlignHCenter/ || /"Align/ || /AlignHCenter\(.*\)/);
    s!AlignVCenter!Qt::AlignVCenter!g unless (/Qt::AlignVCenter/ || /"Align/ || /AlignVCenter\(.*\)/);
    s!AlignTop!Qt::AlignTop!g unless (/Qt::AlignTop/ || /"Align/ || /AlignTop\(.*\)/);
    s!AlignBottom!Qt::AlignBottom!g unless (/Qt::AlignBottom/ || /"Align/ || /AlignBottom\(.*\)/);
    
    s!QCursor::WaitCursor!Qt::waitCursor!g;
    s!QSplitter::Vertical!Qt::Vertical!g;
    s!QSplitter::Horizontal!Qt::Horizontal!g;

    s!\bQSqlError::Connection\b!QSqlError::ConnectionError!g;
    s!QSqlError::None!QSqlError::NoError!g;
    s!\bQSqlError::Statement\b!QSqlError::StatementError!g;
    s!\bQSqlError::Transaction\b!QSqlError::TransactionError!g;
    s!\bQSqlError::Unknown\b!QSqlError::UnknownError!g;


    if (/app/i) {
	s!flushX!flush!; # QApplication
    }
    s!qt_xdisplay\s*\(\s*\)!QX11Info::display()!;
    s!qt_xrootwin\s*\(\s*\)!QX11Info::appRootWindow()!;
    s!qt_x_time!QX11Info::appTime()!;
    if (/QX11Info/) {
        push(@necessaryIncludes, "QX11Info");
    }
    if (/qHeapSort/) {
	push(@necessaryIncludes, "q3tl.h");
    }

    s!class QWidgetList;!typedef QList<QWidget *> QWidgetList;!;

    # this changes usage of QObjectList, since queryList returns QObjectList and not a pointer in qt4.
    s!QObjectList\s*\*!QObjectList ! if (/queryList/);

    # this changes QStringList::split (QT3_SUPPORT) to QString::split (Qt4)
    # but it's a bit broken, e.g. with split(',') or split(" ",tr("foo"))
    # or someCall(QStringList::split("/", path), parent)
    if (0) {
    if (my ($blank, $prefix, $contenu) = m!^(\s*.*)(QStringList::split.*)\((.*)\s*\);$!) {
	#warn "blank : $blank, prefix : $prefix, contenu : $contenu \n";
	if ( my ($firstelement, $secondelement, $thirdelement) = m!.*?\(\s*(.*),\s*(.*),\s*(.*)\);\s*$!) {
	    #warn "three elements: first: $firstelement second: $secondelement third: $thirdelement \n";
	    my $argument = $prefix;
	    # Remove space before argument
	    $secondelement =~ s/ //g;	
	    $thirdelement =~ s/ //g;
					
	    $secondelement = addQStringElement( $secondelement );
	    if ( $blank =~ /insertStringList/ ) {
		$thirdelement =~ s/\)//g;
		if ( $thirdelement =~ /true/ ) {
		    #QString::KeepEmptyParts
		    $_ = $blank . $secondelement . ".split( " . $firstelement . ", QString::KeepEmptyParts" . "));\n" ;
		} elsif ( $thirdelement =~ /false/ ) {
		    #QString::SkipEmptyParts
		    $_ = $blank . $secondelement . ".split( " . $firstelement . ", QString::SkipEmptyParts" . "));\n" ;
		}
		# different element
		else {
		    $_ = $blank . $secondelement . ".split( " . $firstelement . ", $thirdelement" . "));\n" ;
		}	
	    } else {
		if ( $thirdelement =~ /true/ ) {
		    #QString::KeepEmptyParts
		    $_ = $blank . $secondelement . ".split( " . $firstelement . ", QString::KeepEmptyParts" . ");\n" ;	
		} elsif ( $thirdelement =~ /false/ ) {
		    #QString::SkipEmptyParts
		    $_ = $blank . $secondelement . ".split( " . $firstelement . ", QString::SkipEmptyParts" . ");\n" ;
		}
		# different element
		else {
		    $_ = $blank . $secondelement . ".split( " . $firstelement . ", $thirdelement" . ");\n" ;
		}
	    }

	} elsif ( my ($firstelement, $secondelement) = m!.*?\(\s*(.*),\s*(.*)\);\s*$!) {
	    # warn "two elements: first: $firstelement second: $secondelement \n";
	    my $argument = $prefix;
	    # Remove space after argument
	    $secondelement =~ s/\s+$//;
	    $secondelement = addQStringElement( $secondelement );
	    if ($firstelement !~ /QRegExp/ ) { # What to do about QRegExp?
		$_ = $blank . $secondelement . ".split( " . $firstelement . " );\n" ;
	    }
    	}
    }
    }

    } $file) { push(@files,$file); }

    my %alreadyadded = {};
    foreach my $inc (@necessaryIncludes) {
        next if (defined $alreadyadded{$inc});
        $alreadyadded{$inc} = 1;
        functionUtilkde::addIncludeInFile( $file, $inc );
    }
}
functionUtilkde::diffFile( @files );
warn "Warning: $warning\n" if ($warning != "");
