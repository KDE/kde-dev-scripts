#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KPushButton => QPushButton
# find -iname "*.cpp" -o -iname "*.h" -o -iname "*.ui" |xargs kde-dev-scripts/kf5/convert-kpushbutton.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    my $needGuiItem;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        my $regexp = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (3) variable name
           \s*=\s*                       #   assignment
           new\s+KPushButton\s*\((.*)\)  # (4)  new KPushButton(...,...,...,...);
           (.*)$                         # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $argument, $afterreg) = $_ =~ $regexp) {
          #KPushButton( const KGuiItem &item, QWidget *parent = 0 );
          my ($kguiitem, $parent, $after);
          my $constructor_regexp = qr/
                                 ^(\s*KStandardGuiItem[^,]*)\s*        # kguitem
                                 (?:,\s([^,]*))?    # parent
                                 (.*)$              # after
                                 /x;
          if ( ($kguiitem, $parent, $after) = $argument =~ $constructor_regexp ) {
             warn "found a kpushbutton with kguiitem $kguiitem \n";
             if ($parent) {
               $_ = $indent . $left . $var . " = new QPushButton($parent);" . $after . "\n";
             } else {
               $_ = $indent . $left . $var . " = new QPushButton;" . $after . "\n";
             }
             $needGuiItem = 1;
             $_ .= $indent . "KGuiItem::assign($var,$kguiitem);" . $after . "\n";
          }
        }
        
        # support for ui.addButton->setGuiItem( KStandardGuiItem::add() );
        my $setGuiItemRegex = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)\-\>setGuiItem\(         # (2) variable name
           \s*(KStandardGuiItem::.*)     # (3) kstandardguiitem
           (.*)$                         # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $var, $kstandardguiitem, $after) = $_ =~ $setGuiItemRegex) {
           warn "found setGuiItem \"$kstandardguiitem\" \"$var\"\n";
           $_ = $indent . "KGuiItem::assign($var, $kstandardguiitem" . $after . "\n";
           $needGuiItem = 1;
        }
        s/\bKPushButton\b/QPushButton/g;
        s/\<KPushButton\b\>/\<QPushButton>/ =~ /#include/ ;
        s/\<kpushbutton.h\>/\<QPushButton>/ =~ /#include/ ;

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ($needGuiItem) {
           functionUtilkde::addIncludeInFile($file, "KGuiItem");
           functionUtilkde::addIncludeInFile($file, "KStandardGuiItem");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
