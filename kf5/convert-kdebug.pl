#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# kDebug() -> qDebug()
# kWarning() -> qWarning()

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $infoVar;
    my $urlVar;

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        s/kDebug\(\s*\)/qDebug\(\)/;
        s/kdDebug\(\s*\)/qDebug\(\)/;
        s/kWarning\(\s*\)/qWarning\(\)/;
        s/kdWarning\(\s*\)/qWarning\(\)/;
        s/kError\(\s*\)/qCritical\(\)/;
        s/kFatal\(\s*\)/qFatal\(\)/;
        s/k_funcinfo/Q_FUNC_INFO/;

        s/\<kdebug\.h>/\<QDebug>/ if (/#include/);
        s/\<KDebug\>/\<QDebug>/ if (/#include/);

        #s/\<\< endl//; # old stuff

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
