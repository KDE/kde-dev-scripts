#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KImageIO::mimeTypes() => QImageReader/QImageWriter::supportedImageFormats()

# find -iname "*.cpp" -o -iname "*.h"|xargs kde-dev-scripts/kf5/convert-kimageio.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    my $needQImageReader;
    my $needQImageWriter;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        if (/KImageIO::Reading/) {
          s,KImageIO::mimeTypes\s*\(\s*KImageIO::Reading\s*\);,QImageReader::supportedImageFormats\(\);,;
          $needQImageReader = 1;
          warn "QImageReader::supportedImageFormats return a QList<QByteArray> need to adapt code\n";
        }
        if (/KImageIO::Writing/) {
          s,KImageIO::mimeTypes\s*\(\s*KImageIO::Writing\s*\);,QImageWriter::supportedImageFormats\(\);,;
          warn "QImageWriter::supportedImageFormats return a QList<QByteArray> need to adapt code\n";
          $needQImageWriter = 1;
        }
        
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ($needQImageReader) {
          functionUtilkde::addIncludeInFile($file, "QImageReader");
        }
        if ($needQImageWriter) {
          functionUtilkde::addIncludeInFile($file, "needQImageWriter");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
