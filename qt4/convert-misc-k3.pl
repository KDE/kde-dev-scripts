#!/usr/bin/perl
# laurent Montel <montel@kde.org>
# Hamish Rodda <rodda@kde.org>
# Convert some K3 classes

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;

while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $nbLoop = 1;
    my $modified;
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        s!#include <kfiletreeview.h>!#include <k3filetreeview.h>!;
        s!#include "kfiletreeview.h"!#include "k3filetreeview.h"!;
        s!KFileTreeView!K3FileTreeView!g;
        s!#include <kfileiconview.h>!#include <k3fileiconview.h>!;
        s!#include "kfileiconview.h"!#include "k3fileiconview.h"!;
        s!KFileIconViewItem!K3FileIconViewItem!g;
        s!KFileIconView!K3FileIconView!g;
        s!#include <kfiledetailview.h>!#include <k3filedetailview.h>!;
        s!#include "kfiledetailview.h"!#include "k3filedetailview.h"!;
        s!KFileListViewItem!K3FileListViewItem!g;
        s!KFileDetailView!K3FileDetailView!g;
        s!#include <kfiletreeview.h>!#include <k3filetreeview.h>!;
        s!#include "kfiletreeview.h"!#include "k3filetreeview.h"!;
        s!KFileTreeView!K3FileTreeView!g;
        s!#include <klistbox.h>!#include <k3listbox.h>!;
        s!#include "klistbox.h"!#include "k3listbox.h"!;
        s!KListBox!K3ListBox!g;
        s!#include <kcompletionbox.h>!#include <k3completionbox.h>!;
        s!#include "kcompletionbox.h"!#include "k3completionbox.h"!;
        s!KCompletionBox!K3CompletionBox!g;
        s!#include <kfiletreeviewitem.h>!#include <k3filetreeviewitem.h>!;
        s!#include "kfiletreeviewitem.h"!#include "k3filetreeviewitem.h"!;


        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">$file");
        print $OUT @l;
    }
}
functionUtilkde::diffFile( "@ARGV" );
