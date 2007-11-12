#!/usr/bin/perl -pi

s!Q(3|)WhatsThis::add\s*\(\s*([^,]*),!\2->setWhatsThis(!g;
s!#include <q3whatsthis.h>!!g;

s!QToolTip::add\s*\(\s*([^,]*),!\1->setToolTip(!;


# Q(V|H)GroupBox convertion
# TODO add #include <Q3GroupBox>
s!new\s*QVGroupBox\s*\(!new Q3GroupBox\(1, Qt::Horizontal,!g;
s!new\s*QHGroupBox\s*\(!new Q3GroupBox\(1, Qt::Vertical,!g;
s!QVGroupBox\s*\*!Q3GroupBox *!g;
s!#include <qvgroupbox.h>!!g;
