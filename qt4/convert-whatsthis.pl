#!/usr/bin/perl -pi

s!Q3WhatsThis::add\s*\(\s*([^,]*),!\1->setWhatsThis(!;
s!#include <q3whatsthis.h>!!;

# Q(V|H)GroupBox convertion
# TODO add #include <Q3GroupBox>
s!new\s*QVGroupBox\s*\(!new Q3GroupBox\(1, Qt::Horizontal,!;
s!new\s*QHGroupBox\s*\(!new Q3GroupBox\(1, Qt::Vertical,!;
s!QVGroupBox\s*\*!Q3GroupBox *!;
s!#include <qvgroupbox.h>!!;
