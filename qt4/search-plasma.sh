#!/bin/sh
egrep -rl '(KPanelApplet::pLeft|KPanelApplet::pRight!KPanelApplet::pTop!KPanelApplet::pBottom!KPanelApplet::Normal|KPanelApplet::Preferences|PanelApplet::About|KPanelApplet::Help|#include <kpanelapplet.h>|KPanelApplet::Stretch)'  * |grep -v "\.svn" |grep -v "\.libs" | grep -v "\.o" | grep -v Makefile | grep -v Makefile.in  | grep -v "\.moc" | grep -v "\.lo" | grep -v "\.la"
