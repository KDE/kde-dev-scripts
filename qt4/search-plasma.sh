#!/bin/sh
egrep -rl '(KPanelApplet::Normal|KPanelApplet::Preferences|PanelApplet::About|KPanelApplet::Help)'  * |grep -v "\.svn" |grep -v "\.libs" | grep -v "\.o" | grep -v Makefile | grep -v Makefile.in  | grep -v "\.moc" | grep -v "\.lo" | grep -v "\.la"
