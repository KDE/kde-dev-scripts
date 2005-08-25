#!/bin/sh
egrep -rl '(KPanelApplet::ReportBug|KPanelApplet::pLeft|KPanelApplet::pRight!KPanelApplet::pTop!KPanelApplet::pBottom!KPanelApplet::Normal|KPanelApplet::Preferences|PanelApplet::About|KPanelApplet::Help|#include <kpanelapplet.h>|KPanelApplet::Stretch)'  * | egrep -v '\.(svn|libs|o|moc|l[ao])|Makefile(.in)?' 
