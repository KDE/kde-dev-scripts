#!/bin/sh
egrep -rl 'findRev|local8Bit|utf8|simplifyWhiteSpace|stripWhiteSpace|ucs2|leftJustify|rightJustify|fromUcs2|constref|mirrored|changeInterval|absPath|convertToAbs|currentDirPath|homeDirPath|rootDirPath|cleanDirPath|absFilePath|dirPath|ShiftButton|ControlButton|AltButton|MetaButton|Keypad|KeyButtonMask|\.lower|\.upper' * |grep -v "\.svn" |grep -v "\.libs" | grep -v "\.o" | grep -v Makefile | grep -v Makefile.in  | grep -v "\.moc" | grep -v "\.lo" | grep -v "\.la" |grep -v "\.Ulo" |grep -v "\.kidl" | grep -v "\.desktop"
