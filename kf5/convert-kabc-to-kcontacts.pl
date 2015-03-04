#!/bin/sh

# Aleix Pol Gonzalez <aleixpol@kde.org>

for FS in `find $PWD  -name '*.cpp' -or -name '*.h'`; do
    perl -p -i -e 's$#include <KABC/$#include <KContacts/$g' $FS
    perl -p -i -e 's$#include <kabc/$#include <kcontacts/$g' $FS
    perl -p -i -e 's$#include "kabc/$#include "kcontacts/$g' $FS
    perl -p -i -e 's$KABC::$KContacts::$g' $FS
    perl -p -i -e 's/namespace KABC/namespace KContacts/g' $FS
done

for FS in `find $PWD  -name '*.cmake' -or -name 'CMakeLists.txt'`; do
    perl -p -i -e 's$find_package\(KF5Abc/$find_package(KF5Contacts$g' $FS
    perl -p -i -e 's$KF5::Abc$KF5::Contacts$g' $FS
done
