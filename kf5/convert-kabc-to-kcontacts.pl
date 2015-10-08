#!/bin/sh

# Aleix Pol Gonzalez <aleixpol@kde.org>

for FS in `find $PWD  -name '*.cpp' -or -name '*.h'`; do
    perl -p -i -e 's$#include <kabc/addressbook.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/addressbook.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/addresseedialog.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/addresseedialog.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/addresslineedit.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/addresslineedit.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/distributionlistdialog.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/distributionlistdialog.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/distributionlist.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/distributionlist.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/emailselectordialog.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/emailselectordialog.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/errorhandler.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/errorhandler.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/formatfactory.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/formatfactory.h>$$gi' $FS

    perl -p -i -e 's$#include <kabc/format.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/format.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/lock.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/lock.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/locknull.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/locknull.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/stdaddressbook.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/stdaddressbook.h>$$gi' $FS

    perl -p -i -e 's$#include <kabc/vcardformat.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/vcardformat.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/vcard.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/vcard.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/vcardline.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/vcardline.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/vcardparser.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/vcardparser.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/resource.h>$$gi' $FS
    perl -p -i -e 's$#include "kabc/resource.h"$$gi' $FS

    perl -p -i -e 's$#include <kabc/$#include <kcontacts/$gi' $FS
    perl -p -i -e 's$#include "kabc/$#include "kcontacts/$gi' $FS

    perl -p -i -e 's$KABC::$KContacts::$gi' $FS
    perl -p -i -e 's/namespace KABC/namespace KContacts/gi' $FS
done

for FS in `find $PWD  -name '*.cmake' -or -name 'CMakeLists.txt'`; do
    perl -p -i -e 's$find_package\(KF5Abc/$find_package(KF5Contacts$g' $FS
    perl -p -i -e 's$KF5::Abc$KF5::Contacts$g' $FS
done
