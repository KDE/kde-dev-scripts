# Convert the KDE-specific mimetype names to XDG.

# Use this script like this:  (with zsh)
# perl -pi $KDESRC/4/kdesdk/scripts/qt4/fix-mimetypes.pl **/*.desktop

# You might want to also apply it on the codebase itself, in case it's mentionning mimetypes by name.

# Script by: David Faure <faure@kde.org>
# Mimetype renaming list by: Pino Toscano <toscano.pino@tiscali.it>

s,application/chm([^a-z-]),application/x-chm$1,g;
s,application/fits([^a-z-]),image/x-fits$1,g;
s,application/java([^a-z-]),application/x-java$1,g;
s,application/mathml+xml([^a-z-]),text/mathml$1,g;
s,application/msexcel([^a-z-]),application/vnd.ms-excel$1,g;
s,application/mspowerpoint([^a-z-]),application/vnd.ms-powerpoint$1,g;
s,application/ms-tnef([^a-z-]),application/vnd.ms-tnef$1,g;
s,application/pgp([^a-z-]),application/pgp-encrypted$1,g;
s,application/vnd.ms-asf([^a-z-]),video/x-ms-asf$1,g;
s,application/vnd.ms-word([^a-z-]),application/msword$1,g;
s,application/vnd.palm([^a-z-]),application/x-palm-database$1,g;
s,application/vnd.stardivision.writer-global([^a-z-]),application/vnd.stardivision.writer$1,g;
s,application/vnd.sun.xml.base([^a-z-]),application/vnd.oasis.opendocument.database$1,g;
s,application/vnd.sun.xml.writer.master([^a-z-]),application/vnd.sun.xml.writer.global$1,g;
s,application/wordperfect([^a-z-]),application/vnd.wordperfect$1,g;
s,application/x-7z([^a-z-]),application/x-7z-compressed$1,g;
s,application/x-afm([^a-z-]),application/x-font-afm$1,g;
s,application/x-applixgraphics([^a-z-]),image/x-applix-graphics$1,g;
s,application/x-applixspread([^a-z-]),application/x-applix-spreadsheet$1,g;
s,application/x-applixword([^a-z-]),application/x-applix-word$1,g;
s,application/x-bz2dvi([^a-z-]),application/x-bzdvi$1,g;
s,application/x-bzip2([^a-z-]),application/x-bzip$1,g;
s,application/x-dbase([^a-z-]),application/x-dbf$1,g;
s,application/x-font-ghostscript([^a-z-]),application/x-font-type1$1,g;
s,application/x-font-ttc([^a-z-]),application/x-font-ttf$1,g;
s,application/x-gettext([^a-z-]),text/x-gettext-translation$1,g;
s,application/x-hancomword([^a-z-]),application/x-hwp$1,g;
s,application/x-iso([^a-z-]),application/x-cd-image$1,g;
s,application/x-jar([^a-z-]),application/x-java-archive$1,g;
s,application/x-javascript([^a-z-]),application/javascript$1,g;
s,application/x-msaccess([^a-z-]),application/vnd.ms-access$1,g;
s,application/x-msdos-program([^a-z-]),application/x-ms-dos-executable$1,g;
s,application/x-msmetafile([^a-z-]),image/x-wmf$1,g;
s,application/x-ogg([^a-z-]),application/ogg$1,g;
s,application/x-perl-module([^a-z-]),application/x-perl$1,g;
s,application/x-python([^a-z-]),text/x-python$1,g;
s,application/x-rar-compressed([^a-z-]),application/x-rar$1,g;
s,application/x-tbz([^a-z-]),application/x-bzip-compressed-tar$1,g;
s,application/x-tgz([^a-z-]),application/x-compressed-tar$1,g;
s,application/x-troff([^a-z-]),text/troff$1,g;
s,application/x-zip([^a-z-]),application/zip$1,g;
s,application/x-zip-compressed([^a-z-]),application/zip$1,g;
s,application/xml-dtd([^a-z-]),text/x-dtd$1,g;
s,audio/mpegurl([^a-z-]),audio/x-mpegurl$1,g;
s,audio/x-midi([^a-z-]),audio/midi$1,g;
s,audio/x-mp3([^a-z-]),audio/mpeg$1,g;
s,audio/x-oggflac([^a-z-]),audio/x-flac+ogg$1,g;
s,audio/x-pn-realaudio([^a-z-]),audio/vnd.rn-realaudio$1,g;
s,audio/x-speex([^a-z-]),audio/x-speex+ogg$1,g;
s,audio/x-vorbis([^a-z-]),audio/x-vorbis+ogg$1,g;
s,audio/vorbis([^a-z-]),audio/x-vorbis+ogg$1,g;
s,image/fits([^a-z-]),image/x-fits$1,g;
s,image/jp2([^a-z-]),image/jpeg2000$1,g;
s,image/jpg([^a-z-]),image/jpeg$1,g;
s,image/pjpeg([^a-z-]),image/jpeg$1,g;
s,image/svg-xml([^a-z-]),image/svg+xml$1,g;
s,image/x-bmp([^a-z-]),image/bmp$1,g;
s,image/x-djvu([^a-z-]),image/vnd.djvu$1,g;
s,image/x-portable-greymap([^a-z-]),image/x-portable-graymap$1,g;
s,image/x-raw([^a-z-]),image/x-dcraw$1,g;
s,image/x-targa([^a-z-]),image/x-tga$1,g;
s,image/x-vnd.adobe.photoshop([^a-z-]),image/x-psd$1,g;
s,image/x-xbm([^a-z-]),image/x-xbitmap$1,g;
s,image/x-xcf-gimp([^a-z-]),image/x-xcf$1,g;
s,image/x-xpm([^a-z-]),image/x-xpixmap$1,g;
s,text/docbook([^a-z-]),application/docbook+xml$1,g;
s,text/javascript([^a-z-]),application/javascript$1,g;
s,text/rss([^a-z-]),application/rss+xml$1,g;
s,text/rtf([^a-z-]),application/rtf$1,g;
s,text/x-csv([^a-z-]),text/csv$1,g;
s,text/x-diff([^a-z-]),text/x-patch$1,g;
s,text/x-latex([^a-z-]),text/x-tex$1,g;
s,text/xml([^a-z-]),application/xml$1,g;
s,text/x-mswinurl([^a-z-]),application/x-mswinurl$1,g;
s,text/x-vcalendar([^a-z-]),text/calendar$1,g;
s,text/x-vcard([^a-z-]),text/directory$1,g;
s,text/x-xslt([^a-z-]),application/xslt+xml$1,g;
s,video/avi([^a-z-]),video/x-msvideo$1,g;
s,video/x-ogm([^a-z-]),video/x-ogm+ogg$1,g;
s,video/x-theora([^a-z-]),video/x-theora+ogg$1,g;
