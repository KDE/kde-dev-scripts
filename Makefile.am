
bin_SCRIPTS = create_makefile create_makefiles cvs2pack cvschanged \
	cvslastchange cvslastlog cvsversion cxxmetric extend_dmalloc \
	gendebug kdedoc qtdoc kdelnk2desktop.py

# not installed : extractrc fixheaders

#doc: cvs2pack.1 cvs2pack.txt

man1_DATA = cvs2pack.1 cvschanged.1 cvsversion.1
man1dir = $(mandir)/man1

#cvs2pack.1: cvs2pack
	#pod2man -center="K Desktop Environment" cvs2pack > cvs2pack.1

#cvs2pack.txt: cvs2pack
	#pod2text cvs2pack > cvs2pack.txt

#cvschanged.1: $(srcdir)/cvschanged
	#pod2man -center="K Desktop Environment" cvschanged > cvschanged.1

#cvsversion.1: $(srcdir)/cvsversion
	#pod2man -lax -center="K Desktop Environment" cvsversion > cvsversion.1

