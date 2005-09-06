# sorted by function
bin_SCRIPTS = \
  create_makefile create_makefiles adddebug \
  cheatmake makeobj kde-build build-progress.sh pruneemptydirs \
  cvsbackport cvsversion cvscheck cvslastchange cvslastlog cvsrevertlast \
  noncvslist cvs-clean cvs2dist cvsblame cvsforwardport create_cvsignore \
  colorsvn create_svnignore nonsvnlist svn2dist svnaddcurrentdir svnbackport svnforwardport \
  svn-clean svngettags svnlastchange svnlastlog svnrevertlast svnversions \
  svnchangesince findmissingcrystal kdesvn-build \
  kdedoc qtdoc extractrc extractattr zonetab2pot.py \
  licensecheck fixkdeincludes fixuifiles includemocs \
  cxxmetric extend_dmalloc kdekillall kdelnk2desktop.py \
  package_crystalsvg png2mng.pl kdemangen.pl

# Install syntax highlighting file for KWrite/Kate.
kdesvn_build_syntaxdir = $(kde_datadir)/katepart/syntax
kdesvn_build_syntax_DATA = kdesvn-buildrc.xml

# sorted by bin_SCRIPTS reference
man_MANS = \
  kde-build.1 \
  kdesvn-build.1 \
  cvsversion.1 cvscheck.1 \
  noncvslist.1 \
  cvsblame.1 \
  includemocs.1 

CLEANFILES = $(man_MANS)

# sorted alphabetically
noinst_SCRIPTS = \
  add_trace.pl alldcop.rb authors2xml.pl check_licenses colorcvs \
  cvsaddcurrentdir fixheaders kDebug2kdDebug.sh

EXTRA_DIST = completions kde-emacs \
  colorcvsrc-sample gettext.patch kde-buildrc \
  kde-devel-emacs.el kde-devel-gdb kde-devel-vim.vim \
  kde.supp kdesvn-buildrc-sample

# it'd be too good if this worked everywhere ...
#%.1: %
#	pod2man $< $@

kde-build.1: kde-build
	pod2man $(srcdir)/kde-build > $@
kdesvn-build.1: kdesvn-build
	pod2man $(srcdir)/kdesvn-build > $@
cvsversion.1: cvsversion
	pod2man $(srcdir)/cvsversion > $@
cvscheck.1: cvscheck
	pod2man $(srcdir)/cvscheck > $@
noncvslist.1: noncvslist
	pod2man $(srcdir)/noncvslist > $@
cvsblame.1: cvsblame
	pod2man $(srcdir)/cvsblame > $@
includemocs.1: includemocs
	pod2man $(srcdir)/includemocs > $@
