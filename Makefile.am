# sorted by function
bin_SCRIPTS = \
  create_makefile create_makefiles adddebug \
  cheatmake makeobj kde-build build-progress.sh \
  cvsversion cvscheck cvslastchange cvslastlog cvsrevertlast \
  noncvslist pruneemptydirs cvs-clean \
  cvs2pack cvs2dist  cvsblame \
  kdedoc qtdoc extractrc zonetab2pot.py \
  licensecheck fixincludes includemocs create_cvsignore \
  cxxmetric extend_dmalloc kdekillall kdelnk2desktop.py

# sorted by bin_SCRIPTS reference
man1_MANS = \
  kde-build.1 \
  cvsversion.1 cvscheck.1 \
  noncvslist.1 \
  cvs2pack.1  cvsblame.1 \
  includemocs.1 

CLEANFILES = $(man1_MANS)

# sorted alphabetically
noinst_SCRIPTS = \
  add_trace.pl alldcop.rb authors2xml.pl check_licenses colorcvs \
  cvsaddcurrentdir fixheaders kDebug2kdDebug.sh kdecompile

EXTRA_DIST = completions kde-emacs \
  colorcvsrc-sample cvschanged gettext.patch kde-buildrc \
  kde-devel-emacs.el kde-devel-gdb kde-devel-vim.vim \
  kde.supp kdecompile.README 

%.1: %
	pod2man $< $@
