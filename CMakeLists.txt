cmake_minimum_required(VERSION 2.8)
project(kde-dev-scripts)

find_package(KDE4)
if (KDE4_FOUND)
  include(KDE4Defaults)
  include(MacroLibrary)
else()
  set(BIN_INSTALL_DIR   "${CMAKE_INSTALL_PREFIX}/bin"  CACHE PATH "The install dir for executables")
endif()

# Note that all of these files should be *scripts* that do not require
# compilation, therefore we simply install them. We split out Bourne shell
# programs explicitly but it's not guaranteed that the Perl/Python/Ruby/etc.
# ones behave well on Mac or Windows just because we don't elide those too.

########### install files ###############
if (UNIX)
#shell program
add_subdirectory( doc )
install(PROGRAMS
  adddebug
  build-progress.sh
  cheatmake
  create_cvsignore
  create_makefiles
  create_svnignore
  cvsaddcurrentdir
  cvsbackport
  cvsforwardport
  cvslastlog
  cvsrevertlast
  cvsversion
  findmissingcrystal
  fix-include.sh
  kdedoc
  krazy-licensecheck
  optimizegraphics
  nonsvnlist
  pruneemptydirs
  qtdoc
  svnbackport
  svnforwardport
  svnchangesince
  svngettags
  svnlastlog
  svnrevertlast
  wcgrep
  DESTINATION ${BIN_INSTALL_DIR})
endif (UNIX)

install(PROGRAMS
  create_makefile
  makeobj
  cvscheck
  cvslastchange
  noncvslist
  cvs-clean
  cvsblame
  colorsvn
  svn-clean
  svnlastchange
  svnversions
  extractrc
  extractattr
  zonetab2pot.py
  fixkdeincludes
  fixuifiles
  includemocs
  cxxmetric
  extend_dmalloc
  kdekillall
  kdelnk2desktop.py
  package_crystalsvg
  png2mng.pl
  kdemangen.pl
  svnintegrate
  kde_generate_export_header
  kde-systemsettings-tree.py
  DESTINATION ${BIN_INSTALL_DIR})

