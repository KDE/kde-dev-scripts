
########### install files ###############
if (UNIX)
#shell program
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
  extractqml 
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
 
