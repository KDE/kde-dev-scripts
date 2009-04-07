
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
  svnchangesince
  svngettags
  svnlastlog
  svnrevertlast
  wcgrep
  DESTINATION ${BIN_INSTALL_DIR})
endif (UNIX)

install( FILES kdesvn-buildrc.xml  DESTINATION  ${DATA_INSTALL_DIR}/katepart/syntax )
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
  kdesvn-build 
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
  DESTINATION ${BIN_INSTALL_DIR})
 
