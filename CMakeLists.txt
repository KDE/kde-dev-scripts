
########### install files ###############

install( FILES kdesvn-buildrc.xml  DESTINATION  ${DATA_INSTALL_DIR}/katepart/syntax )
install(PROGRAMS 
  create_makefile create_makefiles adddebug 
  cheatmake makeobj  build-progress.sh pruneemptydirs 
  cvsbackport cvsversion cvscheck cvslastchange cvslastlog cvsrevertlast 
  noncvslist cvs-clean cvs2dist cvsblame cvsforwardport create_cvsignore 
  colorsvn create_svnignore nonsvnlist svn2dist svnaddcurrentdir svnbackport 
  svn-clean svngettags svnlastchange svnlastlog svnrevertlast svnversions 
  svnchangesince findmissingcrystal kdesvn-build 
  kdedoc qtdoc extractrc extractattr zonetab2pot.py 
  licensecheck fixkdeincludes fixuifiles includemocs 
  cxxmetric extend_dmalloc kdekillall kdelnk2desktop.py 
  package_crystalsvg png2mng.pl kdemangen.pl svnintegrate DESTINATION bin)
 
