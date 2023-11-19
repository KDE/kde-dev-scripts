cmake_minimum_required(VERSION 3.0)
project(kde-dev-scripts)

set(REQUIRED_QT_VERSION "5.15.2")

find_package(ECM ${KF_MIN_VERSION} REQUIRED NO_MODULE)
set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH})

include(FeatureSummary)
include(KDEInstallDirs)

if (QT_MAJOR_VERSION STREQUAL "6")
    set(KF_MIN_VERSION "5.240.0")
    set(KF_MAJOR_VERSION "6")
else()
    set(KF_MAJOR_VERSION "5")
endif()


find_package(KF${KF_MAJOR_VERSION} COMPONENTS DocTools)

# Note that all of these files should be *scripts* that do not require
# compilation, therefore we simply install them. We split out Bourne shell
# programs explicitly but it's not guaranteed that the Perl/Python/Ruby/etc.
# ones behave well on Mac or Windows just because we don't elide those too.

########### install files ###############
if(UNIX)
#shell program

if(TARGET KF${KF_MAJOR_VERSION}::DocTools)
  add_subdirectory(doc)
  kdoctools_install(po)
endif()

install(PROGRAMS
  adddebug
  build-progress.sh
  cheatmake
  c++-rename-class-and-file
  c++-copy-class-and-file
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
  svnbackport
  svnforwardport
  svnchangesince
  svngettags
  svnlastlog
  svnrevertlast
  uncrustify-kf5
  wcgrep
  draw_lib_dependencies
  DESTINATION ${KDE_INSTALL_BINDIR})

install(FILES
  uncrustify-kf5.cfg
  uncrustify-qt.cfg
  DESTINATION ${KDE_INSTALL_DATADIR}/uncrustify)

endif()

install(PROGRAMS
  addmocincludes
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
  reviewboard-am
  grantlee_strings_extractor.py
  clean-forward-declaration.sh
  clean-includes.sh
  port_new_gitlab_ci_template.sh
  DESTINATION ${KDE_INSTALL_BINDIR})

# SKIP_PO_INSTALL

feature_summary(WHAT ALL FATAL_ON_MISSING_REQUIRED_PACKAGES)
