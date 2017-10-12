import sys
import re
import os
import subprocess
import argparse

import portage.dep as portage_dep

template = """import info


class subinfo(info.infoclass):
    def setTargets(self):
        self.versionInfo.setDefaultValues()

        self.description = "%(appname)s"

    def setDependencies(self):
        self.runtimeDependencies["virtual/base"] = "default"
%(qtdeps)s
%(frameworksdeps)s
%(kdeappsdeps)s
%(otherdeps)s

from Package.CMakePackageBase import *


class Package(CMakePackageBase):
    def __init__(self):
        CMakePackageBase.__init__(self)
"""

def process(app, appname, portage, craft, indent):
    print("%sProcessing %s" % (indent, app))
    ebuild = "%s-17.08.1.ebuild" % app
    qtdeps = []
    frameworksdeps = []
    kdeappsdeps = []
    otherdeps = []
    qtre = re.compile("\$\(add_qt_dep ([^)]+)\)")
    frameworksre = re.compile("\$\(add_frameworks_dep ([^)]+)\)")
    kdeappsre = re.compile("\$\(add_kdeapps_dep ([^)]+)\)")
    optionalre = re.compile("^[^\?]+\?")

    with open(os.path.join(portage, app, ebuild), 'r') as ebuildfile:
        allfile = ebuildfile.read()
        dependencies = re.search("DEPEND=\"[^\"]*\"", allfile)

        if dependencies:
            deplines = dependencies.group(0).split("\n")

            del deplines[0] # The first one is always spurious
            del deplines[-1] # The last one is always spurious

            for d in deplines:
                depline = d.strip()
                qtmatch = qtre.match(depline)
                frameworksmatch = frameworksre.match(depline)
                kdeappsmatch = kdeappsre.match(depline)

                if qtmatch:
                    qtdeps.append(qtmatch.group(1))
                elif frameworksmatch:
                    frameworksdeps.append(frameworksmatch.group(1))
                elif kdeappsmatch:
                    appname = kdeappsmatch.group(1)
                    
                    with subprocess.Popen(["find", os.path.join(craft, "kde", "applications"), "-name", appname], stdout=subprocess.PIPE) as find:
                        craftdep = find.stdout.read().decode("utf-8").strip()

                    if len(craftdep) == 0:
                        if not process(appname, appname, portage, craft, "%s\t" % indent):
                            print("%sCould not add application %s, skipping" % (indent, appname))

                            return False

                    kdeappsdeps.append(appname)
                elif optionalre.match(depline):
                    print("%sOptional dep %s" % (indent, depline))
                else:
                    if portage_dep.isvalidatom(depline):
                        packagename = portage_dep.dep_getkey(depline).split("/")[1]

                        # TODO be smart about these types of mappings
                        if packagename == "eigen":
                            packagename = "eigen3"

                        with subprocess.Popen(["find", craft, "-name", packagename], stdout=subprocess.PIPE) as find:
                            craftdep = find.stdout.read().decode("utf-8").strip()

                            if len(craftdep) > 0:
                                otherdeps.append(craftdep[len(craft):])
                            else:
                                print("%sDependency %s not found, skipping" % (indent, packagename))
                                return False
                    else:
                        print("%sGarbage: %s" % (indent,depline))

    fixedframeworks = []

    for f in frameworksdeps:
        with subprocess.Popen(["find", craft, "-name", f], stdout=subprocess.PIPE) as find:
            fixedframeworks.append(find.stdout.read().decode("utf-8").strip()[len(craft):])

    qtdepsstr = "\n".join(["        self.runtimeDependencies[\"libs/qt5/%s\"] = \"default\"" % q for q in qtdeps])
    frameworksdepsstr = "\n".join(["        self.runtimeDependencies[\"%s\"] = \"default\"" % f for f in fixedframeworks])
    kdeappsdepsstr = "\n".join(["        self.runtimeDependencies[\"kde/applications/%s\"] = \"default\"" % k for k in kdeappsdeps])
    otherdepsstr = "\n".join(["        self.runtimeDependencies[\"%s\"] = \"default\"" % o for o in otherdeps])
    recipe = template % { "appname" : appname, "qtdeps" : qtdepsstr, "frameworksdeps" : frameworksdepsstr, "otherdeps" : otherdepsstr }
    outdir = os.path.join(craft, "kde", "applications", app)

    os.mkdir(outdir)

    with open(os.path.join(outdir, "%s.py" % app), 'w') as out:
        out.write(recipe)

    return True

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Translate from portage ebuilds to craft recipes")

    parser.add_argument("applist", help="List of applications to translate. Each line in this file is of the form <ebuild name> <application name>", type=argparse.FileType('r'))
    parser.add_argument("craft", help="Location of the craft root")
    parser.add_argument("--portage", help="Location of the portage ebuilds for KDE (defaults to /usr/portage/kde-apps)", default="/usr/portage/kde-apps")
    
    options = parser.parse_args()

    for l in options.applist:
        app, appname = tuple(l.strip().split(" "))
        craft_dir = os.path.join(options.craft, "kde/applications/%s" % app)
        portage_dir = os.path.join(options.portage, app)

        if os.path.exists(craft_dir):
            print("%s exists in craft, skipping" % app)
            continue

        if not os.path.exists(portage_dir):
            print("%s does not exist in portage, skipping" % app)
            continue

        process(app, appname, options.portage, options.craft, "")
