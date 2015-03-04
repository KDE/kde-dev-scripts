#!/usr/bin/python3

# Outputs dependencies information from a build directory
#
# Copyright (c) 2014 Aleix Pol Gonzalez <aleixpol@kde.org>
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# cmake . --trace |& grep ^/ | grep -v CMakeLists.txt | cut -d '(' -f 1 | sort -u

import subprocess
import os
import re
import json
import argparse
import sys

def readCache(varName):
    f = open("CMakeCache.txt", "r")
    for line in f:
        m = re.match('(.*?)=(.*)', line)
        if m is not None and m.group(1)==varName:
            return m.group(2)

def checkPackageVersion(frameworkName):
    value = readCache("FIND_PACKAGE_MESSAGE_DETAILS_%s:INTERNAL" % frameworkName)
    if value is None:
        return None
    m = re.match('.*\\]\\[v(.*?)\\((.*?)\\)\\]', value)
    if m:
        return { 'used': m.group(1), 'requested': m.group(2) }
    else:
        return None

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Figures out the dependencies of the build directory in the cwd.')

    try:
        projectDir = readCache("CMAKE_HOME_DIRECTORY:INTERNAL")
    except:
        print("Run in a build directory.")
        parser.print_help()
        sys.exit(1)

    proc = subprocess.Popen(['cmake', '.', '--trace'], stdout=open(os.devnull, "w"), stderr=subprocess.PIPE)
    processedFiles = {}
    lookedUpPackages = {}

    lastFile = ""
    callingFile = ""
    for line in proc.stderr:
        theLine = line.decode("utf-8")

        m = re.match('.*?:\s*find_package\((.*?) (.*?)\).*', theLine)
        if m is not None:
            if "$" not in m.group(2):
                lookedUpPackages[m.group(1)] = m.group(2)

        # match file names
        # e.g./usr/share/cmake-3.0/Modules/FindPackageMessage.cmake(46):  set(...
        m = re.match("(^/.*?)\\(.*", theLine)
        if m is not None:
            currentFile = m.group(1)
            if lastFile != currentFile:
                callingFile = lastFile
            lastFile = currentFile
            filePath, fileName = os.path.split(currentFile)

            if fileName == "CMakeLists.txt":
                continue

            m = re.match("(.*)Config(Version)?.cmake", fileName)
            m2 = re.match("Find(.*).cmake", fileName)
            if m2:
                moduleName = m2.group(1)
            elif m:
                moduleName = m.group(1)
            else:
                continue

            if not moduleName in processedFiles:
                processedFiles[moduleName] = { 'files': set(), 'explicit': False }

            if not 'version' in processedFiles[moduleName]:
                processedFiles[moduleName]['version'] = checkPackageVersion(moduleName)

            processedFiles[moduleName]['files'].add(currentFile)
            processedFiles[moduleName]['explicit'] |= (callingFile.endswith("CMakeLists.txt") or callingFile.endswith("Qt5/Qt5Config.cmake") or callingFile.endswith("FindKF5.cmake"))

    print("[")
    first = True
    for v, value in processedFiles.items():
        if not first:
            print(',\n', end='')
        first = False

        value['files'] = list(value['files'])
        value['project'] = v
        if v in lookedUpPackages:
            if value['version'] is None:
                line = lookedUpPackages[v]
                isVersion = line[:line.find(' ')]

                if len(isVersion)>0 and isVersion[0].isdigit():
                    value['version'] = { 'used': None, 'requested': isVersion }

            del lookedUpPackages[v]

        print("\t%s" % (json.dumps(value)), end='')

    # display missing packages
    for v in lookedUpPackages:
        if not first:
            print(',\n', end='')

        print("\t{ \"project\": \"%s\", \"missing\": true, \"files\": [], \"arguments\": \"%s\", \"explicit\": true }" % (v, lookedUpPackages[v]))


    print("\n]\n")
    proc.wait()
