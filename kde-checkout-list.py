#!/usr/bin/env python3
# SPDX-FileCopyrightText: Carl Schwan <carl@carlschwan.eu>
#
# SPDX-License-Identifier: GPL-2.0-or-later

import argparse
from pathlib import Path
import requests
import typing as tp
import argparse
import logging
import os
from plumbum import local

LOG = logging.getLogger(__name__)

def main() -> None:
    parser = argparse.ArgumentParser('kde-checkout-list')
    parser.add_argument('-c', '--component', action='append', help='Search for projects within this component only')
    parser.add_argument('-n', '--nocomponent', action='append', help='Do not search for projects within this component only')
    parser.add_argument('--clone', action="store_true", help='Actually do a git clone or pull of every repo found\nNote: this is meant for servers like lxr/ebn rather than for developers.')
    parser.add_argument('--prune', action="store_true", help='Remove old git checkouts that are not listed anymore')
    parser.add_argument('--dry-run', action="store_true", help='Show git and prune commands but do not execute them.')
    parser.add_argument('-a', '--all', action="store_true", help='Clone all the repos. By default only clone repos what are active.')
    parser.add_argument('-o', '--outputdir', help='Output dir for the clonned repos', default="repos")

    args = parser.parse_args()

    r = requests.get('https://projects.kde.org/api/v1/find' if args.all else 'https://projects.kde.org/api/v1/find?active=true')
    projects = r.json()
    if not args.all:
        filtered_projects = []
        for project in projects:
            # whitelist
            if args.component and not project.startswith(tuple(args.component)):
                continue

            # blacklist
            if args.nocomponent and project.startswith(tuple(args.nocomponent)):
                continue

            filtered_projects.append(project)
        projects = filtered_projects

    if args.prune:
        if os.path.isdir(args.outputdir):
            find = local['find']
            cmd = find[args.outputdir, '-name', '.git']
            for repo in cmd().splitlines():
                repo = repo[:-5].replace(args.outputdir + '/', '')
                if repo not in projects:
                    rm = local['rm']
                    cmd = rm['-rf', args.outputdir + '/' + repo]
                    print(cmd)
                    if not args.dry_run:
                        cmd()

    if args.clone:
        mkdir = local['mkdir']
        mkdir['-p', args.outputdir]()

        git = local['git']
        for project in projects:
            if not os.path.isdir(args.outputdir + '/' + project):
                cmd = git['clone', 'https://invent.kde.org/' + project, args.outputdir + '/' + project]
                print(cmd)
                if not args.dry_run:
                    cmd()
            else:
                with local.cwd(args.outputdir + '/' + project):
                    cmd = git['pull']
                    print(cmd, 'in', project)
                    if not args.dry_run:
                        cmd()


if __name__ == '__main__':
    main()

