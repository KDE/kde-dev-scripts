#!/usr/bin/python
# -*- coding: utf-8 -*-

# Copyright (C) 2017 Sandro Knauß <sknauss@kde.org>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

"""Graph the dependencies of all pim packages.

The dependencies are minimized, so that only direct dependencies are shown.
The green dots are packages without depdencies inside the set.
The lightblue dots are one end of the build chain.

If you have something like this:

    a -> b -> c
    a -> c
the second depdency is not shown.

You can enable the full dependency if you replace sgraph -> graph in the last for loop.

The tier graph shows you a tier based view to pim packages.
One tier is defined as a maximum set of packages, that do not depend on each other and only
depend on lower tiers. Only dependecies from one tier to the next one are shown. Ellipsed shape
packages without arrows are indicating, that they could be built in a higher tier. Diamond shape
indicate that nothing depends on this anymore.

usage:
git clone kde:kde-build-metadata
<pathto>/pim-build-deps-graphs.py > pim-graphs.dot
dot -T png -o kde-build-metadata-17.12-deps.png  pim-graphs.dot > kde-build-metadata-17.12-tier.png
"""

from __future__ import print_function

import re
import copy

__fresh_id = 0

def main():
    def get_id():
        global __fresh_id
        __fresh_id += 1
        return ("NODE_%d" % __fresh_id)

    def emit_arc(node1, node2):
        print('  "%s" -> "%s" ;' % (node1, node2))
    def emit_node(node, dsc=None):
        if dsc is None:
              print('  "%s";' % (node))
        else:
              print('  "%s" [label="%s"];' % (node, dsc))
    def emit_nodecolor(node, color):
        print('  "%s" [fillcolor="%s", style="filled"] ;' % (node, color))


    line = re.compile(r"^([^#^:]+):\s*([^#]+)\s*(#.*)?$")
    graph = {}
    for i in open('kde-build-metadata/dependency-data-kf5-qt5').readlines():
         if not i.strip():
             continue
         m = line.match(i)
         if not m:
             continue
         pkg = m.group(1).strip()
         dep = m.group(2).strip()
         if not pkg.startswith("kde/pim/"):
             continue
         else:
             pkg = pkg[len("kde/pim/"):]

         if not pkg in graph:
             graph[pkg] = set()

         if not dep.startswith("kde/pim/"):
             continue
         else:
             dep = dep[len("kde/pim/"):]

         if not dep in graph:
             graph[dep] = set()

         graph[pkg].add(dep)

    sgraph = {}     # minimized graph
    fgraph = graph  # full dependency graph

    for i in range(10):
        changed = False
        ograph = fgraph
        for pkg in ograph:
            deps = copy.copy(ograph[pkg])
            for dep in ograph[pkg]:
                deps |= ograph[dep]
            if deps != ograph[pkg]:
                changed = True
            fgraph[pkg] = deps

        if not changed:
            break

    for pkg in fgraph:
        deps = copy.copy(graph[pkg])
        for dep in graph[pkg]:
            deps -= fgraph[dep]
        sgraph[pkg] = deps

    pkgs = set(graph.keys())     # packages to order into tiers
    tiers = []                   # each tier will be one entry
    deps = set()                 # All deps from lower tiers

    while pkgs:
        tD = set()
        if tiers:
            deps |= tiers[-1]
        tiers.append(set())
        for pkg in pkgs:
            if not (sgraph[pkg] - deps):
                tiers[-1].add(pkg)
                tD.add(pkg)
        pkgs -= tD

    ends = set()

    for pkg in graph:
        name = pkg
        sDeps = sgraph[pkg]
        if sDeps:
            for p in sgraph:
                if p == pkg:
                    continue
                if pkg in sgraph[p]:
                    break
            else:
                ends.add(name)

    print("digraph pim {")
    for pkg in graph:
        name = pkg
        sDeps = sgraph[pkg]
        if sDeps == set():
            emit_nodecolor(name, 'darkgreen')
        else:
            for p in sgraph:
                if p == pkg:
                    continue
                if pkg in sgraph[p]:
                    break
            else:
                emit_nodecolor(name, 'lightblue')

    for pkg in graph:
        name = pkg
        sDeps = sgraph[pkg]
        for dep in sDeps:
            emit_arc(dep, name)
    print("}")

    print("digraph pimTier {")
    print("    node [shape=diamond,fillcolor=lightblue,style=filled];")
    for pkg in ends:    # all end notes
        emit_node(pkg)
    print("    node [shape=ellipse,fillcolor=darkgreen];")
    for pkg in tiers[0]:    #   all dependency free packages - aka tier 0
        emit_node(pkg)
    print("    node [shape=ellipse,fillcolor=white];")
    for index, tier in enumerate(tiers):
        print("  subgraph cluster_{} {{".format(index))
        print("     style=filled;")
        print("     color=lightgrey;")
        print('     label = "Tier {}";'.format(index))
        for pkg in tier:
            emit_node(pkg)
        print("  }")
        if index > 0:
            subTier = tiers[index-1]
            for pkg in tier:
                for dep in (sgraph[pkg] & subTier):
                    emit_arc(dep, pkg)
    print("}")



if __name__ == '__main__':
    main()

