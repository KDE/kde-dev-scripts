#!/usr/bin/ruby

#############################################################################
#    Copyright (C) 2005, 2006 by Tobias Hunger
#    tobias.hunger@basyskom.de
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the
#    Free Software Foundation, Inc.,
#    51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#############################################################################

# This script is supposed to build a skeleton cmake configuration file for
# the sources and header files found in the current directory.
#
# It is insprired by "qmake -project" and works with the same limitations.

require 'fileutils'

class Project
    def initialize()
        @pwd = FileUtils.pwd();
        @projectName = File.basename(@pwd)
        @sources = []
        @headers = []
        @uis = []
        @kcfgs = []
        @configs = []

        @got_main = false
        @got_cmakelist = false
        @got_qt = false
        @got_kde = false
    end

    def addHeader(header)
        @headers.push(header) unless @headers.include?(header)
    end

    def haveHeaders?
        ! @headers.empty?
    end

    def addSource(source)
        @sources.push(source) unless @sources.include?(source)
    end

    def haveSources?
        ! @sources.empty?
    end

    def addUi(ui)
        @got_qt = true
        @uis.push(ui) unless @uis.include?(ui)
    end

    def haveUis?
        ! @uis.empty?
    end

    def addKcfg(kcfg)
        @got_kde = true
        @kcfgs.push(kcfg) unless @kcfgs.include?(kcfg)
    end

    def haveKcfgs?
        ! @kcfgs.empty?
    end

    def addConfig(config)
        @configs.push(config) unless @configs.include?(config)
    end

    def haveConfigs?
        ! @configs.empty?
    end

    attr_reader :pwd, :projectName, :headers, :sources, :uis, :kcfgs, :configs,
                :got_main, :got_cmakelist, :got_qt, :got_kde
    attr_writer :pwd, :projectName, :got_main, :got_cmakelist, :got_qt, :got_kde
end

class Scanner
    def initialize(project)
        @project = project
    end

    def findFiles()
        Dir.entries(@project.pwd).each do |file|
            @project.got_cmakelist = true if /^CMakeLists.txt$/.match(File.basename(file))
            @project.addHeader(file) if /(.h|.hh|.H|.hpp|.hxx)$/.match(File.basename(file))
            @project.addSource(file) if /(.c|.cc|.C|.cpp|.cxx)$/.match(File.basename(file))
            @project.addUi(file) if /(.ui)$/.match(File.basename(file))
            @project.addKcfg(file) if /(.kcfgc)$/.match(File.basename(file))
            @project.addConfig(file) if /(.in)$/.match(File.basename(file))
        end
    end

    def scanFile(file)
        last_line = "";
        File.foreach(file) do |line|
            # empty lines:
            next if /^\s*$/.match(line)

            # scan for includes
            if /^\s*#\s*include\s*[<"](.*)[>"]\s*$/.match(line)
                @project.got_qt = true if /^Q(Debug|List|String|Hash|Application)$/.match($1)
                @project.got_kde = true if /^k(debug|kio\/|application)$/.match($1)
            end

            # do we have a main function?
            @project.got_main = true if /^(void|int|)\s*main\s*/.match(line)
        end
    end

    def scanFiles()
        @project.headers.each { |i| scanFile(i); }
        @project.sources.each { |i| scanFile(i); }
    end
end

class Generator
    def initialize(project)
        @project = project
        @install_location = ""
        @target_type = "executable"
        @target_type = "library" unless @project.got_main
        @prefix = "#{@project.projectName}_#{@target_type}"
    end

    def header()
        "# ########## Project setup ##########\n" +
        "PROJECT(#{@project.projectName})\n" +
        "CMAKE_MINIMUM_REQUIRED(VERSION 2.4.5)\n\n"
    end

    def setup()
        "# ######### General setup ##########\n" +
        "INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR})\n\n"
    end

    def files()
        output = "# ########## #{@project.projectName} #{@target_type} ##########\n" +
        "# Sources:\n" +
        "SET(#{@prefix}_SRCS\n"
        @project.sources.each { |i| output+= "    #{i}\n" }
        output += ")\n\n" +
        "# Headers:\n" +
        "SET(#{@prefix}_HDRS\n"
        @project.headers.each { |i| output += "    #{i}\n" }
        output += ")\n\n"
        output
    end

    def specials()
        ""
    end

    def configfiles()
        output = ""
        @project.configs.each do |i|
            output += "CONFIGURE_FILE(#{i} ${CMAKE_BINARY_DIR}/#{i[0..-4]})\n"
        end
        output += "\n" unless output == ""
        return output
    end

    def target()
        output = "# actual target:\n"
        if @project.got_main
            @install_location = "bin"
            output += "ADD_EXECUTABLE(#{@project.projectName} ${#{@prefix}_SRCS})\n"
        else
            @install_location = "lib"
            output += "ADD_LIBRARY(#{@project.projectName} ${#{@prefix}}_SCRS ${#{@prefix}_HDRS})\n" +
            "SET_TARGET_PROPERTIES(#{@project.projectName} PROPERTIES VERSION 0.0.0)\n"
        end
        return output
    end

    def libraries()
        ""
    end

    def install()
        "# add install target:\n" +
        "INSTALL(TARGETS #{@project.projectName} DESTINATION #{@install_location})"
    end

    def generate()
        File.open("CMakeLists.txt", "w") do |file|
            file.puts header
            file.puts setup

            file.puts files
            file.puts configfiles
            file.puts specials

            file.puts target
            file.puts libraries
            file.puts install
        end
    end
end


class QtGenerator < Generator
    def initialize(project)
        super(project)
    end

    def setup
        "# ########## Qt4 setup ##########\n" +
        "FIND_PACKAGE(Qt4 REQUIRED)\n" +
        "INCLUDE(${QT_USE_FILE})\n" +
        "INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR} ${QT_INCLUDES})\n\n"
    end

    def files()
        output = super
        if @project.haveUis?
            output += "# UI files:\n" +
            "SET(#{@prefix}_UIS\n"
            @project.uis.each { |i| output += "    #{i}\n" }
            output += ")\n\n"
        end
        output
    end

    def specials()
        "# scan files and run moc on all that need it:\n" +
        "QT4_AUTOMOC(${#{@prefix}_SRCS})\n\n"
    end

    def libraries()
        "TARGET_LINK_LIBRARIES(#{@project.projectName} ${QT_LIBRARIES})\n\n"
    end
end

class KDEGenerator < QtGenerator
    def initialize(project)
        super(project)
    end

    def setup
        "# ########## KDE4 setup ##########\n" +
        "FIND_PACKAGE(KDE4 REQUIRED)\n" +
        "INCLUDE(KDE4Defaults)\n" +
        "INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR} ${KDE4_INCLUDE_DIR} ${QT_INCLUDES})\n\n"
    end

    def files()
        output = super()
        if @project.haveKcfgs?
            output += "# KCFG files:\n" +
            "SET(#{@prefix}_KCFGS\n"
            @project.kcfgs.each { |i| output += "    #{i}\n" }
            output += ")\n\n"
        end
        output
    end

    def specials()
        output = "# Special code for KDE4:\n"
        output += "# " unless @project.haveUis?
        output += "KDE4_ADD_UI_FILES(${#{@prefix}_SRCS} ${#{@prefix}_UIS})\n"
        output += "# " unless @project.haveKcfgs?
        output += "KDE4_ADD_KCFG_FILES(${#{@prefix}_SRCS} ${#{@prefix}_KCFGS})\n\n"
    end

    def target()
        "# actual target:\n" +
        "KDE4_ADD_EXECUTABLE(#{@project.projectName} ${#{@prefix}_SRCS} ${#{@qprefix}_HDRS})\n"
    end

    def libraries()
        "TARGET_LINK_LIBRARIES(#{@project.projectName} ${KDE4_KDEUI_LIBS})\n\n"
    end

    def install()
        "# add install target:\n" +
        "INSTALL(TARGETS #{@project.projectName} DESTINATION ${BIN_INSTALL_DIR})"
    end

end


my_project = Project.new
my_scanner = Scanner.new(my_project)

my_scanner.findFiles()

my_scanner.scanFiles()

if my_project.got_kde
    my_generator = KDEGenerator.new(my_project)
elsif my_project.got_qt
    my_generator = QtGenerator.new(my_project)
else
    my_generator = Generator.new(my_project)
end
my_generator.generate
