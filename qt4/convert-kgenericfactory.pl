#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# KGenericFactory + K_EXPORT_COMPONENT_FACTORY -> K_PLUGIN_FACTORY + K_EXPORT_PLUGIN

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

my $plugin;
my $factory;

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
        s!kgenericfactory\.h!kpluginfactory.h!g;
        s!<KGenericFactory>!<KPluginFactory>!g;
        if (/typedef KGenericFactory<(.*)>\s*(.*);/) {
            $plugin = $1;
            $factory = $2;
            # Ignore ",BaseClassForParentArg" in $plugin
            if ($plugin =~ s/,(.*)//) {
                print STDERR "Warning, parent argument for plugin was $1, needs to be changed to QObject, or you need a create function\n";
                print STDERR "  (see koffice/libs/main/KoDocInfoPropsFactory.cpp for an example)\n";
            }
            $_ = "K_PLUGIN_FACTORY($factory, registerPlugin<$plugin>();)\n";
        }
        if (/K_EXPORT_COMPONENT_FACTORY\(\s*(\w*),\s*(\w*)\(\s*([^\)]*)\s*\)\s*\)/) {
            my $libname_ignored = $1;
            my $factory2 = $2;
            my $catalogname = $3;
            die "Expected $factory, got $factory2" if ($factory ne $factory2);
            $_ = "K_EXPORT_PLUGIN($factory($catalogname))\n";
        }
        # All in one call, like K_EXPORT_COMPONENT_FACTORY(spreadsheetshape, KGenericFactory<TableShapePlugin>("TableShape"))
        if (/K_EXPORT_COMPONENT_FACTORY\s*\(\s*(\w*),\s*KGenericFactory\s*<(\w*)>\s*\(\s*(\"[^\"]*\")\s*\)\s*\)/) {
            my $libname_ignored = $1;
            my $plugin = $2;
            my $factory = $plugin . "Factory";
            my $catalogname = $3;
            $_ = "K_PLUGIN_FACTORY($factory, registerPlugin<$plugin>();)\n";
            $_ .= "K_EXPORT_PLUGIN($factory($catalogname))\n";
        }

        # Incremental fixing... can be removed later
        $plugin = $1 if (/registerPlugin<(.*)>/);
        $factory = $1 if (/K_PLUGIN_FACTORY\((\w*),/);
    } $file;
    # Now that we know the plugin name, fix its constructor signature, if by chance it's in the same file
    if (defined $plugin) {
        functionUtilkde::substInFile {
            s/QStringList/QVariantList/ if (/${plugin}::$plugin/);
        } $file;
        my $header = $file;
        $header =~ s/\.cpp$/.h/;
        $header =~ s/\.cc$/.h/;
        my $headerChanged = 0;
        functionUtilkde::substInFile {
            if (/${plugin}\(/) {
                if (s/QStringList/QVariantList/) {
                    $headerChanged = 1;
                }
            }
        } $header;
        if ($headerChanged) {
            functionUtilkde::addIncludeInFile( $header, "QVariantList");
        }
    }
}
functionUtilkde::diffFile( "@ARGV" );
