#! /usr/bin/env ruby

require 'optparse'
require 'yaml'

options = {
    :includePorting => false,
    :includeDead => false,
    :includeFlags => true
}
OptionParser.new do |opts|
    opts.banner = "Usage: #{File.basename($0)} [options]"

    opts.on("-p", "--[no-]porting", "Include porting aids") do |p|
        options[:includePorting] = p
    end

    opts.on("-d", "--[no-]dead", "Include dead frameworks (no more released)") do |d|
        options[:includeDead] = d
    end

    opts.on("-f", "--[no-]flags", "Include descriptive flags in the body (active by default)") do |f|
        options[:includeFlags] = f
    end
end.parse!

kdecommon = ENV["KDE_COMMON_SRC"]

if kdecommon.nil? then
    puts "ERROR: kde-common must be available"
    puts "Set the KDE_COMMON_SRC environment variable to point to your kde-common copy"
    exit 1
end

if not File.directory?("#{ENV["PWD"]}/frameworks") then
    puts "ERROR: this script must be executed from the parent directory of your frameworks/ prefix"
    exit 1
end

def kdewho(kdecommon, user)
    if kdecommon.nil? then
        return true
    end

    if user.nil? then
        return { :name => "No maintainer", :email => "kde-frameworks-devel@kde.org" }
    end

    userFound = false

    f = File.open("#{kdecommon}/accounts", "r")
    f.each_line do |line|
        match = line.match(/^#{user}\s+(.*)\s+(\S+)\s*$/)
        if match then
            return { :name => match[1].squeeze(" ").chomp(" "), :email => match[2] }
        end
    end
    f.close

    return nil
end

frameworksMap = {}

Dir.glob("frameworks/*/metainfo.yaml").each do |file|
    framework = file.match(/frameworks\/(.*?)\/metainfo.yaml/)[1]
    yaml = YAML.load_file(file)

    if not yaml.has_key? "maintainer" then
        puts "#{file} has no maintainer property!!"
        exit 1
    end

    if yaml["portingAid"] and not options[:includePorting] then
        next
    elsif yaml["deprecated"] and not yaml["release"] and not options[:includeDead] then
        next
    end

    maintainer = yaml["maintainer"]

    description = framework
    if options[:includeFlags] then
        flags = []
        if yaml["portingAid"] then
            flags << "porting aid"
        elsif not yaml["release"] then
            if yaml["deprecated"] then
                flags << "no more released"
            else
                flags << "upcoming"
            end
        elsif yaml["deprecated"] then
            flags << "deprecated"
        end

        if not flags.empty? then
            description = description + " (" + flags.join(", ") + ")"
        end
    end

    if not frameworksMap.has_key? maintainer then
        frameworksMap[maintainer] = [description]
    else
        frameworksMap[maintainer] << description
    end
end

tempFile = "/tmp/mail_framework_frameworksMap_body.txt"
body = File.open(tempFile, "w")
args = "--msg #{tempFile}"

contacts = {}
frameworksMap.keys.each do |maintainer|
    contacts[maintainer] = kdewho(kdecommon, maintainer)
end

maintainers = frameworksMap.keys
if maintainers.include? nil then
    maintainers.delete nil
    maintainers.sort! { |a, b| contacts[a][:name] <=> contacts[b][:name] }
    maintainers << nil
else
    maintainers.sort! { |a, b| contacts[a][:name] <=> contacts[b][:name] }
end

maintainers.each do |maintainer|
    contact = contacts[maintainer]
    if maintainer.nil? then
        args = args + " \"#{contact[:email]}\""
    else
        args = args + " \"#{contact[:name]} <#{contact[:email]}>\""
    end

    body.write("#{contact[:name]}:\n")
    frameworksMap[maintainer].sort.each do |framework|
        body.write(" - #{framework}\n")
    end
    body.write("\n")
end
body.close

system "kmail #{args}"
File.delete tempFile
