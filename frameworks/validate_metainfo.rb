#! /usr/bin/env ruby

require 'yaml'

if ARGV.length < 1
    puts "We need at least one argument"
    puts "Usage: ruby metainfo.yaml\n"
    exit
end

kdecommon = ENV["KDE_COMMON_SRC"]

def kdewho(kdecommon, user)
    if kdecommon.nil? then
        return true
    end

    userFound = false

    f = File.open("#{kdecommon}/accounts", "r")
    f.each_line do |line|
      if line.match(/^#{user} /) then
          userFound = true
          break
      end
    end
    f.close

    return userFound
end


rules = {
    "description" => nil,
    "tier" => [1, 2, 3, 4],
    "type" => ["functional", "integration", "solution"],
    "portingAid" => [true, false],
    "deprecated" => [true, false],
    "release" => [true, false],

    "platforms" => lambda { |file, platforms|
        if not platforms.is_a? Array then
            return false
        end

        platforms.each do |platform|
            if not platform.is_a? Hash then
                return false
            end

            if not ["All", "MacOSX", "Windows", "Linux"].include? platform["name"] then
                return false
            end
        end

        return true
    },

    "maintainer" => lambda { |file, maintainer|
        if maintainer.nil? then
            return true
        elsif not maintainer.is_a? String then
            return false
        else
            return kdewho(kdecommon, maintainer)
        end
    }
}

errorFound = false

ARGV.each do |file|
    yaml = YAML.load_file(file)

    rules.keys.each do |property|
        if not yaml.has_key? property then
            puts "#{file} is missing #{property}"
            errorFound = true
            next
        end

        rule = rules[property]
        if ((rule.is_a? Array and not rule.include? yaml[property]) or (rule.is_a? Proc and not rule.call file, yaml[property])) then
            puts "#{file} has invalid #{property}: #{yaml[property]}"
            errorFound = true
        end
    end
end

if kdecommon.nil? then
    if errorFound then
        puts
    end
    puts "WARNING: kde-common wasn't available, couldn't validate maintainer fields content"
    puts "Set the KDE_COMMON_SRC environment variable to point to your kde-common copy"
end

exit (errorFound ? 1 : 0)
