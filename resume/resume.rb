#!/usr/bin/env ruby
# Simple resume yaml + erb templating -> pdf/latex/etc...
#
# Copyright (c) 2010, Anja Berens
#
# You can redistribute it and/or modify it under the terms of
# the GPL's licence.
#

require 'optparse'
require 'yaml'
require 'erb'
require 'pp'

# Options
options = {}

# Option parser
optparse = OptionParser.new do |opts|
    # Banner
    opts.banner = "Usage: resume.rb [options] <resume yaml> <erb template> <output>"

    # Define options
        options[:verbose] = false
    opts.on( '-v', '--verbose', 'Output more details' ) do
        options[:verbose] = true
    end

    # Help screen
    opts.on( '-h', '--help', 'Display this screen' ) do
        puts opts
        exit
    end
end

# Parse the command-line.
optparse.parse!

# Parse the file list and check them
if !(ARGV.length == 3)
    puts optparse.help
    exit
else
    options[:resume], options[:template], options[:output] = ARGV
end

# Not dealing with verboseness stuff yet
#p options[:verbose]

# Load up the YAML file
RESUME = YAML::load(File.open(options[:resume]))

# Load up the ERB template file
TEMPLATE = ERB.new File.new(options[:template]).read, nil, "%"

# Do some processing here
#p TEMPLATE.result
File.open(options[:output], "w") do |f|
    f.puts TEMPLATE.result
end
