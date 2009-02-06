#!/usr/bin/env ruby
require "fileutils"

puts "Setting up basic structure"
dirs = %w(deps doc ebin include priv scripts src support)
files = %w(LICENSE Makefile README Rakefile)
dirs.each {|d| FileUtils.mkdir_p d unless ::File.directory? d}
files.each {|d| FileUtils.touch d unless ::File.file? d}

puts "Setting up the test directory structure"
test_dir = File.dirname(__FILE__) + "/../test"
test_dirs = %w(ebin include src)
test_dirs.each do |dir|
  full_dir = test_dir + "/#{dir}"
  FileUtils.mkdir_p full_dir unless ::File.directory? full_dir
end

cmd = "svn co http://svn.process-one.net/contribs/trunk/eunit #{test_dir}/include"
Kernel.system cmd