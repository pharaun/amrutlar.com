# All files in the 'lib' directory will be loaded
# before nanoc starts compiling.
#
# Stolen from Dominikh's default.rb for his site

include Nanoc3::Helpers::Blogging
include Nanoc3::Helpers::LinkTo
include Nanoc3::Helpers::Text

require 'nokogiri'

# This is just some syntactical sugar that we use later
# Don't worry about it for now.
class Nanoc3::Item
    def content(opts = {})
	opts[:rep] ||= :default
	opts[:snapshot] ||= :last
	reps.find { |r| r.name == opts[:rep] }.content_at_snapshot(opts[:snapshot])
    end

    def name
	identifier.split("/").last
    end
end

def get_paragraphs(html, num = 1)
    doc = Nokogiri::HTML.fragment(html)
    doc.css('p')[0, num].to_xhtml
end

LICENSES = {
    :simplified_bsd => {
	:name => "Simplified BSD",
	:link => "http://www.opensource.org/licenses/bsd-license.php",
    },

    :gfdl => {
	:name => "GNU Free Documentation License",
	:link => "http://www.gnu.org/copyleft/fdl.html",
    },

    :gplv2 => {
	:name => "GPLv2",
	:link => "http://www.gnu.org/licenses/gpl-2.0.html",
    },

    :lgplv2 => {
	:name => "LGPLv2.1",
	:link => "http://www.gnu.org/licenses/lgpl-2.1.html",
    },
}
def link_to_license(license)
    license = LICENSES[license]
    link_to(license[:name], license[:link])
end

def annotate_license(link, comment)
  "#{link} (#{comment})"
end

def listify_licenses(licenses)
    lines = []
    licenses.each do |license|
	note = nil
	license, note = license

	link = link_to_license(license)
	if note
	    link = annotate_license(link, note)
	end
	line = "<dd>#{link}</dd>"
	lines << line
    end
    lines.join("\n")
end
