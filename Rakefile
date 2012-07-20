require 'nanoc3/tasks'
require 'uuid'

task :create_post do
    # Check path
    if ENV['title'].nil?
	$stderr.puts('You need to specify a title, e.g. rake create_article title="my new article"')
	break
    end

    title = ENV['title'] # the path

    # Create item
    site = Nanoc3::Site.new('.')
    time = Time.now
    year = time.year
    month = "%.2d" % time.month

    path = "/blog/#{year}/#{month}/" + title.downcase.gsub(/ /, "-").gsub(/[^a-z]/i, '_')

    site.data_sources[0].
	create_item(
		'Hello, I am a new article!', # the content
		{ # the attributes
	:title => ENV['title'],
	:created_at => time,
	:kind => "article",
	:uuid => UUID.generate
    },
	path.cleaned_identifier # the path
    )

    puts "Created post at #{path.cleaned_identifier}"
end
