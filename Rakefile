require 'nanoc3/tasks'
require 'uuid'

# Create a new blog post automatically
task :create_post do
    # Check path
    if ENV['title'].nil?
	$stderr.puts('You need to specify a title - rake create_post title="project name"')
	break
    end

    title = ENV['title'] # the path

    # Create item
    site = Nanoc3::Site.new('.')
    time = Time.now
    year = time.year
    month = "%.2d" % time.month

    path = "/blog/#{year}/#{month}/" + title.downcase.gsub(/ /, "-") \
						    .gsub(/[^\w]/i, '_') \
						    .gsub(/_{2,}/, '_') \
						    .gsub(/(^[_-]+)|([_-]+$)/, '')

    site.data_sources[0].
	create_item(
	    'Hello, this is a new blog post', # the content
	    { # the attributes
		:title => ENV['title'],
		:created_at => time,
		:menu => "blog",
		:kind => "article",
		:categories => nil,
		:uuid => UUID.generate,
		:description => "New blog post!"
	    },
	    path.cleaned_identifier, # the path
	    { :extension => '.markdown' }
	)

    puts "Created post at #{path.cleaned_identifier}"
end

# Create a new project page
task :create_project do
    # Check path
    if ENV['title'].nil?
	$stderr.puts('You need to specify a title - rake create_project title="project name"')
	break
    end

    title = ENV['title'] # the path

    # Create item
    site = Nanoc3::Site.new('.')

    path = "/projects/" + title.downcase.gsub(/ /, "-") \
					.gsub(/[^\w]/i, '_') \
					.gsub(/_{2,}/, '_') \
					.gsub(/(^[_-]+)|([_-]+$)/, '')

    site.data_sources[0].
	create_item(
	    'Hello, this is a new project page', # the content
	    { # the attributes
		:title => ENV['title'],
		:menu => "projects",
		:kind => "projects",
		:sources => nil,
		:documentation => nil,
		:licenses => nil,
	    },
	    path.cleaned_identifier, # the path
	    { :extension => '.markdown' }
	)

    puts "Created project at #{path.cleaned_identifier}"
end
