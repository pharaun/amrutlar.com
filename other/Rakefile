Directory=File.dirname(__FILE__) + "/*"

namespace :deploy do
    task :build => "rake:build:background"

    desc "Deploy to the localhost machine"
    task :localhost => :build do
	puts "Localhost deployment"
    end

    desc "Deploy to asram for local network testing"
    task :asram => :build do
	puts "asram deployment"
	cmd = "rsync -vrz #{Directory} asram:/var/www/localhost/htdocs"
	puts cmd
	system cmd
    end

    desc "Deploy to amrutlar.net for network deployment"
    task :amrutlar => :build do
	puts "amrutlar.net deployment"
	cmd = "rsync -vrz #{Directory} amrutlar.net:/var/www"
	puts cmd
	system cmd
    end
end

namespace :build do
    desc "Build the resume"
    task :resume do
	puts "Build Resume"
    end

    desc "Convert background and crush it into jpeg"
    task :background do
	puts "boston-skyline.png -> boston-skyline.jpg"
	cmd = "convert img/boston-skyline.png -quality 100 img/boston-skyline.jpg"
	puts cmd
	system cmd
	cmd = "jpegoptim -m80 img/boston-skyline.jpg"
	puts cmd
	system cmd
    end
end

namespace :validate do
    desc "Validates the site's CSS files"
    task :css do
	puts "Validates css"
    end
    
    desc "Validates the site's HTML files"
    task :css do
	puts "Validates html"
    end
    
    desc "Validates the site's Links"
    task :css do
	puts "Validates links"
    end
end

# Default deploy to amrutlar.net
task :default => "deploy:amrutlar"
