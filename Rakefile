Directory=File.dirname(__FILE__) + "/*"

namespace :deploy do
    desc "Deploy to the localhost machine"
    task :localhost do
	puts "Localhost deployment"
    end

    desc "Deploy to asram for local network testing"
    task :asram do
	puts "asram deployment"
	cmd = "rsync -vrz #{Directory} asram:/var/www/localhost/htdocs"
	puts cmd
	system cmd
    end

    desc "Deploy to amrutlar.net for network deployment"
    task :amrutlar do
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
