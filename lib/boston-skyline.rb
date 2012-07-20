class BostonSkylineFilter < Nanoc3::Filter
    identifier :boston_skyline
    type :binary

    def run(filename, params={})
	convert = "convert #{filename} -quality 100 jpg:#{output_filename}"
	crush = "jpegoptim -m80 #{output_filename}"

	system(convert)
	system(crush)
    end
end
