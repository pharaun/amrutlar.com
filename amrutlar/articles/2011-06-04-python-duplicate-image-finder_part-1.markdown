---
title: Python Duplicate Image Finder (Part 1)
menu: blog
tags: C, OpenMP, SSE, Projects
---
Welcome to part one of what probably will be a long series of blog posts on the
topic of my [Python Duplicate Image Finder](/projects/python-duplicate-image-finder/)
project. This post will go over the basic image similarity algorithm that is currently
implemented and the various challenges in getting the n-way compare using the basic
algorithm to be fast enough to be usable.


## Basic Algorithm

For this post we'll go over the basic _sum(abs(a - b)) > threshold_ algorithm.
Basically the program goes through these steps:

1. Find all image files as determined by _imghdr_ and enroll it into a list.
2. Crunch each image into a 32x32 array of RGB values (3072 elements total),
   and add them to a list along with the file path. This is sort of an image
   signature.
3. Run a n-way compare on the list of image signature using the above equation
   _sum(abs(a - b))_ and if the result was greater than a certain threshold,
   which currently is 98% then its deemed to be similar enough and its added
   to a list of similar images.

This algorithm works decently well on certain image types such as pictures and
etc, but it does not work very good on images that are mainly one or a few
colors such as line drawings or animated images... So an obvious improvement
would be to somehow bucket/discard these types of images before doing the n-way
compare to save us time. This will hopefully be a future feature...


## A Problem with the Algorithm

The problem is with step 3, which is the n-way compare, this is an _O(n^2)_
operation hence the number of compares needed for a reasonably large image
collection of about 64,000 images for example would be roughly ~4 billion
compares. So clearly we need to find a way to trim that down a little.

Its possible to trim it down by realizing that the ~4 billion compare would have
a ton of re-compares which just wastes time. So now think about it like this, let's
say you have an array of 10 elements that needs to be compared to each others:

1. First element needs to be compared to 9 other elements.
2. Second element needs to be compared to 8 other elements because it already
   was compared with the first element in step #1.
3. Third element needs to be compared to 7 other elements...
4. ...
5. Second to last element only needs to compare with the last element.
6. Last element does not need to be compared with anything else.

Hence its possible to short-circuit a good part of the unneeded compares as you
can see above. This leads to a big O notation of _O(n*(n-1)/2)_ which for
64,000 images would end up being roughly ~2 billion compares which is a nice
improvement. However in the end its still an _O(n^2)_ algorithm so it'll still
end up being nasty in the end.


## Basic speed improvements

Basically since its an _O(n^2)_ algorithm we want the inner loops to be as fast
as possible so that it can burn through as many compares as possible. Since
this problem is for most part quite parallelable, it made sense to try to
distribute the workload cross multiple cores.

Hence, the first version used the
[multiprocessing](http://docs.python.org/library/multiprocessing.html) module
in Python for a quick and easy way to get around the GIL in Python. It was
fairly quick to get a basic version up and running with _Pool_ in the
multiprocessing module.  However due to all of the copying of the data from the
image signature the RAM usage would explode (20+ GB) to silly levels.

So then the next version then stopped copying the image signature data around
and instead attempted to take advantage of a shared read only list, but it
proved to be quite tough because the image signatures were being held in
[Numpy](http://numpy.scipy.org/) _ndarray_. I attempted a few different
approaches and while some of them panned out, in the end it proved to just be
way too slow.

So the next step then was to try various approaches in
[Cython](http://cython.org/) along with also using Numpy again here. And in the
end after a bunch of tweaking I was able to get a few percentage (5-10%)
improvement over Python. However it was still hampered by the lack of
[OpenMP](http://openmp.org/wp/) support which forced me to use the
multiprocessing module to distribute the work over multiple cores to get around
the Python GIL issue.

## More Speed improvements with C

With all of the speed improvements that I was able to squeeze out of Python and
Cython, it was still not quite fast enough so it was time to start considering
migrating over to a C based extension. One of the primary driving reason for
the migration was the OpenMP extension because this would enable me to still
have fairly easy parallelability without dealing with threading directly.

The first implementation was done in vanilla C along with OpenMP and it was
able to do roughly ~1.1 million compares a second which presented a nice
improvement over the Cython version. I sadly don't have much timing information
from the Cython or the plain Python version of the code. So you'll just have to
take my words for it here that it was a nice initial improvement in speed.
Right below is the code snippet that was used in the initial version.

    // Each array == "3072 entry long - 3 * 32 * 32"
    #define ARRAY_LENGTH 3072
    // Similarity div == 255.0 * 1024.0 * 3.0 - Max value * element in array * rgb (3)
    #define SIMILARITY_DIV 783360.0
    #define SIMILARITY_THRESHOLD 0.98

    // The array of pointers to each of the numpy ndarray
    const double** array;

    #pragma omp parallel for shared(dup, array, path) private(j, i, k)
	for(i = 0; i < length; i++) {
	    for(j = (i + 1); j < length; j++) {
		const double* sima = array[i];
		const double* simb = array[j];

		double sum = 0.0;

		for(k = 0; k < ARRAY_LENGTH; k++) {
		    sum += fabs(sima[k] - simb[k]);
		}

		double fp = (1.0 - (sum / SIMILARITY_DIV));

		if(fp >= SIMILARITY_THRESHOLD) {
		    #pragma omp critical
		    {
			dup += 1;
			append_to_list(fp, i, j);
		    }
		}
	    }
	}
{:.c}

Then to see if I could squeeze out a bit more improvement, I went ahead and
implemented the loop into SSE using the SSE intrinsics. And thanks to GCC and
the fact that this machine is running _amd64_ I didn't need to deal with any
aligning at this point in time. However I will eventually need to deal with it.
But anyway it was written using doubles not floats thanks to Numpy storing the
results of the image signature as doubles. Now there was hardly any
improvements here, it just improved by 9% to ~1.2 million compares a second.
Below is the code snippet for the SSE intrinsics version of the inner loop, as
you can see using SSE intrinsics can decrease readability of the code.

    // Init partial sums
    __m128d vsum = _mm_set1_pd(0.0);

    for(k = 0; k < ARRAY_LENGTH; k += 2) {
	    // Load 2 doubles from sima, simb
	    __m128d va = _mm_load_pd(&sima[k]);
	    __m128d vb = _mm_load_pd(&simb[k]);

	    // Calc diff = sima - simb
	    __m128d vdiff = _mm_sub_pd(va, vb);

	    // calc neg diff = 0.0 - diff
	    __m128d vnegdiff = _mm_sub_pd(_mm_set1_pd(0.0), vdiff);

	    // calc abs diff = max(diff, - diff)
	    __m128d vabsdiff = _mm_max_pd(vdiff, vnegdiff);

	    // accumulate two partial sums
	    vsum = _mm_add_pd(vsum, vabsdiff);
    }

    // Accumulate the partial sums into one
    vsum = _mm_hadd_pd(vsum, _mm_set1_pd(0.0));

    // calc vsum = vsum / vdiv
    vsum = _mm_div_sd(vsum, _mm_set1_pd(SIMILARITY_DIV));

    // calc vsum = 1.0 - vsum
    vsum = _mm_sub_pd(_mm_set1_pd(1.0), vsum);

    // Unload vsum -> fp
    double fp[2];
    _mm_store_sd(&fp[0], vsum);
{:.c}

There should have been a bit more improvement because I was able to double pump
two doubles calculation at the same time via SSE... This fact seemed to
indicate that perhaps the computation was being memory bound or there was other
places that I could improve it such as seeing if I even need the extra
precision presented in the doubles.

However, while running the tests I noticed that the OpenMP scheduler did not
seem to be working as well as I would had liked it so the next easy fix would
be to spend some time tweaking the OpenMP scheduler. And thanks to some tweaks
I was able to get a quite nice boost in speed of about 106% which was basically
doubling the performance here with the vanilla C over the previous SSE version
at ~2.48 million compares a second. Then with the SSE intrinsics, it improved
by 17% up to ~2.9 million compares a second.

Since the OpenMP code on the default scheduler would break up the arrays into static
chunks to be distributed to each core for computation, this would lead to an uneven
workload because the amount of work overtime would go down so hence the cores that
were assigned the latter chunks would end up finishing quicker and then sitting there
idle. So to remedy this I instructed OpenMP to use the dynamic scheduler which does
have an overhead but in this case it was a big win.

    #pragma omp parallel for shared(dup, array, path) private(j, i, k) schedule (dynamic)
{:.c}

Now for my final optimization to date on this project is on two items. Firstly
I was able to verify that I didn't need the extra precision granted with using
doubles. And secondly the Numpy arrays were scattered all over memory so it
made sense to gather it all up into a large array and use indexing pointers to
jump directly to the needed data.

    // The array of pointers to each of the numpy ndarray
    const double** array;

    // Array of pointers to each chunk of image similarity data in the farray
    const float** findex;

    // Single array containing all of the image data in a single block of memory
    float* farray;

    // Start the conversion here
    int i, k;

    for(i = 0; i < length; i++) {
        const double* sim = array[i];

        // File away this pointer as a index into the index array
        findex[i] = &farray[(i * ARRAY_LENGTH)];

        // Populate the farray with values
        for(k = 0; k < ARRAY_LENGTH; k++) {
            farray[((i * ARRAY_LENGTH) + k)] = (float)sim[k];
        }
    }
{:.c}

The code above is what was used to gather up all of the doubles, convert to
float, and store it in an single large chunk instead of all over memory. Then
also not shown here the SSE intrinsics were also changed slightly for most part
to use the float SSE instructions instead of the doubles, along with quad
pumping four float calculation at the same time.

With both optimization in place, the C version improved by 21% over the
previous version SSE enabled code at ~3.54 million compares a second. Which was
a nice improvement, but not quite as fast as I was hoping. However the big whooper
came a few moment latter with the float SSE enabled version, it improved by a quite
nice amount of 74% for a final result of ~6.19 million compares a second.


## Conclusion

The final result of ~6.19 million compares a second is an improvement of 424%
over the initial C version of ~1.18 million compares a second. This table below
provides an overview of the performance improvements for various versions. Each
row indicate its performance improvement in addition to the previous row's.

||C Version|SSE Version
|:-|:-|:-
|OpenMP|1.18 Million|1.23 Million
|Scheduling|2.48 Million|2.92 Million
|Floats|3.54 Million|6.19 Million

Overall it is a quite nice improvement and it was able to get the comparing
step at roughly ~3 hours down to ~400 seconds for 64,000 images.  However one
thing to keep in mind is ultimately the algorithm is _O(n^2)_ which will screw
us in the end hence I will be still working on figuring out how to bucket/group
images into similar buckets to keep the total N count low enough for it to
remain fast enough with these optimizations documented here. Also there still
are some other tweaks that I can probably do to make my SSE intrinsics ever
slightly bit better.


### Resources

python-duplicate-finder
: <https://github.com/pharaun/python-duplicate-finder>

How to absolute 2 double or 4 floats using SSE instruction set? (Up to SSE4)
: <http://stackoverflow.com/questions/5508628/how-to-absolute-2-double-or-4-floats-using-sse-instruction-set-up-to-sse4>
